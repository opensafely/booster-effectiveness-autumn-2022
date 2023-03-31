
# # # # # # # # # # # # # # # # # # # # #
# Purpose: 
#  - import matched data
#  - adds outcome variable and restricts follow-up
#  - Fit Cox models
#  - The script must be accompanied by three arguments:
#    `cohort` - pfizer or moderna
#    `type` - unadj, adj (Cox model adjustment)
#    `subgroup` - prior_covid_infection, vax12_type, cev, age65plus
#    `outcome` - the dependent variable
#    `variant_option` - ignore (ignore variant era), 
#                     - split (split fup according to variant era), 
#                     - restrict (restrict fup to each variant era)
#    `cuts` - the follow-up periods during which to estimate HRs

# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----


## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')


## import local functions and parameters ---

source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "survival.R"))


# import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)


if(length(args)==0){
  # use for interactive testing
  cohort <- "mrna"
  type <- "unadj"
  subgroup <- "vax3_type"
  variant_option <- "ignore" # ignore, split, restrict (delta, transition, omicron)
  outcome <- "cvddeath"
  cuts <- "cuts"
  
} else {
  cohort <- args[[1]]
  type <- args[[2]]
  subgroup <- args[[3]]
  variant_option <- args[[4]]
  outcome <- args[[5]]
  cuts <- args[[6]]
}

if (subgroup!="all" & variant_option != "ignore") 
  stop("Must set `variant`=\"ignore\" for subgroup analyses.")

# create output directories ----
output_dir <- ghere("output", cohort, "models", "cox_{type}", subgroup, variant_option, outcome)
fs::dir_create(output_dir)

# derive symbolic arguments for programming with

cohort_sym <- sym(cohort)
subgroup_sym <- sym(subgroup)

# read and process data_matched ----
source(here("analysis", "model", "process_data_model.R"))
source(here("analysis", "model", "merge_or_drop.R"))

rm(data_surv)

# cox models ----

# variant = "ignore": ignore the effect of variant
# variant = "split": split follow-up time by variants
# variant = "delta"/"transition"/"omicron": restrict to a variant era
coxcontrast <- function(data, adj = FALSE, cuts=NULL){
  
  if (is.null(cuts)) {
    stop("Specify `cuts`.")
  } else if (length(cuts) < 2) {
    stop("`cuts` must specify a start and an end date")
  } 
  
  fup_period_labels <- str_c(cuts[-length(cuts)]+1, "-", lead(cuts)[-length(cuts)])
  
  data <- data %>% 
    # create variable for cuts[1] for tstart in tmerge
    mutate(time0 = cuts[1])
  
  # derive fup_split (extra processing required when variant_option %in% c("split", "restrict"))
  fup_split <-
    data %>%
    select(new_id, treated) %>%
    uncount(weights = length(cuts)-1, .id="period_id") %>%
    mutate(
      fupstart_time = cuts[period_id],
      fupend_time = cuts[period_id+1]-1,
    ) %>%
    droplevels() %>%
    select(
      new_id, period_id, fupstart_time, fupend_time# fup_time
    ) %>%
    mutate(across(period_id, factor, labels = fup_period_labels))
  
  
  # extra processing for variant_option %in% c("split", "restrict")
  if (variant_option == "split") {
    
    fup_split <- fup_split %>%
      left_join(
        fup_split_variant, by = "new_id"
      ) %>%
      # update fupstart_time and fupend_time to be within variant periods
      mutate(across(fupstart_time, ~pmax(.x, variantstart_day))) %>%
      mutate(across(fupend_time, ~pmin(.x, variantend_day))) %>%
      filter(
        # only keep rows where fup start and end times make sense
        fupstart_time <= fupend_time
      ) %>%
      mutate(variant = variant_dates$variant[variant_id]) %>%
      select(new_id, variant, period_id, fupstart_time, fupend_time)
    
  } else if (variant_option == "restrict") {
    
    fup_split <- fup_split %>%
      left_join(
        data %>% distinct(new_id, variant), by = "new_id"
      )
    
  }
  
  # add variant label for follow-up periods
  if (variant_option %in% c("split", "restrict")) {
    
    fup_period_labels <- str_c(
      rep(fup_period_labels, each = nrow(variant_dates)), 
      variant_dates$variant, 
      sep = "; "
    )
    
    fup_split <- fup_split %>%
      mutate(across(period_id, 
                    ~factor(
                      str_c(as.character(.x), variant, sep = "; "),
                      levels = fup_period_labels
                    ))) 
    
  }
  
  data_split <-
    tmerge(
      data1 = data,
      data2 = data,
      id = new_id,
      tstart = time0,
      tstop = tte_outcome,
      ind_outcome = event(if_else(ind_outcome, tte_outcome, NA_real_))
    ) %>%
    # add post-treatment periods
    tmerge(
      data1 = .,
      data2 = fup_split,
      id = new_id,
      period_id = tdc(fupstart_time, period_id)
    ) 
  
  cox_formula_string <- "Surv(tstart, tstop, ind_outcome) ~ treated"
  
  # only keep periods with >2 events per level of exposure
  data_cox <- data_split %>%
    group_by(!!subgroup_sym, period_id, treated, ind_outcome) %>%
    mutate(n_events = n()) %>%
    ungroup(treated, ind_outcome) %>%
    mutate(min_events = min(n_events)) %>%
    ungroup() %>%
    filter(min_events>2) %>%
    select(-n_events, -min_events) %>%
    group_by(!!subgroup_sym) %>%
    nest() %>%
    # add strata(period_id) to cox_formula if period_id has more than one distinct values
    mutate(cox_formula = map(data, ~{
      if_else(
        n_distinct(.x$period_id) == 1,
        cox_formula_string,
        str_c(cox_formula_string, ":strata(period_id)")
      )
    })) %>%
    unnest(cox_formula)
  
  rm(data, fup_split, data_split)
  
  
  if (nrow(data_cox) == 0) {
    cat("Not enough events to fit Cox model.\n")
    # return emtpy tibble so that script doesn't fail
    return(tibble())
  }
    
  # add covariates if fitting adjusted model
  if (adj) {
    
    data_cox <- data_cox %>%
      mutate(
        
        # merge covariate levels until at least `event_threshold` events per expo/outcome/covariate level combo
        # drop if not satisfied with >=2 levels
        data = map(data, ~{
          .x %>%
            select(-all_of(covariates_model)) %>%
            bind_cols(
              lapply(
                covariates_model,
                function(var)
                  merge_or_drop(
                    covariate_name = var,
                    covariate_col = .x[[var]],
                    outcome_col = .x[["ind_outcome"]],
                    expo_col = .x[["treated"]],
                    events_threshold = 2
                  )
              )
            )
        }),
        
        # add the covariates to cox_formula 
        cox_formula = map(data, ~{
          add_covariates <- names(.x)[names(.x) %in% covariates_model]
          str_c(c(cox_formula, add_covariates), collapse = " + ")
        })
        
      ) %>%
      unnest(cox_formula)

  }
  
  # fit the models
  data_cox <-
    data_cox %>%
    mutate(
      cox_obj = map(data, ~{
        coxph(
          as.formula(cox_formula), 
          data = .x, 
          y=FALSE, 
          robust=TRUE, 
          id=patient_id, 
          na.action="na.fail"
          )
      }),
      cox_obj_tidy = map(cox_obj, ~broom::tidy(.x)),
    ) %>%
    select(!!subgroup_sym, cox_obj_tidy) %>%
    unnest(cox_obj_tidy) 
  
  if (!("robust.se" %in% names(data_cox))) {
    # because robust.se column not created when all estimates are NA
    data_cox <- data_cox %>% mutate(robust.se=NA_real_)
  }
  
  data_cox %>%
    transmute(
      !!subgroup_sym,
      term,
      coxhr = exp(estimate),
      coxhr.se = robust.se,
      coxhr.ll = exp(estimate + qnorm(0.025)*robust.se),
      coxhr.ul = exp(estimate + qnorm(0.975)*robust.se),
    )
  
  
}

# apply contrast functions ----
cat(glue("---- Start fitting Cox model ----"), "\n")

if (cuts == "cuts") cuts_arg <- postbaselinecuts 
if (cuts == "overall") cuts_arg <- c(0,maxfup)

cox_out <- coxcontrast(
  data_matched, 
  adj = type == "adj",
  cuts = cuts_arg
)

write_csv(cox_out, fs::path(output_dir, glue("cox_{type}_contrasts_{cuts}_rounded.csv")))

cat(glue("---- Cox model complete! ----"), "\n")
