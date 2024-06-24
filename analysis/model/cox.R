# # # # # # # # # # # # # # # # # # # # #
# Purpose: 
# fit cox models to estimate hazard ratios
# arguments:
# - effect: comparative, relative
# - model: cox_unadj, cox_adj
# - subgroup
# - outcome
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----
# Import libraries
library('tidyverse')
library('here')
library('glue')
library('survival')

# import local functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "survival.R"))
# load process_functions for add_vars, process_covs, and process_outcomes
source(here("analysis", "process", "process_functions.R"))

# import command-line arguments 
args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  effect <- "incremental"
  match_strategy <- "a"
  model <- "cox_adj"
  subgroup <- "all"
  outcome <- "covidadmitted"
} else {
  effect <- args[[1]]
  match_strategy <- args[[2]]
  model <- args[[3]]
  subgroup <- args[[4]]
  outcome <- args[[5]]
}

# save items in the match_strategy list to the global environment
list2env(
  x = get(glue("match_strategy_{match_strategy}")),
  envir = environment()
)

# create output directories
output_dir <- ghere("output", glue("{effect}_{match_strategy}"), "model", model, subgroup, outcome)
fs::dir_create(output_dir)

# derive symbolic arguments for programming with
subgroup_sym <- sym(subgroup)

# read and process data_matched ----
read_final <- TRUE
source(here("analysis", "process", "process_postmatch.R"))
# process data for models
source(here("analysis", "process", "process_premodel.R"))

# cox models ----
coxcontrast <- function(data, adj = FALSE, cuts=NULL) {
  
  if (is.null(cuts)) {
    stop("Specify `cuts`.")
  } else if (length(cuts) < 2) {
    stop("`cuts` must specify a start and an end date")
  } 
  
  fup_period_labels <- str_c(cuts[-length(cuts)]+1, "-", lead(cuts)[-length(cuts)])
  
  data <- data %>% 
    # create variable for cuts[1] for tstart in tmerge
    mutate(time0 = cuts[1])
  
  # derive fup_split 
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
    mutate(across(period_id, ~factor(.x, labels = fup_period_labels)))
  
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
  
  cox_formula_string <- "Surv(tstart, tstop, ind_outcome) ~ strata(trial_date) + strata(stp) + treated"
  
  # only keep periods with >2 events per level of exposure
  prd_counts <- data_split %>%
    group_by(!!subgroup_sym, period_id, treated, ind_outcome) %>%
    count() %>% 
    ungroup() 
    
  prd_counts_spine <-  prd_counts %>% 
    expand(!!subgroup_sym, period_id, treated, ind_outcome) 
  
  prd_counts <- left_join(prd_counts_spine, prd_counts) %>% 
    mutate(
      n_events = case_when(
        !is.na(n) ~ n , 
        is.na(n)  ~ 0L 
      )
    ) %>% 
    group_by(!!subgroup_sym, period_id) %>% 
    mutate(min_events = min(n_events)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>% select(!!subgroup_sym, period_id, min_events) 
  
  data_cox <- left_join(data_split, prd_counts) %>%
    filter(min_events>2) %>%
    select(-min_events) %>%
    group_by(!!subgroup_sym) %>%
    nest() %>%
    # add :period_id to cox_formula if period_id has more than one distinct values
    mutate(cox_formula = map(data, ~{
      if_else(
        n_distinct(.x$period_id) == 1,
        cox_formula_string,
        str_c(cox_formula_string, ":period_id")
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
    
    source(here("analysis", "model", "merge_or_drop.R"))
    
    # need to update this id continuous covars other than age are added
    cat_vars <- adj_vars[!(adj_vars %in% "age")]
    
    data_cox <- data_cox %>%
      mutate(
        
        # merge covariate levels until at least `event_threshold` events per expo/outcome/covariate level combo
        # drop if not satisfied with >=2 levels
        data = map(data, ~{
          .x %>%
            select(-all_of(cat_vars)) %>%
            bind_cols(
              lapply(
                cat_vars,
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
          add_covariates <- names(.x)[names(.x) %in% cat_vars]
          formula_string <- str_c(c(cox_formula, add_covariates), collapse = " + ")
          if ("age" %in% adj_vars) {
            formula_string <- str_c(formula_string, " + poly(age, degree = 2)")
          }
          return(formula_string)
        })
        
      ) %>%
      unnest(cox_formula)

  }
  
  # counts of events in each period - note that periods with <2 events per level of exposure have been removed
  evnt_count <- data_cox[[2]][[1]] %>% 
    count(period_id, ind_outcome, treated) %>%
    filter(ind_outcome == 1) %>%
    mutate(
      term = str_c("treated:period_id", period_id), 
      event_prd = roundmid_any(n, to = 6)
    ) %>%
    select(term, event_prd, treated) %>%
    pivot_wider(names_from = treated, names_prefix = "event_inPeriod_rnd6_", 
                values_from = event_prd)

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
      npat = length(unique(data_cox[[2]][[1]]$patient_id)), 
      nswitch = length(unique(data_cox[[2]][[1]]$new_id)) - npat,
      nevent_total_rnd6 = roundmid_any(cox_obj[[1]]$nevent, to = 6),
      nevent_untreated_rnd6 = roundmid_any(sum(data_cox[[2]][[1]]$ind_outcome==1 & data_cox[[2]][[1]]$treated == 0), to = 6),
      nevent_treated_rnd6   = roundmid_any(sum(data_cox[[2]][[1]]$ind_outcome==1 & data_cox[[2]][[1]]$treated == 1), to = 6),
      cox_obj_tidy = map(cox_obj, ~broom::tidy(.x)),
    ) %>%
    select(!!subgroup_sym, cox_obj_tidy, npat, nswitch, nevent_total_rnd6, nevent_untreated_rnd6, nevent_treated_rnd6) %>%
    unnest(cox_obj_tidy) 
  
  if (!("robust.se" %in% names(data_cox))) {
    # because robust.se column not created when all estimates are NA
    data_cox <- data_cox %>% mutate(robust.se=NA_real_)
  }
  
  data_cox <- data_cox %>%
    transmute(
      !!subgroup_sym,
      term,
      coxhr = exp(estimate),
      coxhr.se = robust.se,
      coxhr.ll = exp(estimate + qnorm(0.025)*robust.se),
      coxhr.ul = exp(estimate + qnorm(0.975)*robust.se),
      npat, 
      nswitch, 
      nevent_total_rnd6, 
      nevent_untreated_rnd6, 
      nevent_treated_rnd6
    )
  
  data_cox <- left_join(data_cox, evnt_count) 

}

# apply contrast functions ----
cat(glue("---- Start fitting overall Cox model ----"), "\n")
cox_out <- coxcontrast(
  data_surv, 
  adj = model == "cox_adj",
  cuts = c(0, fup_params$maxfup)
)  %>% select(!c(event_inPeriod_rnd6_0, event_inPeriod_rnd6_1))
write_csv(cox_out, file.path(output_dir, glue("{model}_contrasts_overall.csv")))
cat(glue("---- overall Cox model complete! ----"), "\n")

cat(glue("---- Start fitting cuts Cox model ----"), "\n")
cox_out <- coxcontrast(
  data_surv, 
  adj = model == "cox_adj",
  cuts = fup_params$postbaselinecuts
)
write_csv(cox_out, file.path(output_dir, glue("{model}_contrasts_cuts.csv")))
cat(glue("---- cuts Cox model complete! ----"), "\n")
