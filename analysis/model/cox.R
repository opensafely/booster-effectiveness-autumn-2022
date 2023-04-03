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
  effect <- "comparative"
  model <- "cox_adj"
  subgroup <- "all"
  outcome <- "covidadmitted"
} else {
  effect <- args[[1]]
  model <- args[[2]]
  subgroup <- args[[3]]
  outcome <- args[[4]]
}

# create output directories
output_dir <- ghere("output", effect, "model", model, subgroup, outcome)
fs::dir_create(output_dir)

# derive symbolic arguments for programming with
subgroup_sym <- sym(subgroup)

# read and process data_matched ----
source(here("analysis", "process", "process_postmatch.R"))

arms <- "treated"
if (effect == "relative") arms <- c(arms, "control")

# add outcomes data
data_matched <- data_matched %>%
  add_vars(vars = "outcomes", arms = arms) %>%
  process_outcomes() 

# because we don't need the covariates  for unadjusted models
if (model == "cox_unadj") covariates_model <- NULL
if (model == "cox_adj") {
  
  # add covariates data
  data_matched <- data_matched %>%
    add_vars(vars = "covs", arms = arms) %>%
    process_covs() 
  
}

source(here("analysis", "process", "process_premodel.R"))

# cox models ----
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
    mutate(across(period_id, factor, labels = fup_period_labels))
  
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
    
    source(here("analysis", "model", "merge_or_drop.R"))
    
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
cat(glue("---- Start fitting overall Cox model ----"), "\n")
cox_out <- coxcontrast(
  data_surv, 
  adj = model == "cox_adj",
  cuts = fup_params$postbaselinecuts
)
write_csv(cox_out, file.path(output_dir, glue("{model}_contrasts_overall_rounded.csv")))
cat(glue("---- overall Cox model complete! ----"), "\n")

cat(glue("---- Start fitting cuts Cox model ----"), "\n")
cox_out <- coxcontrast(
  data_surv, 
  adj = model == "cox_adj",
  cuts = c(0, fup_params$maxfup)
)
write_csv(cox_out, file.path(output_dir, glue("{model}_contrasts_cuts_rounded.csv")))
cat(glue("---- cuts Cox model complete! ----"), "\n")
