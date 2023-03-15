# create final dummy data for control population ----

library('tidyverse')
library('arrow')
library('here')
library('glue')

# import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)


if(length(args)==0){
  cohort <- "mrna"
} else {
  cohort <- args[[1]]
}

fs::dir_create(here("output", cohort,  "dummydata"))


if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){ 
  
  source(here("lib", "functions", "utility.R"))
  
  source(here("analysis", "design.R"))
  
  index_date <- as.Date("2020-01-01") # doesn't matter what this is, just need some constant date
  
  # import all datasets of matched controls, including matching variables
  data_matchedcontrols <- 
    map_dfr(
      seq_len(n_matching_rounds_list[[cohort]]), 
      ~{read_rds(ghere("output", cohort, glue("matchround", .x), "actual", glue("data_successful_matchedcontrols.rds")))},
      .id="matching_round"
    ) %>%
    mutate(
      trial_day = as.integer(trial_date - index_date)
    ) %>%
    select(
      # select variables with_value_from_file
      patient_id, trial_day, match_id,
    )
  
  missing <- function(x, rate){
    missing_index <- seq_len(length(x))[rbinom(length(x), 1, rate)==1]
    x[missing_index] <- NA
    x
  }
  
  
  set.seed(10)
  
  dummydata <- data_matchedcontrols %>%
    mutate(
      # covariates
      bmi = factor(
        sample(
          x = c("Not obese", "Obese I (30-34.9)", "Obese II (35-39.9)", "Obese III (40+)"),
          size = n(),
          replace = TRUE
        ),
        levels = c("Not obese", "Obese I (30-34.9)", "Obese II (35-39.9)", "Obese III (40+)")
      ),
      pregnancy = rbernoulli(n = n(), p=0.01),
      prior_test_frequency = as.integer(rpois(n=n(), lambda=3)),
      flu_vaccine = rbernoulli(n = n(), p=0.1),
      
      # post baseline
      dereg_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.99),
      # primary_care_covid_case_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.7),
      # covid_test_day = missing(as.integer(runif(n=n(), trial_day, trial_day+90)), 0.7),
      postest_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.7),
      emergency_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.8),
      emergencyhosp_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.85),
      covidemergency_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.8),
      covidemergencyhosp_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.85),
      covidadmitted_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.7),
      covidcritcare_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.8),
      # admitted_unplanned_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.7),
      death_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.9),
      coviddeath_day = missing(death_day, 0.7),
      fractureemergency_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.99),
      fractureadmitted_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.99),
      fracturedeath_day = missing(as.integer(runif(n=n(), trial_day, trial_day+maxfup)), 0.99)
    )
  
  
  dummydata %>%
    #convert logical to integer as study defs output 0/1 not TRUE/FALSE
    # mutate(across(where(is.logical), ~ as.integer(.))) %>%
    #convert integer days to dates since index date and rename vars
    mutate(across(ends_with("_day"), ~ as.Date(as.character(index_date + .)))) %>%
    rename_with(~str_replace(., "_day", "_date"), ends_with("_day")) %>%
    write_feather(sink = here("output", cohort, "dummydata", "dummy_control_final.feather"))
  
  
} else {
  
  # save empty output to save space if running on real data
  tibble() %>%
    write_feather(sink = here("output", cohort, "dummydata", "dummy_control_final.feather"))
  
}
