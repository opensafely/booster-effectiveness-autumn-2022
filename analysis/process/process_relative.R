######################################

# This script:

######################################

# Preliminaries ----

## Import libraries ----
library(tidyverse)
library(lubridate)
library(glue)
library(here)

## import local functions and parameters ---

source(here("analysis", "design.R"))

source(here("lib", "functions", "utility.R"))


data_all <- local({
  
  # read data
  data_matchstatus_allrounds <- read_rds(
    ghere("output", "matchround{n_matching_rounds}", "controlactual", "matching", "data_matchstatus_allrounds.rds")
  )
  
  # import final dataset of matched treated
  data_treated <- data_matchstatus_allrounds %>%
    filter(treated == 1) %>%
    select(patient_id, trial_date, controlistreated_date) %>%
    left_join(
      read_rds(here("output", "treated", "eligible", "data_treated.rds")) %>%
        select(-c(index_date, autumnbooster2022_date)),
      by = "patient_id"
    )
  
  # import final dataset of matched controls
  data_control <- map_dfr(
    1:n_matching_rounds,
    ~read_rds(
      here("output", glue("matchround", .x), "controlactual", "matching", "data_successful_matchedcontrols.rds")
    )
  )
  
  # import covariates and outcomes
  import_vars <- function(vars) {
    
    map_dfr(
      c("treated", "control"),
      ~arrow::read_feather(here("output", "final", vars, glue("input_{vars}_", .x, ".feather")))
    ) %>%
      process_input()
    
  }
  
  data_covs <- import_vars("covs")
  data_outcomes <- import_vars("outcomes")
  
  # combine all datasets
  data_all <- bind_rows(data_treated, data_control) %>%
    left_join(data_covs, by = c("patient_id", "trial_date")) %>%
    left_join(data_outcomes, by = c("patient_id", "trial_date"))
  
  return(data_all)
  
})


add_vars <- function(.data, vars, arms) {
  
  stopifnot("")
  
  if (all(c("treated", "control") %in% arms)) {
    
    by_vars <- c("patient_id", "trial_id")
    
  } else if (all(arms == "treated")) {
    
    # omit trial_id here as not needed and will slow down
    by_vars <- "patient_id"
    
  } else {
    
    stop("`arms` must be either c(\"treated\", \"control\") or \"treated\"")
    
  }
  
  source(here::here("analysis", "process", "process_functions.R"))
  
  data_vars <- map_dfr(
    arms,
    ~arrow::read_feather(here("output", "final", vars, glue("input_{vars}_", .x, ".feather")))
  ) %>%
    process_input()
  
  .data %>%
    left_join(data_vars, by = by_vars)
  
}





