######################################

# This script:

######################################

data_relative <- local({
  
  # read data
  data_matchstatus_allrounds <- read_rds(
    ghere("output", "matchround{n_matching_rounds}", "controlactual", "matching", "data_matchstatus_allrounds.rds")
  )
  
  # import final dataset of matched treated
  data_treated <- data_matchstatus_allrounds %>%
    filter(treated == 1) %>%
    select(patient_id, trial_date, treated, controlistreated_date) %>%
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
  
  # # import covariates and outcomes
  # import_vars <- function(vars) {
  #   
  #   map_dfr(
  #     c("treated", "control"),
  #     ~arrow::read_feather(here("output", "final", vars, glue("input_{vars}_", .x, ".feather")))
  #   ) %>%
  #     process_input()
  #   
  # }
  # 
  # data_covs <- import_vars("covs")
  # data_outcomes <- import_vars("outcomes")
  
  # combine all datasets
  data_relative <- bind_rows(data_treated, data_control) %>%
    # derive extra variables
    mutate(
      timesincelastvax = as.integer(trial_date - lastvaxbeforeindex_date)
      )
    # left_join(data_covs, by = c("patient_id", "trial_date")) %>%
    # left_join(data_outcomes, by = c("patient_id", "trial_date"))
  
  return(data_relative)
  
})
