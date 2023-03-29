

if (effect == "comparative") {
  
  data_stage <- local({
    
    # read treated data
    data_treated <- read_rds(here("output", "treated", "eligible", "data_treated.rds"))
    
    # read match status
    data_matchstatus <- read_rds(here("output", "treated", "matching", "data_matchstatus.rds")) %>%
      filter(matched) %>%
      select(patient_id)
    
    data_treated <- data_treated %>%
      right_join(data_matchstatus, by = "patient_id") %>%
      mutate(trial_date = vax_boostautumn_date) %>%
      mutate(treated_desc = vax_boostautumn_brand)
    
    return(data_treated)
    
  })
  
}


if (effect == "relative") {
  
  data_stage <- local({
    
    # read data
    data_matchstatus_allrounds <- read_rds(
      ghere("output", "matchround{n_matching_rounds}", "controlactual", "matching", "data_matchstatus_allrounds.rds")
    )
    
    # import final dataset of matched treated
    data_treated <- data_matchstatus_allrounds %>%
      filter(treated == 1) %>%
      select(patient_id, trial_date, treated, controlistreated_date) %>%
      left_join(
        read_rds(here("output", "treated", "eligible", "data_treated.rds")),
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
      mutate(treated_desc = factor(treated, levels = c(0,1), labels = c("Unboosted", "Boosted")))
    
    return(data_relative)
    
  })
  
}

if (vars == "matching") {
  
  data_stage <- data_stage %>%
    # derive extra variables
    mutate(
      timesincelastvax = as.integer(trial_date - lastvaxbeforeindex_date)
    )
  
}

if (vars == "covs") {
  
  if (effect == "comparative") {
    
    data_stage <- data_stage %>%
      add_vars(vars = "covs", arms = "treated")
    
  }
  
  if (effect == "relative") {
    
    data_stage <- data_stage %>%
      add_vars(vars = "covs", arms = c("treated", "control"))
    
  }
  
  
}

  
