

if (effect == "comparative") {
  
  data_matched <- local({
    
    # read treated data
    data_treated <- read_rds(here("output", "treated", "eligible", "data_treated.rds"))
    
    # read match status
    data_matchstatus <- read_rds(here("output", "comparative", "match", "data_matchstatus.rds")) %>%
      filter(matched) %>%
      select(patient_id, treated, trial_date)
    
    data_treated <- data_treated %>%
      right_join(data_matchstatus, by = "patient_id") 
    
    return(data_treated)
    
  })
  
}


if (effect == "relative") {
  
  data_matched <- local({
    
    # read data
    data_matchstatus_allrounds <- read_rds(
      ghere("output", "matchround{n_match_rounds}", "controlactual", "match", "data_matchstatus_allrounds.rds")
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
      1:n_match_rounds,
      ~read_rds(
        here("output", glue("matchround", .x), "controlactual", "match", "data_successful_matchedcontrols.rds")
      )
    )
    
    # combine all datasets
    data_relative <- bind_rows(data_treated, data_control)
    
    return(data_relative)
    
  })
  
}

# add labels to the treated variable
data_matched <- data_matched %>%
  mutate(across(treated, ~fct_recoderelevel(.x, recoder[[effect]])))
