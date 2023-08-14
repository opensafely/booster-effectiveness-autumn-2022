# prepare final matched datasets (do in script rather than saving to save storage)

if (effect == "comparative") {
  
  data_matched <- local({
    
    # read treated data
    data_treated <- read_rds(here("output", "treated", "eligible", "data_treated.rds"))
    
    # read match status
    data_matchstatus <- read_rds(ghere("output", "comparative_{match_strategy}", "match", "data_matchstatus.rds")) %>%
      filter(matched) %>%
      select(patient_id, treated, trial_date)
    
    data_treated <- data_treated %>%
      right_join(data_matchstatus, by = "patient_id") 
    
    return(data_treated)
    
  })
  
}


if (effect == "incremental") {
  
  data_matched <- local({
    
    # read data
    data_matched <- map_dfr(
      1:n_match_rounds,
      ~read_rds(here("output", glue("incremental_{match_strategy}"), glue("matchround", .x), "controlactual", "match", "data_matched.rds"))
    )
    
    return(data_matched)
    
  })
  
}
