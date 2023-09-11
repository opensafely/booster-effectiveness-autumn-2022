# prepare final matched datasets (do in script rather than saving to save storage)

if (effect == "comparative") {
  
  data_matched <- local({
    
    # read treated data
    data_treated <- read_rds(here("output", "treated", "eligible", "data_treated.rds"))
    
    # read match status
    data_matchstatus <- read_rds(ghere("output", "comparative_{match_strategy}", "match", "data_matchstatus.rds")) %>%
      filter(matched) %>%
      select(patient_id, treated, trial_date)
    
    data_matched <- data_treated %>%
      right_join(data_matchstatus, by = "patient_id") 
    
    # only read outcomes data when needed, as all baseline variables already extracted for treated
    if (read_final) {
      
      data_outcomes <- arrow::read_feather(ghere("output", "treated", "extract", "input_final_treated.feather")) %>%
        mutate(across(ends_with("_date"), ~as.Date(.x))) %>%
        process_outcomes()
      
      data_matched <- data_matched %>%
        left_join(data_outcomes %>% select(-trial_date), by = "patient_id")
      
      rm(data_outcomes)
      
    }
    
    return(data_matched)
    
  })
  
}


if (effect == "incremental") {
  
  data_matched <- local({
    
    # read matched data
    data_matched <- map_dfr(
      1:n_match_rounds,
      ~read_rds(here("output", glue("incremental_{match_strategy}"), glue("matchround", .x), "controlactual", "match", "data_matched.rds"))
    )
    
    if (read_final) {
      
      # read outcome and covariates for controls
      data_final_control <- 
        arrow::read_feather(ghere("output", "incremental_{match_strategy}", "match", "input_final_{match_strategy}.feather")) %>%
        mutate(across(ends_with("_date"), ~as.Date(.x)))
      
      # read covariates for treated
      data_treated <- 
        read_rds(here("output", "treated", "eligible", "data_treated.rds")) %>%
        select(any_of(names(data_final_control)))
      
      # read outcomes for treated
      data_outcomes_treated <- 
        arrow::read_feather(ghere("output", "treated", "extract", "input_final_treated.feather")) %>%
        mutate(across(ends_with("_date"), ~as.Date(.x))) 
      
      # join covariates and outcomes for treated
      data_final_treated <- data_treated %>% 
        left_join(data_outcomes_treated, by = "patient_id")
      
      # bind covariates and outcomes for treated and controls
      data_final <- bind_rows(data_final_control, data_final_treated)
      
      # identify variables that are in data_matched and data_final
      names_intersect <- base::intersect(names(data_matched), names(data_final))
      
      # join to matched data
      data_matched <- data_matched %>%
        # remove variables in names_intersect apart from those used to join
        select(-setdiff(names_intersect, c("patient_id", "trial_date"))) %>%
        left_join(
          data_final, 
          by = c("patient_id", "trial_date")
        )
      
      # process covariates and outcomes
      data_matched <- data_matched %>%
        process_extra_vars(extra_vars = adj_vars) %>%
        process_outcomes()
      
    }
    
    return(data_matched)
    
  })
  
}

# fix outcome event dates
if (read_final & (Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations"))) {
  
  data_matched <- data_matched %>%
    mutate(
      across(
        all_of(c(censor_vars[[effect]], str_c(outcomes, "_date"))), 
        ~if_else(.x < trial_date, as.Date(NA_character_), .x)
        )
      )
  
}
