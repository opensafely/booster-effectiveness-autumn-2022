# stillmatch and rematch only when stage="controlactual" ----
if (stage == "controlactual") { 
  
  # controlactual data (matches with matching variables re-extracted on trial_date)
  data_control <- data_eligible %>% mutate(treated = 0L, matched = 1L)
  # data from treated individuals
  data_treated_matched <- 
    read_rds(ghere("output", "treated", "eligible", "data_treated.rds")) %>%
    # only keep variables that are in data_control
    select(any_of(names(data_control))) %>%
    # only keep treated individuals who were successfully matched in the 
    # controlpotential stage for this match_round
    right_join(
      data_potential_matchstatus %>% filter(treated==1L, matched),
      by = "patient_id"
    )
  
  # create dataset of candidates for matching
  match_candidates <- bind_rows(data_treated_matched, data_control) %>%
    arrange(treated, match_id, trial_date)
  
  # we need to define this list of variables several times, so create an object
  vars_stillmatch <- c("match_id", "trial_date", names(caliper_vars), exact_vars)
  
  local({
    # check for missing values
    match_candidates_missing <- match_candidates %>%
      select(treated, all_of(vars_stillmatch)) %>%
      map_lgl(~any(is.na(.x)))
    
    if (any(match_candidates_missing)) {
      stop(
        paste(
          "Missing values in the following variables will cause rematching to fail:", 
          str_c(names(match_candidates_missing)[match_candidates_missing], collapse = ", ")
        )
      )
    }
  })
  
  # check if pairs still match ----
  # first join on exact variables + match_id + trial_date
  # (need to do the exact match join even if exact_vars=NULL, otherwise code  
  # for caliper matching won't work, as it relies on the .x .y suffixes
  # generated from the join)
  data_stillmatch_pairs <- inner_join(
    x = data_treated_matched %>% select(all_of(vars_stillmatch)),
    y = data_control %>% select(all_of(vars_stillmatch)),
    by = c("match_id", "trial_date", exact_vars)
  ) 
  
  # next make sure caliper variables are still within the caliper  
  if (length(caliper_vars) > 0) {
    # check caliper_vars are still within caliper
    data_stillmatch_pairs <- data_stillmatch_pairs %>%
      bind_cols(
        map_dfr(
          set_names(names(caliper_vars), names(caliper_vars)),
          ~ abs(data_stillmatch_pairs[[str_c(.x, ".x")]] - data_stillmatch_pairs[[str_c(.x, ".y")]]) <= caliper_vars[.x]
        )
      ) %>%
      filter_at(
        all_of(names(caliper_vars)),
        all_vars(.)
      )
  } 
  
  # save dataset of match_id and trial_date for pairs that still match
  # (i.e. each row is a matched pair)
  data_stillmatch_pairs <- data_stillmatch_pairs %>%
    select(match_id, trial_date) %>%
    mutate(matched=1)
  
  # create dataset of all variables for all individuals who still match
  # (i.e. each row is a patient)
  data_stillmatch <- match_candidates %>%
    inner_join(
      data_stillmatch_pairs, 
      by=c("match_id", "trial_date", "matched")
    ) %>%
    mutate(match_round = match_round) %>%
    arrange(trial_date, match_id, treated)
  
  # rematch ----
  
  # get data for patients whose matches failed in stillmatch
  data_unsuccessful_match <- match_candidates %>%
    anti_join(data_stillmatch_pairs, by=c("match_id", "trial_date", "matched"))
  
  # rerun matchit in the unsuccessful matches to see if any new matches 
  # can be found from the stillmatch failures
  set.seed(10)
  obj_matchit <-
    safely_matchit(
      data = data_unsuccessful_match,
      exact = c("trial_index", exact_vars),
      caliper = caliper_vars
    )[[1]]
  
  # save output (each row is a patient)
  data_rematch_all <- 
    if(is.null(obj_matchit)){
      tibble(
        patient_id = data_unsuccessful_match$patient_id,
        matched = FALSE,
        match_id = data_unsuccessful_match$match_id,
        treated = data_unsuccessful_match$treated,
        control = data_unsuccessful_match$control,
        weight = 0,
        trial_index = data_unsuccessful_match$trial_index
      )
    } else {
      tibble(
        patient_id = data_unsuccessful_match$patient_id,
        matched = !is.na(obj_matchit$subclass),
        # match_id + 1000 to distinguish mathces made by rematchit
        match_id = as.integer(as.character(obj_matchit$subclass)) + 1000L,
        treated = obj_matchit$treat,
        control = 1L - treated,
        weight = obj_matchit$weights,
        trial_index = data_unsuccessful_match$trial_index
      ) 
    }
  
  # print matchit success for each trial
  cat("matchit rerun success by trial_index:\n")
  data_rematch_all %>%
    arrange(trial_index) %>%
    group_by(trial_index, matched) %>%
    count() %>%
    ungroup() %>%
    pivot_wider(names_from = matched, values_from = n, values_fill = 0) %>% 
    print(n=Inf)
  
  # only keep matched individuals and join all other variables
  data_rematch_successful <- data_rematch_all %>%
    filter(matched) %>%
    left_join(
      match_candidates %>% 
        select(-c(match_id, matched, treated, control, controlistreated_date)),
      by = c("patient_id", "trial_index")
    ) %>%
    group_by(match_id) %>%
    # controlistreated_date needs updating, as there are new matched pairs
    mutate(
      # this only works because of the group_by statement above
      controlistreated_date = vax_boostautumn_date[treated==0], 
      .after = control
    ) %>%
    ungroup()
  
  
  # add to successful matches
  data_successful_match <- bind_rows(data_stillmatch, data_rematch_successful)
  
  # process and summarise successful matches ----
  
  matchstatus_vars <- c("patient_id", "match_id", "trial_date", "match_round", "treated", "controlistreated_date")
  
  data_successful_matchstatus <- data_successful_match %>% 
    # keep all variables from the processed data as they are required for adjustments in the cox model
    select(all_of(matchstatus_vars), everything())
  
  # perform some checks
  stopifnot(
    "Missing values in data_successful_matchstatus$treated" = 
      all(!is.na(data_successful_matchstatus$treated))
  )
  stopifnot(
    "Unequal numbers of treated and controls in data_successful_matchstatus" = 
      data_successful_matchstatus %>% filter(treated==1) %>% nrow() ==
      data_successful_matchstatus %>% filter(treated==0) %>% nrow()
  )
  
  local({
    cat("---- Match summary ----\n")
    ## overall matching success for match_round
    n_total <- data_potential_matchstatus %>% filter(treated == 0) %>% nrow()
    # stillmatch
    n_stillmatch <- data_stillmatch_pairs %>% nrow()
    p_stillmatch <- round(100*n_stillmatch/n_total,1)
    print(glue("{n_stillmatch} of {n_total} ({p_stillmatch}%) of controls still match."))
    # rematch 
    n_rematch <- data_rematch_successful %>% filter(treated == 0) %>% nrow()
    p_rematch <-  round(100*n_rematch/(n_total-n_stillmatch),1)
    print(glue("{n_rematch} of {n_total-n_stillmatch} ({p_rematch}%) of controls rematched."))
    n_overall <- n_stillmatch + n_rematch
    p_overall <- round(100*n_overall/n_total,1)
    print(glue("Overall matching success: {n_overall} of {n_total} ({p_overall}%) treated individuals matched."))
    cat("----------------------\n")
  })
  
  ## size of dataset
  print("data_successful_matchstatus treated/untreated numbers")
  print(table(treated = data_successful_matchstatus$treated, useNA="ifany"))
  
  
  # pick up all previous successful matches ----
  if (match_round > 1) {
    data_matchstatusprevious <- read_rds(
      ghere("output", "incremental_{match_strategy}", "matchround{match_round-1}", "controlactual", "match", "data_matchstatus_allrounds.rds")
    )
    data_matchstatus_allrounds <- 
      data_successful_matchstatus %>% 
      select(all_of(matchstatus_vars)) %>%
      bind_rows(data_matchstatusprevious) 
  } else {
    data_matchstatus_allrounds <- 
      data_successful_matchstatus %>%
      select(all_of(matchstatus_vars))
  }
  
  # save all successful matches
  data_matchstatus_allrounds %>%
    write_rds(
      file.path(path_stem, "match", "data_matchstatus_allrounds.rds"), 
      compress="gz"
    )
  
  if (match_round == n_match_rounds) {
    #create directory
    tmp_dir <- ghere("output", "incremental_{match_strategy}", "match")
    fs::dir_create(tmp_dir)
    # save patient_id and trial_date for reading into outcome study def
    data_matchstatus_allrounds %>%
      select(patient_id, trial_date) %>%
      write_csv(file.path(tmp_dir, "data_matchcontrol.csv.gz"))
    rm(tmp_dir)
  }
  
  ## size of dataset
  print("data_matchstatus_allrounds treated/untreated numbers")
  print(table(treated = data_matchstatus_allrounds$treated, useNA="ifany"))
  
  local({
    ## duplicate IDs
    dup_ids <- data_matchstatus_allrounds %>% 
      group_by(treated, patient_id) %>%
      summarise(n=n(), .groups = "drop") %>%
      group_by(treated) %>%
      summarise(ndups = sum(n>1), .groups = "drop") 
    if (any(dup_ids$ndups>0)) {
      print(dup_ids)
      stop("Duplicate patient_ids within treatment groups.")
    }
  })
  
  data_eligible %>%
    my_skim(
      path = file.path(path_stem, "match", "data_successful_matchedcontrols_skim.txt")
    )
  
  data_successful_matchstatus %>% 
    filter(treated==0L) %>% 
    select(-starts_with("vax_boostautumn")) %>%
    write_rds(
      file.path(path_stem, "match", "data_successful_matchedcontrols.rds"), 
      compress="gz"
    )
  
  # read data from all individuals satisfying initial eligibility criteria
  data_eligible <- read_csv(here("output", "initial", "eligible", "data_eligible.csv.gz"))
  # save csv for all unmatched individuals to read into extract_controlpotential_{matchround+1}
  data_eligible %>%
    # remove all individuals previously matched as controls
    anti_join(
      data_matchstatus_allrounds %>% 
        filter(treated == 0) %>%
        distinct(patient_id),
      by = "patient_id"
    ) %>%
    write_csv(file.path(path_stem, "match", "data_unsuccessful_matchedcontrols.csv.gz"))
  
}
