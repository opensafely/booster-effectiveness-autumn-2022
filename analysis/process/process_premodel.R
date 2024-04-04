# this script reads and processes data_matched
# it is called in analysis/model/km.R and analysis/model/cox.R

cat("---- start process_premodel\n")

# define arms for the add_vars function
group <- "treated"
if (effect == "incremental") group <- c(group, "control")


if ("imd_Q5" %in% adj_vars) {
  # relevel for models
  data_matched <- data_matched %>%
    mutate(imd_Q5 = fct_relevel(imd_Q5, levels(data_matched$imd_Q5)[3]))
}
  
if ("timesince_coviddischarged" %in% adj_vars) {
  # relevel for models
  data_matched <- data_matched %>%
    mutate(
      timesince_coviddischarged = factor(
        if_else(
          timesince_coviddischarged == "No prior COVID-19 admission",
          as.character(timesince_coviddischarged),
          "Prior COVID-19 admission"
        ),
        levels = c("No prior COVID-19 admission", "Prior COVID-19 admission")
      )
    )
}

if (effect == "incremental") {
  
  data_matched <- data_matched %>%
    # create a new id to account for the fact that some controls become treated (this is only needed for cox models)
    group_by(patient_id, match_id, match_round, treated) %>% 
    mutate(new_id = cur_group_id()) %>% 
    ungroup()
}
if (effect == "comparative") {
  
  data_matched <- data_matched %>%
    # can be identical for comparative effectiveness as individuals occur only once
    mutate(new_id = patient_id)
  
}

if (outcome %in% c("covidadmitted", "covidcritcare", "fracture")) {
  studyend_date <- study_dates$hospitalisationend
}
if (outcome %in% c("coviddeath", "noncoviddeath")) {
  studyend_date <- study_dates$deathend
}



# calculate time to event variables

if (effect == "incremental") censor_incremental <- as.integer("1")
if (effect == "comparative") {
  censor_incremental <- as.integer("0")
  data_matched <- data_matched %>%
    mutate(controlistreated_date = as.Date(0) ) 
}

data_tte <- data_matched %>%
  mutate(
    all="all",
    studyend_date,
    outcome_date = !! sym(glue("{outcome}_date")), 
    
    # trial_date-1 below because we assume vax occurs at the start of the day, 
    # and so outcomes occurring on the same day as treatment are assumed "1 day" long
    
    censor_date = pmin(
      trial_date - 1 + fup_params$maxfup,
      dereg_date,
      death_date,
      if_else(rep(censor_incremental, n())==1, controlistreated_date-1, as.Date(Inf)),
      studyend_date,
      na.rm=TRUE
    ),

    tte_censor = tte(trial_date-1, censor_date, censor_date, na.censor=TRUE),
    ind_censor = censor_indicator(censor_date, censor_date),
    
    tte_outcome = tte(trial_date-1, outcome_date, censor_date, na.censor=TRUE),
    ind_outcome = censor_indicator(outcome_date, censor_date),
    
    tte_stop = pmin(tte_censor, tte_outcome, na.rm=TRUE),
  ) %>%
  select(
    # select only variables needed for models to save space
    patient_id, new_id, treated, trial_date, censor_date,
    tte_censor, ind_censor, tte_outcome, ind_outcome, tte_stop,
    all_of(strata_vars),
    all_of(adj_vars),
    all_of(subgroup), 
    stp
  ) 


# check one row per new_id
cat("check for duplicate new_id:\n")
data_tte %>% group_by(new_id) %>% count() %>% filter(n>1) %>% nrow() %>% print()
# should always be 0

# check for non-positive event times
cat("check for non-positive tte_outcome:\n")
check_pos_tte <- data_tte %>% group_by(tte_stop>0) %>% count() %>% print()
stopifnot("tte_outcome has non-positive event times" = all(check_pos_tte[[1]]))


# one row per day
alltimes <- expand(data_tte, patient_id, times=as.integer(full_seq(c(1, tte_stop),1)))
data_plr <-
  tmerge(
    data1 = data_tte %>% select(everything()),
    data2 = data_tte,
    id = patient_id,
    
    outcome_status = tdc(tte_outcome),
    censor_status= tdc(tte_censor),
    
    outcome_event = event(tte_outcome),
    censor_event = event(tte_censor),
    
    tstart = 0L,
    tstop = tte_stop
    
  ) %>%
  tmerge(
    data2 = alltimes,
    id = patient_id,
    alltimes = event(times, times)
  ) 


cat("---- end process_premodel ---- \n")
