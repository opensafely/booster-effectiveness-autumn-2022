# this script reads and processes data_matched
# it is called in analysis/model/km.R and analysis/model/cox.R

cat("---- start process_premodel\n")

# define arms for the add_vars function
group <- "treated"
if (effect == "incremental") group <- c(group, "control")

# because we only need the covariates for adjusted cox models
if (model %in% c("km", "cox_unadj")) adj_vars <- NULL
if (model == "cox_adj") {
  
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


## create censoring dates 
if (effect == "incremental") {
  # taking treatment dates for all patients
  treated_dates_for_censoring <- read_rds(here("output", "treated", "eligible", "data_treated.rds")) %>% 
    select(patient_id, vax_boostautumn_date)
  
  # merging on to matched dataset so that censoring can be implemented when the control is themselves vaccinated 
  data_matched <- left_join(data_matched, treated_dates_for_censoring) 
  
  # create censoring variable - 
  data_matched$vax_boostautumn_date[which(data_matched$treated==1)] = NA 
  data_matched <- data_matched %>% mutate(
    censor_date = pmin(death_date, 
                       dereg_date, 
                       controlistreated_date, 
                       trial_date - 1 + fup_params$maxfup, 
                       studyend_date, 
                       vax_boostautumn_date, 
                       na.rm = TRUE
    )
  )
}
if (effect == "comparative") {
  # create censoring variable - 
  data_matched <- data_matched %>% mutate(
    censor_date = pmin(death_date, 
                       dereg_date, 
                       trial_date - 1 + fup_params$maxfup, 
                       studyend_date, 
                       na.rm = TRUE
    ), 
    match_id = NA, # added to get fix of false matches to work
    match_round = NA 
  )  
}


  
data_surv <- data_matched %>%
  mutate(all="all") %>%
  select(
    # select only variables needed for models to save space
    patient_id, new_id, treated, trial_date, censor_date,
    match_id, match_round,
    outcome_date = !! sym(glue("{outcome}_date")),
    all_of(strata_vars),
    all_of(adj_vars),
    all_of(subgroup)
  ) %>%
  mutate(
    # trial_date-1 below because we assume vax occurs at the start of the day, 
    # and so outcomes occurring on the same day as treatment are assumed "1 day" long
    tte_outcome = tte(trial_date - 1, outcome_date, censor_date, na.censor=FALSE), 
    ind_outcome = censor_indicator(outcome_date, censor_date),
    .after = "outcome_date"
  )

# check one row per new_id
cat("check for duplicate new_id:\n")
data_surv %>% group_by(new_id) %>% count() %>% filter(n>1) %>% nrow() %>% print()
# should always be 0

# check for non-positive event times
cat("check for non-positive tte_outcome:\n")
check_pos_tte <- data_surv %>% group_by(tte_outcome>0) %>% count()
check_pos_tte %>% print()
if(dim(check_pos_tte)[1] > 1) {
  patids_error <- data_surv %>% filter(tte_outcome<=0) %>% select(patient_id) 
  # print 1
  data_surv %>% filter(patient_id %in% patids_error$patient_id) %>%
    select(patient_id, new_id, treated, trial_date, censor_date, outcome_date, tte_outcome) %>% 
    print()
  # print 2
  data_matched %>% filter(patient_id %in% patids_error$patient_id) %>%
    select(patient_id, match_id, new_id, treated, trial_date, match_round, death_date, 
           dereg_date, controlistreated_date, vax_boostautumn_date) %>% 
    print()
  
}

if(match_strategy == "riskscore_i" & effect == "incremental") { # match errors affecting riskscore_i only - removing affected matched individuals 
  data_surv <- data_surv %>% 
    group_by(trial_date, match_id, match_round) %>% 
    mutate(neg_tte_outcome = min(tte_outcome) < 0) %>% 
    ungroup()
  cat("Number of false matches:\n")
  data_surv %>% filter(neg_tte_outcome == TRUE) %>% group_by(treated) %>% count() %>% print()
  data_surv <- data_surv %>% filter(neg_tte_outcome == FALSE)
  cat("False matches removed \n")
}  
data_surv <- data_surv %>% select(!c(match_id, match_round)) # remove unneeded variables that were added for checking false matches

check_pos_tte <- data_surv %>% group_by(tte_outcome>0) %>% count()
check_pos_tte %>% print()
stopifnot("tte_outcome has non-positive event times" = all(check_pos_tte[[1]]))

surv_formula <- formula(Surv(tte_outcome, ind_outcome) ~ 1)

cat("---- end process_premodel ---- \n")
