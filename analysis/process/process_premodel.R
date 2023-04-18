# this script reads and processes data_matched
# it is called in analysis/model/km.R and analysis/model/cox.R
# and creates split data when variant_option="split"

cat("---- start process_premodel\n")

if (effect == "relative") {
  
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

# Will - can you think of a way to do this within mutate?
# (when you have a character vector of the names of the censor variables of unknown length)
# possible with c_across and rowwise, but very slow
# also, should we be censoring on controlistreated_date or controlistreated_date-1?
# currently implementing the former
data_matched$censor_date <- do.call(
  pmin, 
  splice(
    map(censor_vars[[effect]], ~data_matched[[.x]]), 
    data_matched[["trial_date"]] - 1 + fup_params$maxfup,
    na.rm = TRUE
    )
  )


data_surv <- data_matched %>%
  mutate(all="all") %>%
  select(
    # select only variables needed for models to save space
    patient_id, new_id, treated, trial_date, censor_date,
    outcome_date = !! sym(glue("{outcome}_date")),
    all_of(covariates_model),
    all_of(subgroup)
  ) %>%
  mutate(
    # trial_date-1 below because we assume vax occurs at the start of the day, 
    # and so outcomes occurring on the same day as treatment are assumed "1 day" long
    tte_outcome = tte(trial_date - 1, outcome_date, censor_date, na.censor=FALSE), 
    ind_outcome = censor_indicator(outcome_date, censor_date),
  )

# check one row per new_id
cat("check for duplicate new_id:\n")
data_surv %>% group_by(new_id) %>% count() %>% filter(n>1) %>% nrow() %>% print()
# should always be 0

# check for non-positive event times
cat("check for non-positive tte_outcome:\n")
check_pos_tte <- data_surv %>% group_by(tte_outcome>0) %>% count() %>% print()
stopifnot("tte_outcome has non-positive event times" = all(check_pos_tte[[1]]))

surv_formula <- formula(Surv(tte_outcome, ind_outcome) ~ 1)

cat("---- end process_premodel ---- \n")
