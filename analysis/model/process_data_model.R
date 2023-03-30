# this script reads and processes data_matched
# it is called in analysis/model/km.R and analysis/model/cox.R
# and creates split data when variant_option="split"

cat("---- start process_data_matched\n")

if (outcome %in% c("cvddeath", "cancerdeath")) {
  
  data_matched <- data_matched %>%
    left_join(
      # non-covid death cause data
      arrow::read_feather(here("output", cohort, "noncoviddeathcause", "extract", "input_noncoviddeathcause.feather")) %>%
        mutate(across(ends_with("_date"), as.Date)), 
      by = "patient_id"
    ) %>%
    # cvd or cancer deaths must be non-covid
    mutate(across(
      c(cvddeath_date, cancerdeath_date),
      ~if_else(
        !is.na(.x) & is.na(coviddeath_date), 
        # use noncoviddeath_date in there is any mismatch due to cvddeath_date and 
        # cancerdeath_date being extracted on a different date
        noncoviddeath_date,
        as.Date(NA_character_)
      )
    ))
  
}


#### TEMPORARY FIX
nrow_before <- nrow(data_matched)
data_matched <- data_matched %>%
  filter(is.na(death_date) | trial_date <= death_date)
nrow_after <- nrow(data_matched)
cat(
  "Remove patients with death_date < trial_date:\n",
  glue("{nrow_before-nrow_after} patients removed."),
  "\n"
  )
#### TEMPORARY FIX

# set study end date ----
if (outcome == "postest") {
  study_dates$modelend_date <- study_dates$testend_date
} else {
  study_dates$modelend_date <- study_dates$studyend_date
}

if (variant_option == "restrict") {
  
  # create a duplicate row for each variant era (will filter later)
  data_matched <- data_matched %>%
    uncount(weights = nrow(variant_dates), .id="variant_id") %>%
    mutate(variant = factor(variant_id, labels = variant_dates$variant))
  
} else {
  
  data_matched <- data_matched %>%
    mutate(
      variant_id = 1L, # here variant_id is just constant
      variant = factor(variant_option)
    )
  
}

if (subgroup == "vax3_type") {
  
  # when subgroup is vax3_type, matched pairs take the vax3_type of the treated person
  data_matched <- data_matched %>%
    group_by(trial_date, match_id, matching_round) %>%
    mutate(uniquematch_id = cur_group_id()) %>% 
    ungroup() %>%
    group_by(uniquematch_id) %>%
    mutate(across(vax3_type, ~.x[treated==1])) %>%
    ungroup() %>%
    select(-uniquematch_id)
  
}

## import baseline data for matched individuals, and derive time-to-event variables
data_matched <- data_matched %>%
  # derive start and end dates for variant eras
  group_by(variant_id) %>%
  mutate(
    variantstart_date = if_else(
      variant %in% c("ignore", "split"),
      study_dates[[cohort]]$start_date,
      max(study_dates[[cohort]]$start_date, variant_dates$start_date[variant_id])
    ),
    variantend_date = if_else(
      variant %in% c("ignore", "split"),
      study_dates$modelend_date,
      min(study_dates$modelend_date, variant_dates$end_date[variant_id])
    )
  ) %>%
  ungroup() %>%
  # filter to keep only individuals with trial date during the variant era (only neede when variant_option="restrict")
  # this should take it back to 1 row per patient when variant_option="restrict", but double check this
  filter(
    trial_date >= variantstart_date,
    trial_date < variantend_date
  ) %>%
  # create a new id to account for the fact that some controls become treated (this is only needed for cox models)
  group_by(patient_id, match_id, matching_round, treated) %>% 
  mutate(new_id = cur_group_id()) %>% 
  ungroup() %>%
  mutate(all="all") %>%
  select(
    # select only variables needed for models to save space
    patient_id, new_id, treated, trial_date, variantend_date, variant,
    controlistreated_date,
    vax3_date,
    death_date, dereg_date, coviddeath_date, noncoviddeath_date, vax4_date,
    all_of(covariates_model),
    all_of(c(glue("{outcome}_date"), subgroup))
  ) %>%
  mutate(
    
    #trial_date,
    outcome_date = !! sym(glue("{outcome}_date")),
    
    # follow-up time is up to and including censor date
    censor_date = pmin(
      dereg_date,
      # vax4_date-1, # -1 because we assume vax occurs at the start of the day
      death_date,
      variantend_date,
      trial_date -1 + maxfup, # I think the "-1" term is needed here, because time 0 is trial_date-1, and if the -1 is not included time goes up to maxfup+1, which results in an NA row in the contrasts output
      na.rm=TRUE
    ),
    
    matchcensor_date = pmin(censor_date, controlistreated_date -1, na.rm=TRUE), # new censor date based on whether control gets treated or not
    
    # for debugging
    tte_dereg = tte(trial_date - 1, dereg_date, dereg_date, na.censor=FALSE),
    tte_death = tte(trial_date - 1, death_date, death_date, na.censor=FALSE),
    tte_variantend = tte(trial_date - 1, variantend_date, variantend_date, na.censor=FALSE),
    
    tte_outcome = tte(trial_date - 1, outcome_date, matchcensor_date, na.censor=FALSE), # -1 because we assume vax occurs at the start of the day, and so outcomes occurring on the same day as treatment are assumed "1 day" long
    ind_outcome = censor_indicator(outcome_date, matchcensor_date),
    
  )


# check one row per new_id
cat("check for duplicate new_id:\n")
data_matched %>% group_by(new_id) %>% count() %>% filter(n>1) %>% nrow() %>% print()
# should always be 0

# check for non-positive event times
cat("check for non-positive tte_outcome (should be c(0, 0, nrow(data_matched)):\n")
for (t in c(0,1)) {
  for(x in c("dereg", "death", "variantend")) {
    cat(glue("treated={t}; event={x}:\n"))
    print(
      table(
        cut(
          data_matched[[glue("tte_{x}")]][data_matched[["treated"]] == t],
          c(-Inf, 0, 1, Inf),
          right=FALSE,
          labels= c("<0", "0", ">0")
          )
      )
    )
  }
  
}

cat("---- end process_data_matched ---- \n")

# preprocessing for data_surv ----
cat("---- start data_surv ----\n")
if (variant_option == "split") {
  
  # split follow-up time according to variant era
  # (`fup_split_variant` is also read into the `coxcontrast` function)
  fup_split_variant <-
    data_matched %>%
    select(new_id, trial_date) %>%
    uncount(weights = nrow(variant_dates), .id="variant_id") %>%
    group_by(variant_id) %>%
    mutate(
      variant = variant_dates$variant[variant_id],
      variantstart_date = variant_dates$start_date[variant_id],
      variantend_date = variant_dates$end_date[variant_id],# - 1,
      variantstart_day = as.integer(variantstart_date - trial_date),
      variantend_day = as.integer(variantend_date - trial_date)
    ) %>%
    ungroup() %>%
    # earliest variantstart_day is zero 
    mutate(across(variantstart_day, ~pmax(0, .x))) %>%
    # latest variantend_day is trial_date + maxfup
    mutate(across(variantend_day, ~pmin(maxfup, .x))) %>% 
    # filter nonsense rows after cleaning
    filter(
      # remove rows where variantend_date < trial_date
      variantend_day >= 0,
      # remove rows where cleaned variantend_day <= variantstart_day
      variantend_day > variantstart_day
    ) %>%
    select(
      new_id, variant_id, variantstart_day, variantend_day
    )
  
  data_split_variant <-
    tmerge(
      data1 = data_matched,
      data2 = data_matched,
      id = new_id,
      tstart = 0,
      tstop = tte_outcome,
      ind_outcome = event(if_else(ind_outcome, tte_outcome, NA_real_))
    ) %>%
    # add post-treatment periods
    tmerge(
      data1 = .,
      data2 = fup_split_variant,
      id = new_id,
      variant_id = tdc(variantstart_day, variant_id)
    ) 
  
  data_surv <- data_split_variant %>%
    # update the variant variable from "split" to the variant using variant_id
    mutate(variant = factor(variant_dates$variant[variant_id], levels = variant_dates$variant))
  
  surv_formula <- formula(Surv(tstart, tstop, ind_outcome) ~ 1)
  
} else {
  
  data_surv <- data_matched
  
  surv_formula <- formula(Surv(tte_outcome, ind_outcome) ~ 1)
  
}
cat("---- end data_surv ----\n")