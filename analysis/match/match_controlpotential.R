# # # # # # # # # # # # # # # # # # # # #
# This script:
# imports processed data
# chooses match sets for each sequential trial
# outputs match summary
#
# The script must be accompanied by one argument:
# `match_round` - the match round (1,2,3,...)

# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----
# Import libraries
library(tidyverse)
library(lubridate)
library(here)
library(glue)
library(MatchIt)

# import local functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "match_control.R"))

# import command-line arguments
args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  match_strategy <- "a"
  match_round <- as.integer("2")
} else {
  match_strategy <- args[[1]]
  match_round <- as.integer(args[[2]])
}

# save elements of match_strategy_* list to global environment
list2env(
  x = get(glue("match_strategy_{match_strategy}")),
  envir = environment()
  )

# get cohort-specific parameters study dates and parameters
match_round_date <- study_dates$control_extract[match_round]

# create output directory
outdir <- ghere("output", "incremental_{match_strategy}", "matchround{match_round}", "controlpotential", "match")
fs::dir_create(outdir)

# Import datasets ----
# import treated cohort
data_treated <- read_rds(here("output", "treated", "eligible", "data_treated.rds")) %>%
  mutate(treated=1L)

# import control cohort 
if (match_round == 1) {
  controlpotential_dir <- "none"
} else {
  controlpotential_dir <- match_strategy
}
data_control <- read_rds(
  ghere("output", "incremental_{controlpotential_dir}", "matchround{match_round}", "controlpotential", "eligible", "data_controlpotential.rds")
) %>% 
  mutate(treated=0L)

# remove already-matched people from previous match rounds
if(match_round > 1) {
  
  # read data from up all successful matches so far
  data_matchstatusprevious <- map_dfr(
    1:(match_round-1),
    ~read_rds(here("output", glue("incremental_{match_strategy}"), glue("matchround", .x), "controlactual", "match", "data_matched.rds"))
  ) %>%
    select(patient_id, treated)
  
  # do not select treated people who have already been matched
  data_treated <- 
    data_treated %>%
    anti_join(
      data_matchstatusprevious, 
      by=c("patient_id", "treated")
    )
  
  # do not select untreated people who have already been matched
  data_control <- 
    data_control %>%
    anti_join(
      data_matchstatusprevious, 
      by=c("patient_id", "treated")
    )

}

# bind to create dataset of all eligibile individuals
data_eligible <- bind_rows(data_treated, data_control) %>%
  mutate(
    treatment_date = if_else(treated==1L, vax_boostautumn_date, as.Date(NA))
  ) 

rm(data_treated, data_control)

local({

  ## sequential trial match routine is as follows:
  # each daily trial includes all n people who were vaccinated on that day (treated=1) and
  # a sample of n controls (treated=0) who:
  # - had not been vaccinated on or before that day (still at risk of treatment);
  # - still at risk of an outcome (not deregistered or dead); 
  # - had not already been selected as a control in a previous trial

  # all possible trial dates
  all_trial_dates <- seq(study_dates$studystart, study_dates$recruitmentend, 1) 
  
  # initialise list of candidate controls
  candidate_ids <- data_eligible %>% filter(treated==0) %>% pull(patient_id)

  # initialise match summary data
  data_treated <- NULL
  data_matched <- NULL

  # trial_index = 1
  for(trial_index in seq_along(all_trial_dates)){

    cat("match trial ", trial_index, "\n")
    
    trial_date <- all_trial_dates[trial_index]
    
    # set of people vaccinated on trial_date
    data_treated_i <-
      data_eligible %>%
      filter(
        # select treated
        treated == 1L,
        # select people vaccinated on trial day i
        treatment_date == trial_date
        ) %>% 
      transmute(
        patient_id,
        treated,
        trial_index=trial_index,
        trial_date=trial_date
      )
    
    # append total treated on trial day i to all previous treated people
    data_treated <- bind_rows(data_treated, data_treated_i)

    # set of people still eligible for control inclusion on trial day
    data_control_i <-
      data_eligible %>%
      filter(
        # select controls
        treated==0L,
        # remove anyone already vaccinated
        (vax_boostautumn_date > trial_date) | is.na(vax_boostautumn_date),
        # select only people not already selected as a control
        patient_id %in% candidate_ids
      ) %>%
      transmute(
        patient_id,
        treated,
        trial_index=trial_index,
        trial_date=trial_date
      )
    
    n_treated_all <- nrow(data_treated_i)
    
    if(n_treated_all<1) {
      message("Skipping trial ", trial_index, " - No treated people eligible for inclusion.")
      next
    }
  
    match_candidates_i <- 
      bind_rows(data_treated_i, data_control_i) %>%
      left_join(
        data_eligible %>% 
          select(
            patient_id, 
            treated,
            all_of(c(
              exact_vars, 
              names(caliper_vars)
              )),
        ),
        by = c("patient_id", "treated")
      )

    # run match algorithm
    # (this will be updated to allow for different matching strategies)
    set.seed(10)
    obj_matchit_i <-
      safely_matchit(
        data = match_candidates_i,
        exact = exact_vars,
        caliper = caliper_vars
      )[[1]]
    
    if(is.null(obj_matchit_i)) {
      message("Skipping trial ", trial_index, " - No exact matches found.")
      next
    }
    
    data_matchstatus_i <-
      if(is.null(obj_matchit_i)){
        tibble(
          patient_id = match_candidates_i$patient_id,
          matched = FALSE,
          #thread_id = data_thread$thread_id,
          match_id = NA_integer_,
          treated = match_candidates_i$treated,
          weight = 0,
          trial_index = trial_index,
          trial_date = trial_date,
        )
      } else {
        tibble(
          patient_id = match_candidates_i$patient_id,
          matched = !is.na(obj_matchit_i$subclass),
          #thread_id = data_thread$thread_id,
          match_id = as.integer(as.character(obj_matchit_i$subclass)),
          treated = obj_matchit_i$treat,
          weight = obj_matchit_i$weights,
          trial_index = trial_index,
          trial_date = trial_date,
        ) 
      } %>%
      arrange(match_id, treated)
    
    # summary info for recruited people
    # - one row per person
    # - match_id is within match_i
    data_matched_i <-
      data_matchstatus_i %>%
      filter(!is.na(match_id)) %>% # remove unmatched people. equivalent to weight != 0
      arrange(match_id, desc(treated)) %>%
      left_join(
        data_eligible %>% select(patient_id, treated, vax_boostautumn_date),
        by = c("patient_id", "treated")
      ) %>%
      group_by(match_id) %>%
      mutate(
        controlistreated_date = vax_boostautumn_date[treated==0], # this only works because of the group_by statement above! do not remove group_by statement!
      ) %>%
      ungroup()

    n_treated_matched <- sum(data_matched_i$treated)

    # append matched data to matches from previous trials
    data_matched <- bind_rows(data_matched, data_matched_i)
    
    # update list of candidate controls to those who have not already been recruited
    candidate_ids <- candidate_ids[!(candidate_ids %in% data_matched_i$patient_id)]

  }

  # remove trial_index and trial_date counters created by the loop
  rm(trial_index, trial_date)

  data_matched <-
    data_matched %>%
    transmute(
      patient_id, 
      match_id, 
      matched = TRUE, 
      treated,
      control = 1L - treated, 
      trial_index, 
      trial_date, 
      controlistreated_date
    )

  # match status for all treated people and their controls (if matched).
  # includes: unmatched treated; matched treated; matched control
  data_matchstatus <<-
    data_treated %>%
    left_join(
      data_matched %>% filter(treated==1L, matched) %>% select(-trial_date), 
      by=c("patient_id", "treated", "trial_index")
      ) %>%
    mutate(
      matched = replace_na(matched, FALSE), 
      control = if_else(matched==1L, 0L, NA_integer_) # 1 if matched control, 0 if matched treated, NA if unmatched treated
    ) %>%
    bind_rows(
      data_matched %>% filter(control==1L) %>% mutate(treated=0L)
    )
  
  unmatched_control_ids <<- candidate_ids
})
 
# output match status ----
data_matchstatus %>%
  write_rds(file.path(outdir, "data_potential_matchstatus.rds"), compress="gz")

# number of treated/controls per trial
with(data_matchstatus %>% filter(matched), table(trial_index, treated))

# total matched pairs
with(data_matchstatus %>% filter(matched), table(treated))

# max trial date
print(
  paste0(
    "max trial day is ", 
    as.integer(max(data_matchstatus %>% filter(matched) %>% pull(trial_index), na.rm=TRUE))
    )
  )


# output csv for subsequent study definition
data_matchstatus %>% 
  filter(control==1L, matched) %>% 
  select(patient_id, trial_date, match_id) %>%
  mutate(
    trial_date=as.character(trial_date)
  ) %>%
  write_csv(file.path(outdir, "potential_matchedcontrols.csv.gz"))


dup_control_ids <- data_matchstatus %>%
  filter(control==1L, matched) %>% 
  group_by(patient_id) %>% 
  summarise(n=n(), .groups = "drop") %>%
  filter(n>1) %>% nrow() 

stopifnot("Duplicate patient_ids in the control group" = dup_control_ids == 0)
