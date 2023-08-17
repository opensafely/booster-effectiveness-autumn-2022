######################################

# This script:

######################################

# Preliminaries ----

## Import libraries ----
library(tidyverse)
library(lubridate)
library(glue)
library(here)

## import local functions and parameters ---

source(here("analysis", "design.R"))

source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "match_control.R"))

# source(here("analysis", "process", "process_functions.R"))

## import command-line arguments ----

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  match_strategy <- "riskscore_i"
  match_round <- as.integer("1")
} else {
  match_strategy <- args[[1]]
  match_round <- as.integer(args[[2]])
} 

# save items in the match_strategy list to the global environment
list2env(
  x = get(glue("match_strategy_{match_strategy}")),
  envir = environment()
)

# create output directory
outdir <- ghere("output", "incremental_{match_strategy}", "matchround{match_round}", "controlactual", "match")
fs::dir_create(outdir)

data_eligible <- read_rds(
  ghere("output", "incremental_{match_strategy}", "matchround{match_round}", "controlactual", "eligible", "data_controlactual.rds")
)

# trial info for potential matches in round X
data_potential_matchstatus <- read_rds(
  ghere("output", "incremental_{match_strategy}", "matchround{match_round}", "controlpotential", "match", "data_potential_matchstatus.rds")
) %>% 
  filter(matched)

# controlactual data (matches with matching variables re-extracted on trial_date)
data_control <- data_eligible %>% mutate(treated = 0L, matched = 1L)
# data from treated individuals
data_treated_matched <- read_rds(
  ghere("output", "treated", "eligible", "data_treated.rds")
  ) %>%
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

rm(data_control, data_eligible, data_treated_matched)

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

# tidy up
rm(data_unsuccessful_match)

# print matchit success for each trial
if (nrow(data_rematch_all) > 0) {
  cat("matchit rerun success by trial_index:\n")
  data_rematch_all %>%
    arrange(trial_index) %>%
    group_by(trial_index, matched) %>%
    count() %>%
    ungroup() %>%
    pivot_wider(names_from = matched, values_from = n, values_fill = 0) %>% 
    print(n=Inf)
}

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
  p_stillmatch <- round(100*n_stillmatch/n_total, 1)
  print(glue("{n_stillmatch} of {n_total} ({p_stillmatch}%) of controls still match."))
  p_overall <- 0
  if (n_stillmatch < n_total) {
    # rematch 
    denom_rematch <- n_total - n_stillmatch
    n_rematch <- data_rematch_successful %>% filter(treated == 0) %>% nrow()
    p_rematch <-  round(100*n_rematch/denom_rematch, 1)
    print(glue("{n_rematch} of {denom_rematch} ({p_rematch}%) of controls rematched."))
    # overall
    n_overall <- n_stillmatch + n_rematch
    p_overall <- round(100*n_overall/n_total,1)
    print(glue("Overall matching success: {n_overall} of {n_total} ({p_overall}%) treated individuals matched."))
  } 
  cat("----------------------\n")
  if (
    ((p_stillmatch > 99.9) | (p_overall > 99.9)) & 
    (match_round < n_match_rounds)
    ) {
    stop(
      "Matching coverage has exceeded 99.9% and this is not the final match_round.\n",
      "Reduce `match_strategy_obj$n_match_rounds` in \"analysis/design.R\",\n",
      "then rerun \"create-project.R\" and rerun this action."
      )
  }
})

# save all successful matches
data_matched <- data_successful_matchstatus %>%
  select(any_of(c(matchstatus_vars, keep_vars))) 

write_rds(
  data_matched,
  file.path(outdir, "data_matched.rds"), 
  compress="gz"
)

# tidy up
rm(data_potential_matchstatus, data_matched)
rm(data_rematch_all, data_rematch_successful)
rm(data_stillmatch, data_stillmatch_pairs)
rm(data_successful_match, data_successful_matchstatus)

# read data from up all successful matches so far
data_matchstatus_allrounds <- map_dfr(
  unique(1:max(1, match_round-1)),
  ~read_rds(file.path(outdir, "data_matched.rds"))
)

# check for duplicate ids within treatment groups
local({
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

# restrict to successful matched controls
data_successful_matchedcontrols <- data_matchstatus_allrounds %>% 
  filter(treated==0L) %>% 
  select(patient_id, trial_date) 

if (match_round < n_match_rounds) {
  
  # read data from all individuals satisfying initial eligibility criteria
  data_initial_eligible <- read_csv(here("output", "initial", "eligible", "data_eligible.csv.gz"))
  
  # save csv of all unmatched controls to read into extract_controlpotential_{matchround+1}
  data_initial_eligible %>%
    # remove all individuals previously matched as controls
    anti_join(
      data_successful_matchedcontrols,
      by = "patient_id"
    ) %>%
    write_csv(file.path(outdir, "data_unsuccessful_matchedcontrols.csv.gz"))
  
}

# for final match_round only
if (match_round == n_match_rounds) {

  # create directory
  outdir_final <- ghere("output", "incremental_{match_strategy}", "match")
  fs::dir_create(outdir_final)
  
  # save patient_id and trial_date for all matched controls for reading into 
  # outcome study def
  write_csv(
    data_successful_matchedcontrols,
    file.path(outdir_final, "data_matchcontrol.csv.gz")
    )
  
} 
