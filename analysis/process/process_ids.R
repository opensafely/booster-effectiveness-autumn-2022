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

# source(here("analysis", "process", "process_functions.R"))

# create output directory
outdir <- here("output", "final", "eligible")
fs::dir_create(outdir)

# read matched data for comparative and relative effectiveness

# comparative:
data_matchstatus_comparative <-read_rds(here("output", "treated", "matching", "data_matchstatus.rds"))

# relative:
data_matchstatus_relative <-read_rds(ghere("output", "matchround{n_matching_rounds}", "controlactual", "matching", "data_matchstatus_allrounds.rds"))

# treated and matched:
data_matchedtreated <- bind_rows(
  data_matchstatus_comparative %>% 
    filter(matched) %>% 
    rename(trial_date = autumnbooster2022_date),
  data_matchstatus_relative %>% filter(treated == 1)
) %>%
  distinct(patient_id, trial_date)

cat("Number of duplicate IDs (should be zero):\n")
data_matchedtreated %>% 
  group_by(patient_id) %>%
  summarise(n=n(), .groups = "keep") %>% 
  filter(n>1) %>%
  nrow() %>%
  print()
  

# control and matched:
data_matchedcontrol <- data_matchstatus_relative %>% 
  filter(treated == 0) %>%
  select(patient_id, trial_date)

# save datasets
data_matchedtreated %>%
  write_csv(file.path(outdir, "data_matchedtreated.csv.gz"))

data_matchedcontrol %>%
  write_csv(file.path(outdir, "data_matchedcontrol.csv.gz"))

# ## create output directories and define parameters ----
# fs::dir_create(ghere("output", "match"))
# studydef_path <- ghere("output", "extract", "input_controlfinal.feather")
# custom_path <- ghere("output", "dummydata", "dummy_control_final.feather")
# 
# # import data ----
# if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")) {
#   
#   extract_path <-  here::here("output", "final", "dummydata", glue::glue("dummydata_final.feather"))
#   
# } else {
#   
#   extract_path <- here::here("output", "final", "extract", glue::glue("input_final.feather"))
#   
# }
# 
# # read data
# data_extract <- arrow::read_feather(extract_path) %>%
#   process_input()
# 
# # summarise extracted data 
# my_skim(data_extract, path = ghere("output", "final", "extract", "input_controlfinal_skim.txt"))
# 
# # process the final dataset ----
# data_matchstatus <- read_rds(ghere("output", cohort, "matchround{n_matching_rounds_list[[cohort]]}", "actual", "data_matchstatus_allrounds.rds"))
# 
# data_treatedeligible <- read_rds(ghere("output", xxx, "treated", "data_treatedeligible.rds"))
# 
# data_treated <- 
#   left_join(
#     data_matchstatus %>% filter(treated==1L),
#     data_treatedeligible,
#     by="patient_id"
#   ) 
# 
# 
# # import extracted data from controls
# 
# 
# # import final dataset of matched controls, including matching variables
# # alternative to this is re-extracting everything in the study definition
# data_control <- 
#   data_matchstatus %>% filter(treated==0L) %>%
#   left_join(
#     map_dfr(
#       seq_len(n_matching_rounds_list[[cohort]]), 
#       ~{read_rds(ghere("output", cohort, glue("matchround", .x), "actual", "data_successful_matchedcontrols.rds"))}
#     ) %>% select(-match_id, -trial_date, -treated, -controlistreated_date), # remove to avoid clash with already-stored variables
#     by=c("patient_id", "matching_round")
#   ) %>%
#   # merge with outcomes data
#   left_join(
#     data_extract,
#     by=c("patient_id", "match_id", "trial_date")
#   ) %>%
#   mutate(
#     treated=0L
#   )
# 
# # check final data agrees with matching status
# 
# all(data_control$patient_id %in% (data_matchstatus %>% filter(treated==0L) %>% pull(patient_id)))
# all((data_matchstatus %>% filter(treated==0L) %>% pull(patient_id)) %in% data_control$patient_id)
# 
# # merge treated and control groups
# data_matched <-
#   bind_rows(
#     data_treated,
#     data_control %>% 
#       # process the covariates and post-baseline variables (done previously for data_treated)
#       process_covs() %>%
#       process_outcome() 
#   ) %>%
#   select(
#     ends_with("_id"),
#     starts_with(other_variables),
#     any_of(c(matching_variables, covariates, events_lookup$event_var, subgroups))
#   )
# 
# # for reading into analysis scripts
# data_matched %>%
#   write_rds(here("output", cohort, "match", "data_matched.rds"), compress="gz")
# 
# # for reading into tests project yaml
# # treated
# data_matched %>%
#   filter(treated==1) %>%
#   select(patient_id, trial_date) %>%
#   write_csv(here("output", cohort, "match", "data_matched_treated.csv.gz"))
# # control
# data_matched %>%
#   filter(treated==0) %>%
#   select(patient_id, trial_date) %>%
#   write_csv(here("output", cohort, "match", "data_matched_control.csv.gz"))
# # unique patient ids for reading into noncoviddeathcause project yaml
# data_matched %>%
#   distinct(patient_id) %>%
#   write_csv(here("output", cohort, "match", "data_matched_unique.csv.gz"))
# 
# # summarise matched data by treatment group
# data_matched %>% filter(treated==0) %>%
#   my_skim(
#     path = here("output", cohort, "match", "data_matched_control_skim.txt")
#   )
# data_matched %>% filter(treated==1) %>%
#   my_skim(
#     path = here("output", cohort, "match", "data_matched_treated_skim.txt")
#   )
# 
# # matching status of all treated, eligible people ----
# 
# data_treatedeligible_matchstatus <- 
#   left_join(
#     data_treatedeligible %>% select(patient_id, vax3_date, vax3_type),
#     data_matchstatus %>% filter(treated==1L),
#     by="patient_id"
#   ) %>%
#   mutate(
#     matched = if_else(is.na(match_id), 0L, 1L),
#     treated = if_else(is.na(match_id), 1L, treated),
#   )
# 
# print(
#   glue(
#     "all trial dates match vaccination dates for matched, treated people: ",
#     data_treatedeligible_matchstatus %>% 
#       filter(matched==1L) %>%
#       mutate(
#         agree = trial_date==vax3_date
#       ) %>% pull(agree) %>% all()
#   )
# )
# 
# write_rds(data_treatedeligible_matchstatus, here("output", cohort, "match", "data_treatedeligible_matchstatus.rds"), compress="gz")
# 
