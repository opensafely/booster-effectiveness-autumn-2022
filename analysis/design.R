# # # # # # # # # # # # # # # # # # # # # #
# # This script:
# # creates metadata for aspects of the study design
# # # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

## Import libraries ----
library(tidyverse)
library(here)

## create output directories ----
fs::dir_create(here("lib", "design"))

# redaction threshold ----
threshold <- 6

# define key dates ----

study_dates <- lst(

  # vaccine schedule dates
  dose1 = lst(
    pfizer = "2020-12-08", # first pfizer vaccination in national roll-out
    az = "2021-01-04", # first az vaccination in national roll-out
    moderna = "2021-04-13", # first moderna vaccination in national roll-out
  ),
  dose2 = lst(
    end =  "2021-12-01", # don't recruit anyone with second vaccination after this date
  ),
  booster1 = lst(
    pfizer = "2021-09-16", # first pfizer vaccination in national roll-out
    moderna = "2021-10-29", # first moderna vaccination in national roll-out
  ),
  boosterspring2022 = lst(
    start = "2022-03-23"
  ),
  boosterautumn2022 = lst(
    ages65plus = "2022-09-12",
    ages50to64 = "2022-10-15"
  ),
  
  studystart = min(boosterautumn2022$ages65plus, boosterautumn2022$ages50to64),
  recruitmentend = "2022-12-24", #TBC
  studyend = "2023-01-31",# TBC end of available hospitalization data
  
)

study_dates <- rapply(
  study_dates, 
  function(x) as.Date(x, format = "%Y-%m-%d"), 
  how = "list"
  )

extract_increment <- 14

study_dates$control_extract = seq(study_dates$studystart, study_dates$recruitmentend, extract_increment)

# reduce the matching rounds for testing
study_dates$control_extract <- study_dates$control_extract[1:2]

# number of matching rounds to perform for each cohort

n_matching_rounds <- length(study_dates[["control_extract"]])

jsonlite::write_json(study_dates, path = here("lib", "design", "study-dates.json"), auto_unbox=TRUE, pretty =TRUE)

# # define outcomes ----
# 
# events_lookup <- tribble(
#   ~event, ~event_var, ~event_descr,
# 
#   # other
#   "dereg", "dereg_date", "Deregistration date",
# 
#   # effectiveness
#   "postest", "postest_date", "Positive SARS-CoV-2 test",
#   "covidadmitted", "covidadmitted_date", "COVID-19 hospitalisation",
#   "covidcritcare", "covidcritcare_date", "COVID-19 critical care",
#   "coviddeath", "coviddeath_date", "COVID-19 death",
#   "covidcritcareordeath", "covidcritcareordeath_date", "COVID-19 critical care or death",
# 
#   # other
#   "emergency", "emergency_date", "A&E attendance",
#   "emergencyhosp", "emergencyhosp_date", "A&E attendance with disharge to hospital",
#   "covidemergency", "covidemergency_date", "COVID-19 A&E attendance",
#   "covidemergencyhosp", "covidemergencyhosp_date", "COVID-19 A&E attendance with disharge to hospital",
#   "noncoviddeath", "noncoviddeath_date", "Non-COVID-19 death",
#   "cvdnoncoviddeath", "cvdnoncoviddeath_date", "CVD-related non-COVID-19 death",
#   "cancernoncoviddeath", "cancernoncoviddeath_date", "Cancer-related non-COVID-19 death",
#   "death", "death_date", "Any death",
#   "fracture", "fracture_date", "Fracture"
# 
# )
# 
# # outcomes <- c("postest",  "covidadmitted", "covidcritcareordeath", "coviddeath", "emergency", "covidemergency", "noncoviddeath")
# outcomes <- c("covidadmitted", "coviddeath", "noncoviddeath", "cvdnoncoviddeath", "cancernoncoviddeath", "fracture")
# 
# # define treatments ----
# 
# treatement_lookup <-
#   tribble(
#     ~dose, ~treatment, ~treatment_descr,
#     "4", "xxx", "xxx",
#     "3","pfizer", "BNT162b2",
#     "3", "az", "ChAdOx1-S",
#     "3", "moderna", "mRNA-1273",
#     "primary", "pfizer-pfizer", "BNT162b2",
#     "primary", "az-az", "ChAdOx1-S",
#     "primary", "moderna-moderna", "mRNA-1273"
#   )
# 
# ## lookups to convert coded variables to full, descriptive variables ----
# 
# recoder <-
#   lst(
#     subgroups = c(
#       `Main` = "all",
#       `Third dose brand` = "vax3_type",
#       `Prior SARS-CoV-2 infection` = "prior_covid_infection",
#       `Primary course vaccine brand` = "vax12_type",
#       `Age` = "agegroup"
#     ),
#     status = c(
#       `Unmatched`= "unmatched",
#       `Matched` = "matched"
#     ),
#     treated = c(
#       `Two doses` = "0",
#       `Three doses` = "1"
#     ),
#     outcome = set_names(events_lookup$event, events_lookup$event_descr),
#     all = c(`Main` = "all"),
#     prior_covid_infection = c(
#       `No prior SARS-CoV-2 infection` = "FALSE",
#       `Prior SARS-CoV-2 infection` = "TRUE"
#     ),
#     vax12_type = c(
#       `BNT162b2` = "pfizer-pfizer",
#       `ChAdOx1-S` = "az-az"
#     ),
#     vax3_type = c(
#       `BNT162b2` = "pfizer",
#       `mRNA-1273` = "moderna"
#     ),
#     agegroup = c(
#       `18-49 years` = "18-49",
#       `50-64 years` = "50-64",
#       `65-79 years` = "65-79",
#       `80+ years` = "80+"
#     )
#   )
# 
# # subgroups <- c("all", "vax3_type", "prior_covid_infection", "vax12_type", "agegroup")
# subgroups <- "all"
# 
# 
# ## follow-up time ----
# 
# fup_params <- lst(
#   # length of baseline period
#   baselinedays = 14,
#   # length of follow-up period
#   postbaselinedays = 28,
#   # number of follow-up periods
#   postbaselineperiods = 6,
#   # where to split follow-up time after recruitment
#   postbaselinecuts = c(0, baselinedays, baselinedays + (1:postbaselineperiods)*postbaselinedays),
#   # maximum follow-up
#   maxfup = max(postbaselinecuts),
# )
# 
# jsonlite::write_json(fup_params, path = here("lib", "design", "fup-params.json"), auto_unbox=TRUE, pretty =TRUE)
# 
# # split into named objects until scripts updated
# for(i in 1:length(fup_params)){
#   assign(names(fup_params)[i],fup_params[[i]])
# }
# 
# 
# # matching variables ----
# 
# # exact variables
# exact_variables <- c(
#   "jcvi_ageband",
#   "cev_cv",
#   "vax12_type",
#   "region",
#   "prior_covid_infection",
#   NULL
# )
# 
# # caliper variables
# caliper_variables <- c(
#   age = 3,
#   vax2_day = 7,
#   NULL
# )
# matching_variables <- c(exact_variables, names(caliper_variables))
# 
# # covariates ----
# 
# covariates_model <- c(
#   "sex",
#   "ethnicity",
#   "imd_Q5",
#   "bmi",
#   "learndis",
#   "sev_mental",
#   "immunosuppressed",
#   "multimorb",
#   "pregnancy",
#   "vax12_gap",
#   "time_since_infection",
#   "prior_test_cat",
#   "flu_vaccine"
# )
# 
# covariates_summarise <- c(
#   "cv",
#   "cev"
# )
# 
# covariates <- c(covariates_model, covariates_summarise)
# 
# # other variables -----
# # keep all variables starting with these strings
# other_variables <- c("trial", "treated", "control", "match", "vax", "jcvi")
# 
# # variant_options <- c("ignore", "split", "restrict")
# # 
# # # define variant dates ----
# # variant_dates <- tribble(
# #   ~variant, ~start_date, 
# #   "delta", study_dates$mrna$start_date, 
# #   "transition", as.Date("2021-12-01"), 
# #   "omicron", as.Date("2022-01-01"),
# # ) %>% 
# #   mutate(end_date = lead(start_date, default = study_dates$studyend_date))
# # 
# # analysis table
# km_args <- expand_grid(
#   model=c("km", "cox_unadj", "cox_adj"),
#   subgroup=subgroups,
#   outcome=outcomes,
# ) 
# 
