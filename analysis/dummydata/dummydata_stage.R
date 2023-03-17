# create output directories
custom_dummy_path_treated <- here::here("output", "treated", "dummydata")
custom_dummy_path_controlpotential <- here::here("output", "matchround1",  "controlpotential", "dummydata")
fs::dir_create(custom_dummy_path_treated)
fs::dir_create(custom_dummy_path_controlpotential)

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){ 
  
  library(tidyverse)
  
  set.seed(123)
  
  source(here::here("analysis", "design.R"))
  
  index_date <- study_dates$studystart
  
  # read dummydata_initial
  data_custom_dummy <- arrow::read_feather(here::here("output", "initial", "dummydata", "dummydata_initial.feather")) 
  
  # read data_eligible
  data_eligible <- read_csv(here::here("output","initial", "eligible", "data_eligible.csv.gz")) %>%
    mutate(across(patient_id, as.integer))
  
  data_eligible_treated <- read_csv(here::here("output","initial", "eligible", "data_eligible_treated.csv.gz")) %>%
    mutate(across(patient_id, as.integer))
  
  data_eligible <- data_eligible %>%
    left_join(data_eligible_treated, by = "patient_id") %>%
    left_join(data_custom_dummy, by = "patient_id")
  
  rm(data_eligible_treated, data_custom_dummy)
  
  # jcvi indicator variables
  vars_jcvi <- c( 
    "asthma", "chronic_neuro_disease", "chronic_resp_disease", "sev_obesity",
    "diabetes", "sev_mental", "chronic_heart_disease", "chronic_kidney_disease",
    "chronic_liver_disease", "immunosuppressed", "asplenia", "learndis", 
    "hscworker", "carehome", "endoflife", "housebound"
  )
  names(vars_jcvi) <- vars_jcvi
  
  rbern <- purrr::rbernoulli
  
  data_stage <- data_eligible %>%
    select(patient_id, age, autumnbooster2022_date) %>%
    # demo variables
    mutate(
      has_follow_up_previous_1year = rbern(n = nrow(.), p=0.99),
      sex = sample(x = c("M", "F"), size = nrow(.), replace = TRUE),
      ethnicity = sample(
        x = c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other", "Unknown"),
        size = nrow(.),
        replace = TRUE,
        prob = c(0.5, 0.1, 0.1, 0.1, 0.1, 0.1)
      ),
      practice_id = sample(x = 1L:100L, size = nrow(.), replace = TRUE),
      msoa = sample(
        x = str_c("E020000", str_pad(1:17, width = 2, side = "left", pad = "0")),
        size = nrow(.),
        replace = TRUE
      ),
      stp = sample(
        x = str_c("STP", 1:10),
        size = nrow(.),
        replace = TRUE
      ),
      region = sample(
        x = c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East", "London", "South East", "South West"),
        size = nrow(.),
        replace = TRUE
      ),
      imd_Q5 = sample(
        x = c("Unknown", as.character(1:5)),
        size = nrow(.),
        replace = TRUE
      )
    )  %>%
    # jcvi vars
    bind_cols(
      map_dfr(
        .x = vars_jcvi,
        .f = function(x) rbern(n = nrow(.), p=0.1)
      )
    ) %>%
    # prevars
    mutate(
      admitted_unplanned_0_date = if_else(
        rbern(n = nrow(.), p = 0.1),
        -as.integer(runif(n = nrow(.), 1, 100)),
        NA_integer_
      ),
      discharged_unplanned_0_date = {
        length_of_stay_unplanned = as.integer(rpois(n = nrow(.), 7))
        if_else(
          admitted_unplanned_0_date + length_of_stay_unplanned < 0,
          admitted_unplanned_0_date + length_of_stay_unplanned,
          NA_integer_
        )
      },
      admitted_planned_0_date = if_else(
        rbern(n = nrow(.), p = 0.1) & is.na(admitted_unplanned_0_date),
        -as.integer(runif(n = nrow(.), 1, 100)),
        NA_integer_
      ),
      discharged_planned_0_date = {
        length_of_stay_planned = as.integer(rpois(n = nrow(.), 7))
        if_else(
          admitted_planned_0_date + length_of_stay_planned < 0,
          admitted_planned_0_date + length_of_stay_planned,
          NA_integer_
        )
      },
      admitted_covid_0_date = if_else(
        rbern(n = nrow(.), p = 0.1),
        admitted_unplanned_0_date,
        NA_integer_
      )
    ) %>%
    mutate(across(
      c(admitted_unplanned_0_date, discharged_unplanned_0_date, admitted_planned_0_date, discharged_planned_0_date, admitted_covid_0_date),
      ~ index_date + .x
    ))
  
  # treated
  data_stage %>%
    filter(!is.na(autumnbooster2022_date)) %>%
    # covariates
    mutate(
      bmi = sample(
        x = c("Not obese", "Obese I (30-34.9)", "Obese II (35-39.9)", "Obese III (40+)"),
        size = nrow(.),
        replace = TRUE
      ),
      flu_vaccine = rbern(n = nrow(.), p = 0.7)
    ) %>%
    arrow::write_feather(file.path(custom_dummy_path_treated, "dummydata_treated.feather"))
  
  # potential 
  data_stage %>%
    select(-autumnbooster2022_date) %>%
    arrow::write_feather(file.path(custom_dummy_path_controlpotential, "dummydata_controlpotential.feather"))
  
} else {
  
  # save empty outputs to keep the project yaml happy
  tibble() %>%
    arrow::write_feather(sink = file.path(custom_dummy_path_treated, "empty.feather"))
  
  tibble() %>%
    arrow::write_feather(sink = file.path(custom_dummy_path_controlpotential, "empty.feather"))
  
}
