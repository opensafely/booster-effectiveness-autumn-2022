# create output directories
custom_dummy_path_treated <- here::here("output", "treated", "dummydata")
custom_dummy_path_controlpotential <- here::here("output", "incremental_none", "matchround1",  "controlpotential", "dummydata")
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
  vars_elig <- c(
    "carehome", "endoflife", "housebound"
  )
  names(vars_elig) <- vars_elig
  vars_jcvi <- c( 
    "asthma", "chronic_neuro_disease", "chronic_resp_disease", "sev_obesity",
    "diabetes", "sev_mental", "chronic_heart_disease", "chronic_kidney_disease",
    "chronic_liver_disease", "immunosuppressed", "asplenia", "learndis"
  )
  names(vars_jcvi) <- vars_jcvi
  
  rbern <- purrr::rbernoulli
  
  data_stage <- data_eligible %>%
    select(patient_id, age, vax_boostautumn_date) %>%
    ### inclusion vars
    mutate(
      registered = rbern(n = nrow(.), p=0.99),
      has_died = rbern(n = nrow(.), p=0.01)
    ) %>%
    ### elig vars
    mutate(
      has_follow_up_previous_1year = rbern(n = nrow(.), p=0.99),
      inhospital = rbern(n = nrow(.), p=0.01),
      discharged_covid_0_date = if_else(
        rbern(n = nrow(.), p = 0.01),
        index_date + as.integer(runif(n = nrow(.), -100, 100)),
        as.Date(NA_character_)
      )
    ) %>%
    bind_cols(
      map_dfr(
        .x = vars_elig,
        .f = function(x) rbern(n = nrow(.), p=0.05)
      )
    ) %>%
    ### jcvi vars
    bind_cols(
      map_dfr(
        .x = vars_jcvi,
        .f = function(x) rbern(n = nrow(.), p=0.1)
      )
    ) %>%
    mutate(
      # bmi_value_date_measured = index_date + as.integer(runif(n = nrow(.), -500, 0)),
      bmi_value = rnorm(n = nrow(.), mean = 30, sd = 7),
      bmi = case_when(
        bmi_value < 30 ~ "Not obese",
        bmi_value < 35 ~ "Obese I (30-34.9)",
        bmi_value < 40 ~ "Obese II (35-39.9)",
        TRUE ~ "Obese III (40+)"
      ),
      sev_obesity = bmi == "Obese III (40+)"
    ) %>%
    ### match vars
    mutate(
      # practice_id = sample(x = 1L:100L, size = nrow(.), replace = TRUE),
      # msoa = sample(
      #   x = str_c("E020000", str_pad(1:17, width = 2, side = "left", pad = "0")),
      #   size = nrow(.),
      #   replace = TRUE
      # ),
      # stp = sample(
      #   x = str_c("STP", 1:10),
      #   size = nrow(.),
      #   replace = TRUE
      # ),
      region = sample(
        x = c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East", "London", "South East", "South West"),
        size = nrow(.),
        replace = TRUE
      ),
      # define imd rank
      imd = sample(
        x = 1:32800, 
        size = nrow(.),
        replace = TRUE
      ),
      # round to nearest 100 and add some NAs
      imd = if_else(
        rbern(n = nrow(.), p=0.90),
        as.integer(round(imd, -2)),
        NA_integer_
      ),
      imd = as.character(imd),
      flu_vaccine = rbern(n = nrow(.), p=0.3)
    ) 
  
  # treated
  data_stage %>%
    filter(!is.na(vax_boostautumn_date)) %>%
    arrow::write_feather(file.path(custom_dummy_path_treated, "dummydata_treated.feather"))
  
  # potential 
  data_stage %>%
    select(-vax_boostautumn_date) %>%
    arrow::write_feather(file.path(custom_dummy_path_controlpotential, "dummydata_controlpotential.feather"))
  
} else {
  
  # save empty outputs to keep the project yaml happy but not waste storage
  arrow::write_feather(
    x = tibble::tibble(),
    sink = file.path(custom_dummy_path_treated, "empty.feather")
  )
  
  arrow::write_feather(
    x = tibble::tibble(),
    sink = file.path(custom_dummy_path_controlpotential, "empty.feather")
  )
  
}
