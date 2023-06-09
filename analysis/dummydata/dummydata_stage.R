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
    select(patient_id, age, vax_boostautumn_date) %>%
    # demo variables
    mutate(
      registered = rbern(n = nrow(.), p=0.99),
      has_died = rbern(n = nrow(.), p=0.01),
      has_follow_up_previous_1year = rbern(n = nrow(.), p=0.99),
      sex = sample(x = c("M", "F"), size = nrow(.), replace = TRUE),
      ethnicity = sample(
        x = c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other", "Unknown"),
        size = nrow(.),
        replace = TRUE,
        prob = c(0.5, 0.12, 0.12, 0.12, 0.12, 0.02)
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
        x = c("Unknown", "1 (most deprived)", "2", "3", "4", "5 (least deprived)"),
        size = nrow(.),
        prob = c(0.001, rep((1-0.001)/5, 5)),
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
      inhospital = rbern(n = nrow(.), p=0.01),
      discharged_covid_0_date = if_else(
        rbern(n = nrow(.), p = 0.01),
        index_date + as.integer(runif(n = nrow(.), -100, 100)),
        as.Date(NA_character_)
      )
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
