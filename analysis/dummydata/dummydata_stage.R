library(tidyverse)

source(here::here("analysis", "design.R"))

index_date <- study_dates$studystart

# read dummydata_initial
data_custom_dummy <- arrow::read_feather(here::here("lib", "dummydata", "dummydata_initial.feather")) 

# read data_eligible
data_eligible <- read_csv(here::here("output","initial", "eligible", "data_eligible.csv.gz")) 

data_eligible_treated <- read_csv(here::here("output","initial", "eligible", "data_eligible_treated.csv.gz")) 

data_eligible <- data_eligible %>%
  left_join(data_eligible_treated, by = "patient_id") %>%
  left_join(data_custom_dummy, by = "patient_id")

rm(data_eligible_treated, data_custom_dummy)

rbern <- purrr::rbernoulli

data_stage <- data_eligible %>%
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
      x = c("North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East", "London", "South East", "South West"),
      size = nrow(.),
      replace = TRUE
    ),
    imd_Q5 = sample(
      x = c("Unknown", as.character(1:5)),
      size = nrow(.),
      replace = TRUE
    )
  )  %>%
  # covariates
  mutate(
    bmi = sample(
      x = c("Not obese", "Obese I (30-34.9)", "Obese II (35-39.9)", "Obese III (40+)"),
      size = nrow(.),
      replace = TRUE
    ),
    flu_vaccine = rbern(n = nrow(.), p = 0.7)
  )


# jcvi indicator variables
vars_jcvi <- c( 
  "asthma", "chronic_neuro_disease", "chronic_resp_disease", "sev_obesity",
  "diabetes", "sev_mental", "chronic_heart_disease", "chronic_kidney_disease",
  "chronic_liver_disease", "immunosuppressed", "asplenia", "learndis", 
  "hscworker", "carehome", "endoflife", "housebound"
)
names(vars_jcvi) <- vars_jcvi

vars_jcvi_tbl <- map_dfr(
  .x = vars_jcvi,
  .f = function(x) rbern(n = nrow(data_stage), p=0.1)
)

# TODO
# pre vars


test <- arrow::read_feather(here::here("output", "matchround1", "extract", "input_controlpotential.feather")) 

  