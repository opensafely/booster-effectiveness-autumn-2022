######################################

# This script:

######################################

# Preliminaries ----

# import libraries
library(tidyverse)
library(here)

# import local functions and parameters
source(here::here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))
source(here::here("analysis", "process", "process_functions.R"))

# create output directories
fs::dir_create(here::here("output", "initial", "eligible"))

# define extract_path
data_studydef <- arrow::read_feather(
  here("output", "initial", "extract", "input_initial.feather")
) %>%
  process_input()

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){
  
  data_dummy <- arrow::read_feather(
    here("output", "initial", "dummydata", "dummydata_initial.feather")
  )
  
  # check dummydata
  source(here::here("analysis", "dummydata", "dummydata_check.R"))
  dummydata_check(
    dummydata_studydef = data_studydef,
    dummydata_custom = data_dummy
  )
  
  data_extract <- data_dummy
  rm(data_dummy)
  
} else {
  
  data_extract <- data_studydef
  rm(data_studydef)
  
}



# Transform vaccine data ----

# any brand data
data_any <- data_extract %>%
  select(
    patient_id, 
    matches("covid_vax_disease_\\d_date")
    ) %>%
  pivot_longer(
    cols = -patient_id,
    names_to = "index",
    # need to do this as mutate after as the version of dplyr in opensafely does not support this option
    # names_transform = ~as.integer(str_extract(.x, "\\d")),
    values_to = "date",
    values_drop_na = TRUE
  ) %>%
  mutate(across(index, ~as.integer(str_extract(.x, "\\d"))))

# brand data
data_brand <- data_extract %>%
  select(
    patient_id,
    matches("covid_vax_(pfizer|az|moderna|pfizerbivalent|modernabivalent)_\\d_date")
    ) %>%
  pivot_longer(
    cols = -patient_id,
    names_pattern = "covid_vax_(.*)_\\d_date",
    names_to = "brand",
    values_to = "date",
    values_drop_na = TRUE
  )

# combine
data_vax <- data_any %>%
  left_join(data_brand, by = c("patient_id", "date")) %>%
# `names_glue` doesn't work because older version of dplyr in opensafely
  pivot_wider(
    names_from = index,
    values_from = c(date, brand),
    names_glue = "covid_vax_{index}_{.value}"
  ) %>%
  left_join(
    data_extract %>% 
      transmute(
        patient_id, 
        agegroup = if_else(age>=65, "ages65plus", "ages50to64")
      ), 
    by = "patient_id"
  )

# tidy up
rm(data_any, data_brand, data_extract)

# define eligibility criteria ----
data_criteria <- data_vax %>%
  transmute(

    patient_id,

    ##### primary course criteria

    primarycourse_brand = case_when(

      is.na(covid_vax_1_brand) | is.na(covid_vax_2_brand) ~ FALSE,

      covid_vax_1_brand == "pfizer" &
        covid_vax_2_brand == "pfizer" &
        study_dates$dose1[["pfizer"]] <= covid_vax_1_date ~ TRUE,

      covid_vax_1_brand == "az" &
        covid_vax_2_brand == "az" &
        study_dates$dose1[["az"]] <= covid_vax_1_date ~ TRUE,

      covid_vax_1_brand == "moderna" &
        covid_vax_2_brand == "moderna" &
        study_dates$dose1[["moderna"]] <= covid_vax_1_date ~ TRUE,

      TRUE ~ FALSE

    ),

    primarycourse_interval = case_when(
      # 17-105 days between first and second dose
      17 <= as.integer(covid_vax_2_date - covid_vax_1_date) &
        as.integer(covid_vax_2_date - covid_vax_1_date) <= 105
      ~ TRUE,
      # otherwise
      TRUE ~ FALSE
    ),

    ##### third dose criteria

    thirddose_missing = is.na(covid_vax_3_date),

    thirddose_brand = case_when(

      is.na(covid_vax_3_brand) ~ FALSE,

      covid_vax_3_brand == "pfizer" &
        study_dates$booster1[["pfizer"]] <= covid_vax_3_date ~ TRUE,

      covid_vax_3_brand == "moderna" &
        study_dates$booster1[["moderna"]] <= covid_vax_3_date ~ TRUE,

      covid_vax_3_brand %in% c("pfizerbivalent", "modernabivalent") &
        agegroup == "ages65plus" &
        study_dates$boosterautumn2022[["ages65plus"]] <= covid_vax_3_date ~ TRUE,

      covid_vax_3_brand %in% c("pfizerbivalent", "modernabivalent") &
        agegroup == "ages50to64" &
        study_dates$boosterautumn2022[["ages50to64"]] <= covid_vax_3_date ~ TRUE,

      TRUE ~ FALSE

    ),

    thirddose_interval = case_when(
      is.na(covid_vax_3_date) ~ FALSE,
      # at least 168 days between second and third dose
      168 <= as.integer(covid_vax_3_date - covid_vax_2_date) ~ TRUE,
      # otherwise
      TRUE ~ FALSE
    ),

    ##### fourth dose criteria

    fourthdose_missing = is.na(covid_vax_4_date),

    fourthdose_brand = case_when(

      is.na(covid_vax_4_brand) ~ FALSE,

      covid_vax_4_date %in% c("pfizer", "moderna") &
        study_dates$boosterspring2022$start <= covid_vax_4_date ~ TRUE,

      covid_vax_4_brand %in% c("pfizerbivalent", "modernabivalent") &
        agegroup == "ages65plus" &
        study_dates$boosterautumn2022[["ages65plus"]] <= covid_vax_4_date ~ TRUE,

      covid_vax_4_brand %in% c("pfizerbivalent", "modernabivalent") &
        agegroup == "ages50to64" &
        study_dates$boosterautumn2022[["ages50to64"]] <= covid_vax_4_date ~ TRUE,

      TRUE ~ FALSE

    ),

    fourthdose_interval = case_when(
      is.na(covid_vax_4_date) ~ FALSE,
      # at least 91 days between third and fourth dose
      91 <= as.integer(covid_vax_4_date - covid_vax_3_date) ~ TRUE,
      # otherwise
      TRUE ~ FALSE
    ),

    ##### fifth dose criteria

    fifthdose_missing = is.na(covid_vax_5_date),

    fifthdose_brand = case_when(

      is.na(covid_vax_5_brand) ~ FALSE,

      covid_vax_5_brand %in% c("pfizerbivalent", "modernabivalent") &
        agegroup == "ages65plus" &
        study_dates$boosterautumn2022[["ages65plus"]] <= covid_vax_5_date ~ TRUE,

      covid_vax_5_brand %in% c("pfizerbivalent", "modernabivalent") &
        agegroup == "ages50to64" &
        study_dates$boosterautumn2022[["ages50to64"]] <= covid_vax_5_date ~ TRUE,

      TRUE ~ FALSE

    ),

    fifthdose_interval = case_when(
      is.na(covid_vax_5_date) ~ FALSE,
      # at least 91 days between fourth and fifth dose
      91 <= as.integer(covid_vax_5_date - covid_vax_4_date) ~ TRUE,
      # otherwise
      TRUE ~ FALSE
    ),


    # define eligibility criteria

    c0 = TRUE,
    c1 = c0 & (primarycourse_brand & primarycourse_interval),
    c2 = c1 & (thirddose_missing | (thirddose_brand & thirddose_interval)),
    c3 = c2 & (fourthdose_missing | (fourthdose_brand & fourthdose_interval)),
    c4 = c3 & (fifthdose_missing | (fifthdose_brand & fifthdose_interval)),

    include = c4

  )


# apply eligibility criteria ----

data_eligible <- data_criteria %>%
  filter(include) %>%
  select(patient_id) %>%
  # first vaccination date after eligible for autumn booster
  left_join(
    data_vax %>%
      select(patient_id, agegroup, matches("covid_vax_\\d_date")) %>%
      pivot_longer(
        cols = -c(patient_id, agegroup),
        values_drop_na = TRUE
      ) %>%
      mutate(
        start_date = if_else(
          agegroup == "ages65plus", 
          study_dates$boosterautumn2022[["ages65plus"]],
          study_dates$boosterautumn2022[["ages50to64"]]
          )
        ) %>%
      filter(value >= start_date) %>%
      group_by(patient_id) %>%
      summarise(autumnbooster2022_date = min(value)) %>%
      ungroup()
    , by = "patient_id"
  ) 
  

# save patient_ids and autumnbooster2022_date for reading into study_definition_treated.py
data_eligible %>%
  filter(!is.na(autumnbooster2022_date)) %>%
  write_csv(here::here("output","initial", "eligible", "data_eligible_treated.csv.gz"))

# save for reading into study_definition_controlpotential.py
data_eligible %>%
  select(patient_id) %>%
  write_csv(here::here("output","initial", "eligible", "data_eligible.csv.gz"))

# save data_vax for eligible patients ----
data_criteria %>%
  filter(include) %>%
  select(patient_id) %>%
  left_join(data_vax, by = "patient_id") %>%
  write_rds(here::here("output", "initial", "eligible", "data_vax.rds"), compress = "gz")

# save flowchart data ----
data_flow <- data_criteria %>%
  summarise(across(matches("^c\\d"), .fns=sum)) %>%
  pivot_longer(
    cols=everything(),
    names_to="criteria",
    values_to="n"
  )

data_flow %>%
  flow_stats_rounded(to = 1) %>%
  write_csv(here::here("output", "initial", "eligible", "flowchart_unrounded.csv"))

