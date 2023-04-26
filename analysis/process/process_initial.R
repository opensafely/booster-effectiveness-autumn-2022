######################################

# This script:
# - loads input_initial
# - applies the initial inclusion criteria based on vaccination data
# - saves:
#   - flowchart
#   - data for input into subsequent actions
#   - data for input into subsequent study definitions

######################################

# Preliminaries ----

# import libraries
library(tidyverse)
library(here)
library(glue)

# import local functions and parameters
source(here::here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))
source(here::here("analysis", "process", "process_functions.R"))

# create output directories
path_stem <- here("output", "initial")
fs::dir_create(file.path(path_stem, "eligible"))
fs::dir_create(file.path(path_stem, "flowchart"))

# Load data ----

# load studydef data
data_studydef <- arrow::read_feather(
  file.path(path_stem, "extract", "input_initial.feather")
) %>%
  process_input()

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){
  
  # read custom dummydata
  data_dummy <- arrow::read_feather(
    file.path(path_stem, "dummydata", "dummydata_initial.feather")
  )
  
  # check custom and studydef dummydata match
  source(here::here("analysis", "dummydata", "dummydata_check.R"))
  dummydata_check(
    dummydata_studydef = data_studydef,
    dummydata_custom = data_dummy
  )
  
  data_extract <- data_dummy
  rm(data_dummy)
  
} else {
  
  data_extract <- data_studydef
  
}

rm(data_studydef)

data_extract %>%
  my_skim(
    path = file.path(path_stem, "extract", "data_extract_skim.txt")
  )

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

# combine and derive some extra variables
data_vax <- data_any %>%
  left_join(data_brand, by = c("patient_id", "date")) %>%
  # this is much quicker than using group_by
  arrange(patient_id, index) %>%
  mutate(
    dose1brand = if_else(index == 2, lag(brand), NA_character_),
    dayssincelastdose = if_else(index > 1, as.integer(date - lag(date)), NA_integer_)
  )  %>%
  left_join(
    data_extract %>% select(patient_id, age), 
    by = "patient_id"
  )

rm(data_any, data_brand)

# identify doses that meet the primary course criteria
data_vax <- data_vax %>%
  mutate(
    primary = case_when(
      # only consider indices 1&2 for primary course
      index > 2 ~ FALSE,
      # discard if brand for index 1 or 2 missing
      is.na(brand) | is.na(dose1brand) ~ FALSE,
      # discard if brand 1 and 2 not the same 
      brand != dose1brand ~ FALSE,
      # discard if date after primary course end 
      date > study_dates$dose2$end ~ FALSE,
      # discard if interval between doses 1 and 2 too short/long
      dayssincelastdose < 17 | dayssincelastdose > 105 ~ FALSE,
      # keep if satisfying brand-specific dates
      brand == "pfizer" & date >= study_dates$dose1[["pfizer"]] ~ TRUE,
      brand == "az" & date >= study_dates$dose1[["az"]] ~ TRUE,
      brand == "moderna" & date >= study_dates$dose1[["moderna"]] ~ TRUE,
      # discard otherwise
      TRUE ~ FALSE
    )
  ) %>%
  # don't need rows where index=1 anymore
  filter(index > 1)

# identify doses that meet the first boost criteria
data_vax <- data_vax %>%
  mutate(
    boostfirst = case_when(
      # discard index not 3
      index != 3 ~ FALSE,
      # discard if brand missing
      is.na(brand) ~ FALSE,
      # discard if date after booster first end 
      date > study_dates$boosterfirst$end ~ FALSE,
      # discard if interval since last dose too short
      dayssincelastdose < 168 ~ FALSE,
      brand == "pfizer" & date >= study_dates$boosterfirst[["pfizerstart"]] ~ TRUE,
      brand == "moderna" & date >= study_dates$boosterfirst[["modernastart"]] ~ TRUE,
      # discard otherwise
      TRUE ~ FALSE
    )
  )

# identify doses that meet the spring boost criteria
data_vax <- data_vax %>%
  mutate(
    boostspring = case_when(
      # discard if primary course indices
      index <= 2 ~ FALSE,
      # discard if brand missing
      is.na(brand) ~ FALSE,
      # discard if date before booster first start 
      date < study_dates$boosterspring$start ~ FALSE,
      # discard if date after booster first end 
      date > study_dates$boosterspring$end ~ FALSE,
      # discard if interval since last dose too short
      dayssincelastdose < 91 ~ FALSE,
      # discard if age < 75
      age < 75 ~ FALSE,
      # discard if brand not pfizer or moderna
      !(brand %in% c("pfizer", "moderna")) ~ FALSE,
      # otherwise TRUE
      TRUE ~ TRUE
    )
  )

# identify doses that meet the aumtumn boost criteria
data_vax <- data_vax %>%
  mutate(
    boostautumn = case_when(
      # discard if primary course indices
      index <= 2 ~ FALSE,
      # discard if brand missing
      is.na(brand) ~ FALSE,
      # discard if date after recruitmentend
      date > study_dates$recruitmentend ~ FALSE,
      # discard if interval since last dose too short
      dayssincelastdose < 91 ~ FALSE,
      # discard if not bivalent
      !(brand %in% c("pfizerbivalent", "modernabivalent")) ~ FALSE,
      # keep if within age-specific cutoff dates
      age < 65 & (study_dates$boosterautumn[["ages50to64"]] <= date) ~ TRUE,
      age >= 65 & (study_dates$boosterautumn[["ages65plus"]] <= date) ~ TRUE,
      # discard otherwise
      TRUE ~ FALSE
    )
  )


# check for any doses categorised as multiple courses
check_courses_cols <- data_vax %>%
  group_by(primary, boostfirst, boostspring, boostautumn) %>%
  count() %>%
  ungroup() %>%
  mutate(total = primary + boostfirst + boostspring + boostautumn)

check_courses_cols %>% print()

stopifnot("Doses categorised as multiple courses." = all(check_courses_cols$total <= 1))

# Check for any individuals with multiple doses categorised as the same course
check_courses_rows <- data_vax %>%
  group_by(patient_id) %>%
  summarise(across(c(primary, boostfirst, boostspring, boostautumn), sum)) %>%
  ungroup() %>%
  filter_at(
    vars(c(primary, boostfirst, boostspring, boostautumn)),
    any_vars(. > 1)
    )

if (nrow(check_courses_rows) > 0) {
  
  cat("Some patients have multiple doses categorised as the following courses:\n")
  
  check_courses_rows %>%
    pivot_longer(
      cols = -patient_id
    ) %>% 
    filter(value > 1) %>%
    distinct(name) %>%
    print()
  
  stop()
  
} else {
  
  cat("No patients have multiple doses categorised as the same course.\n")
  
}

data_vax <- data_vax %>%
  mutate(
    course = case_when(
      primary ~ "primary",
      boostfirst ~ "boostfirst",
      boostspring ~ "boostspring",
      boostautumn ~ "boostautumn",
      TRUE ~ "undefined"
    )
  ) %>%
  select(patient_id, age, index, date, brand, course) 

# identify patients with any undefined doses
# flag if any undefined dose before study starts,
# and keep date of first undefined dose after study start
data_vax_undefined <- data_vax %>%
  filter(course == "undefined") %>%
  mutate(
    undefinedbeforestart = case_when(
      age < 65 & date < study_dates$boosterautumn$ages50to64 ~ TRUE,
      age >= 65 & date < study_dates$boosterautumn$ages65plus ~ TRUE,
      TRUE ~ FALSE
    )
  ) 

data_vax_undefined_remove <- data_vax_undefined %>%
  filter(undefinedbeforestart) %>%
  distinct(patient_id)

# keep first undefined date after start date
data_vax_undefined_date <-  data_vax_undefined %>%
  filter(!undefinedbeforestart) %>%
  # identify first date of an undefined dose after the study start date
  group_by(patient_id, course, age) %>%
  summarise(date = min(date), .groups = "keep") %>%
  ungroup() 
    
# as anyone with a dose of undefined course before the study start date will be excluded

data_vax <- data_vax %>%
  filter(course != "undefined") %>%
  # add the first date of an undefined dose after the start date
  bind_rows(data_vax_undefined_date) %>%
  select(patient_id, age, date, brand, course) %>%
  pivot_wider(
    names_from = course,
    values_from = c(date, brand),
    names_glue = "vax_{course}_{.value}"
  ) %>%
  mutate(
    # flag to remove those with an undefined dose before their age group's start date
    undefinedbeforestart = patient_id %in% data_vax_undefined_remove$patient_id
  )
  
###########

# define eligibility criteria ----
data_vax <- data_vax %>%
  mutate(

    # define eligibility criteria

    c0 = TRUE,
    c1 = c0 & !undefinedbeforestart,

    include = c1

  ) 

# save flowchart data ----
data_flow <- data_vax %>%
  summarise(across(matches("^c\\d"), .fns=sum)) %>%
  pivot_longer(
    cols=everything(),
    names_to="crit",
    values_to="n"
  ) %>%
  mutate(
    criteria = case_when(
      crit == "c0" ~ glue("Alive and registered on {study_dates$studystart}, aged >= 50 and two doses received before {study_dates$dose2$end}"),
      crit == "c1" ~ "Eligible for boosted or unboosted group based on initial eligibility criteria"
    )
  )

data_flow %>%
  flow_stats_rounded(to = 1) %>%
  write_csv(file.path(path_stem, "flowchart", "flowchart_unrounded.csv"))

# apply eligibility criteria ----

data_vax <- data_vax %>%
  filter(include) %>%
  # remove the columns used for eligiblility criteria
  select(-c(c0, c1, include, undefinedbeforestart))

# save summary
data_vax %>%
  my_skim(
    path = file.path(path_stem, "eligible", "data_eligible_skim.txt")
  )

# save dataset
data_vax %>%
  write_rds(
    file.path(path_stem, "eligible", "data_vax.rds"), 
    compress = "gz"
  )

# save patient_ids and vax_boostautumn_date for reading into study_definition_treated.py
data_vax %>%
  filter(!is.na(vax_boostautumn_date)) %>%
  select(patient_id, vax_boostautumn_date) %>%
  write_csv(file.path(path_stem, "eligible", "data_eligible_treated.csv.gz"))

# save for reading into study_definition_controlpotential.py
data_vax %>%
  select(patient_id) %>%
  write_csv(file.path(path_stem, "eligible", "data_eligible.csv.gz"))
