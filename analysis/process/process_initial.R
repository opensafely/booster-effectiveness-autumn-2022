################################################################################

# This script:
# - loads input_initial
# - applies the initial inclusion criteria based on vaccination data
# - saves:
#   - flowchart
#   - data for input into subsequent actions
#   - data for input into subsequent study definitions

################################################################################

# Preliminaries ----

# import libraries
library(tidyverse)
library(here)
library(glue)

# import local functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))
source(here("analysis", "process", "process_functions.R"))

# create output directories
path_stem <- here("output", "initial")
fs::dir_create(file.path(path_stem, "processed"))
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
  source(here("analysis", "dummydata", "dummydata_check.R"))
  data_extract <- dummydata_check(
    dummydata_studydef = data_studydef,
    dummydata_custom = data_dummy
  )
  
  rm(data_dummy)
  
} else {
  
  data_extract <- data_studydef
  
}

rm(data_studydef)

data_extract %>%
  my_skim(
    path = file.path(path_stem, "extract", "data_extract_skim.txt")
  )

################################################################################

# process vaccine data ----

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
    # values_drop_na = TRUE
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
    dayssincelastdose = if_else(index > 1, as.integer(date - lag(date)), NA_integer_),
    dateoflastdose = lag(date)
  ) %>%
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
  # don't need rows where index=1 anymore, as index=2 store the information 
  # about their primary course
  filter(index > 1) %>%
  # don't need rows where date is missing and index>2, we only need to keep 
  # index=2 here to make sure the individuals with no doses stay in the data
  filter(!((index > 2) & is.na(date)))

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
      # discard if date of last dose was after the start of spring booster
      # i.e. only count the first dose during the spring booster period
      dateoflastdose >= study_dates$boosterspring$start ~ FALSE,
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

# identify doses that meet the autumn boost criteria
data_vax <- data_vax %>%
  mutate(
    boostautumn = case_when(
      # discard if primary course indices
      index <= 2 ~ FALSE,
      # discard if brand missing
      is.na(brand) ~ FALSE,
      # discard if date of last dose was after the start of autumn booster
      # i.e. only count the first dose during the autumn booster period
      dateoflastdose >= study_dates$boosterautumn[["ages65plus"]] ~ FALSE,
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

if (any(check_courses_cols$total > 1)) {
  cat("Doses categorised as multiple courses:\n")
  check_courses_cols %>% print()
}

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
  
  cat("Number of patients with multiple doses categorised as the following courses:\n")
  
  check_courses_rows %>%
    pivot_longer(
      cols = -patient_id
    ) %>% 
    group_by(name) %>%
    count() %>%
    print()
  
} 

# derive the course variable
data_vax <- data_vax %>%
  mutate(
    course = case_when(
      primary ~ "primary",
      boostfirst ~ "boostfirst",
      boostspring ~ "boostspring",
      boostautumn ~ "boostautumn",
      TRUE ~ "undefined"
    )
  )

# only keep the first date of a vaccine dose of undefined course
data_vax <- data_vax %>%
  arrange(patient_id, date) %>%
  mutate(undefined = course == "undefined") %>%
  group_by(patient_id, undefined) %>%
  mutate(course_index = row_number()) %>%
  ungroup() %>%
  filter(!(undefined & course_index > 1)) %>%
  select(patient_id, age, date, brand, course)

# define function for setting the factor levels of the vax_*_brand variables
set_brand_levels <- function(.data, course_select) {
  # get the brand levels from treatment_lookup
  brand_levels <- treatment_lookup %>% 
    filter(course==course_select) %>% 
    pull(treatment)
  # set the factor levels
  .data %>%
    mutate(
      across(
        !!sym(paste0("vax_", course_select, "_brand")),
        ~factor(
          replace_na(.x, replace = "none"), 
          levels = c("none", brand_levels)
        )
      )
    )
}

# reshape and apply factor levels
data_vax <- data_vax %>%
  pivot_wider(
    names_from = course,
    values_from = c(date, brand),
    names_glue = "vax_{course}_{.value}"
  ) %>%
  # remove unneeded column
  select(-vax_undefined_brand) %>%
  # recode vax_*_brand variables as factors
  set_brand_levels("primary") %>%
  set_brand_levels("boostfirst") %>%
  set_brand_levels("boostspring") %>%
  set_brand_levels("boostautumn") 

# flag to remove those with an undefined dose before their age group's start date
data_vax <- data_vax %>%
  mutate(
    undefinedbefore_start = case_when(
      (age < 65) & (vax_undefined_date < study_dates$boosterautumn$ages50to64) ~ TRUE,
      (age >= 65) & (vax_undefined_date < study_dates$boosterautumn$ages65plus) ~ TRUE,
      TRUE ~ FALSE
      )
  )

# join the static variables from data_extract
data_vax <- data_vax %>%
  left_join(
    data_extract %>% 
      transmute(
        patient_id, 
        # process initial_vars
        sex = fct_case_when(
          sex == "F" ~ "Female",
          sex == "M" ~ "Male",
          TRUE ~ NA_character_
        ),
        ethnicity = factor(
          ethnicity,
          levels = c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other")
        ),
        hscworker
      ),
    by = "patient_id"
  )

# save summary
data_vax %>%
  my_skim(
    path = file.path(path_stem, "processed", "data_vax_skim.txt")
  )

# save data_vax (do this before applying eligibility criteria, so it can be used 
# to set the eligibility criteria for the risk score)
data_vax %>%
  write_rds(
    file.path(path_stem, "processed", "data_vax.rds"), 
    compress = "gz"
  )

################################################################################

# define initial eligibility criteria ----
data_crit_main <- data_vax %>%
  left_join(
    data_extract %>% select(patient_id, registered, has_died),
    by = "patient_id"
    ) %>%
  transmute(
    # define eligibility criteria
    patient_id,
    c0_descr = "All patients in OpenSAFELY-TPP",
    c0 = TRUE,
    c1_descr = "  Alive and registered at the start of the recruitment period",
    c1 = c0 & registered & !has_died,
    c2_descr = "  Aged >= 50 years by the end of the recruitment period",
    c2 = c1 & age >= 50,
    c3_descr = glue("  Homologous primary course of pfizer/az/moderna received before {study_dates$dose2$end}"),
    c3 = c2 & !is.na(vax_primary_date), # homologous/date restrictions already applied
    c4_descr = "  Valid vaccination history before start of the recruitment period",
    c4 = c3 & !undefinedbefore_start,
    include = c4
  ) 

# save flowchart data ----
data_flowchart <- data_crit_main %>%
  select(patient_id, matches("^c\\d+")) %>%
  rename_at(vars(matches("^c\\d+$")), ~str_c(., "_value")) %>%
  pivot_longer(
    cols = matches("^c\\d+"),
    names_to = c("crit", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  group_by(crit, descr) %>%
  summarise(n = sum(value), .groups = "keep") %>%
  ungroup() %>%
  rename(criteria = descr) %>%
  arrange(crit) %>%
  flow_stats_rounded(1)

write_csv(
  data_flowchart,
  file.path(path_stem, "flowchart", "flowchart_unrounded.csv")
  )

################################################################################

# apply initial eligibility criteria ----

data_vax_eligible <- data_vax %>%
  # remove the columns used for applying the eligiblility criteria
  select(-c(undefinedbefore_start)) %>%
  # only keep those who are eligible
  inner_join(data_crit_main %>% filter(include), by = "patient_id") 

rm(data_vax)

# save summary
data_vax_eligible %>%
  my_skim(
    path = file.path(path_stem, "eligible", "data_vax_eligible_skim.txt")
  )

# save patient_ids and vax_boostautumn_date for reading into study_definition_treated.py
data_vax_eligible %>%
  filter(!is.na(vax_boostautumn_date)) %>%
  select(patient_id, vax_boostautumn_date) %>%
  write_csv(file.path(path_stem, "eligible", "data_eligible_treated.csv.gz"))

# save for reading into study_definition_controlpotential.py
data_vax_eligible %>%
  select(patient_id) %>%
  write_csv(file.path(path_stem, "eligible", "data_eligible.csv.gz"))
