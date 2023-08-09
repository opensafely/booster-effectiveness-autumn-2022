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
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))
source(here("analysis", "process", "process_functions.R"))

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
  source(here("analysis", "dummydata", "dummydata_check.R"))
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
    dayssincelastdose = if_else(index > 1, as.integer(date - lag(date)), NA_integer_),
    dateoflastdose = lag(date)
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
  
  cat("Number of patients with multiple doses categorised as the following courses:\n")
  
  check_courses_rows %>%
    pivot_longer(
      cols = -patient_id
    ) %>% 
    group_by(name) %>%
    count() %>%
    print()
  
} else {
  
  cat("No patients have multiple doses categorised as the same course.\n")
  
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
  ) %>%
  select(patient_id, age, index, date, brand, course) 



# create flags for undefined doses
data_vax_undefined <- data_vax %>%
  # identify patients with any undefined doses
  filter(course == "undefined") %>%
  mutate(
    # flag if any undefined dose before risk score start date
    undefinedbefore_riskscore = date < study_dates$riskscore$start,
    # flag if any undefined dose before recruitment starts
    undefinedbeforestart = case_when(
      age < 65 & date < study_dates$boosterautumn$ages50to64 ~ TRUE,
      age >= 65 & date < study_dates$boosterautumn$ages65plus ~ TRUE,
      TRUE ~ FALSE
    )
  ) 
# save patient ids for creating flags later
data_vax_undefined_remove <- data_vax_undefined %>%
  filter(undefinedbeforestart) %>%
  distinct(patient_id)
data_vax_undefined_riskscore_remove <- data_vax_undefined %>%
  filter(undefinedbefore_riskscore) %>%
  distinct(patient_id)

# keep first undefined date after start date, as will impact eligibility for trials
# (we only keep the first so that there is only one row for each patient-course)
data_vax_first_undefined_after <- data_vax_undefined %>%
  filter(!undefinedbeforestart) %>%
  # identify first date of an undefined dose after the study start date
  group_by(patient_id, course, age) %>%
  summarise(date = min(date), .groups = "keep") %>%
  ungroup()
    
data_vax <- data_vax %>%
  # get rid of all undefined courses
  # (this is ok as data_vax_undefined contains the information we need to 
  # people who have an undefined dose before the start of recruitment,
  # and data_vax_first_undefined_after contains the first undefined dose on or 
  # after recruitment
  filter(course != "undefined") %>%
  # add the first date of an undefined dose after the start date
  bind_rows(data_vax_first_undefined_after) %>%
  select(patient_id, age, date, brand, course) %>%
  pivot_wider(
    names_from = course,
    values_from = c(date, brand),
    names_glue = "vax_{course}_{.value}"
  ) %>%
  mutate(
    # flag to remove those with an undefined dose before their age group's start date
    undefinedbeforestart = patient_id %in% data_vax_undefined_remove$patient_id,
    # flag to remove those with an undefined dose before risk score start date
    undefinedbeforestart_riskscore = patient_id %in% data_vax_undefined_riskscore_remove$patient_id,
  )

rm(data_vax_undefined_remove, data_vax_undefined_riskscore_remove, data_vax_first_undefined_after, data_vax_undefined)
  
################################################################################

# define initial eligibility criteria for main study ----
data_crit_main <- data_vax %>%
  transmute(
    # define eligibility criteria
    patient_id,
    c0 = TRUE,
    c1 = c0 & !undefinedbeforestart,
    include = c1
  ) 

# save flowchart data ----
data_flow <- data_crit_main %>%
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

data_vax_eligible <- data_vax %>%
  # only keep those who are eligible
  inner_join(data_crit_main %>% filter(include), by = "patient_id") %>%
  # remove the columns used for eligiblility criteria
  select(-c(undefinedbeforestart, vax_undefined_brand)) %>%
  # recode vax_*_brand variables as factors
  mutate(across(matches("vax_\\w+_brand"), ~replace_na(.x, replace = "none"))) %>%
  mutate(
    across(
      vax_primary_brand, 
      ~factor(
        .x,
        levels = treatment_lookup %>% filter(course=="primary") %>% pull(treatment)
        )
      )
    ) %>%
  mutate(
    across(
      vax_boostfirst_brand, 
      ~factor(
        .x,
        levels = c(
          "none",
          treatment_lookup %>% filter(course=="boostfirst") %>% pull(treatment)
        )
      )
    )
  ) %>%
  mutate(
    across(
      vax_boostspring_brand, 
      ~factor(
        .x,
        levels = c(
          "none",
          treatment_lookup %>% filter(course=="boostspring") %>% pull(treatment)
        )
      )
    )
  ) %>%
  mutate(
    across(
      vax_boostautumn_brand, 
      ~factor(
        .x,
        levels = c(
          "none",
          treatment_lookup %>% filter(course=="boostautumn") %>% pull(treatment)
        )
      )
    )
  ) %>%
  left_join(
    data_extract %>% transmute(
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
data_vax_eligible %>%
  my_skim(
    path = file.path(path_stem, "eligible", "data_vax_skim.txt")
  )

# save dataset
data_vax_eligible %>%
  select(-matches("^c\\d+$"), -include) %>%
  write_rds(
    file.path(path_stem, "eligible", "data_vax.rds"), 
    compress = "gz"
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

rm(data_vax_eligible)

################################################################################

# define initial eligibility criteria for mortality risk score cohort ----
data_vax_riskscore <- data_vax %>%
  mutate(
    
    # define eligibility criteria
    
    c0 = TRUE,
    c1 = c0 & !undefinedbeforestart_riskscore,
    
    include = c1
    
  ) 

# save patient ids for reading into a study definition
data_vax_riskscore %>%
  distinct(patient_id) %>%
  write_csv(here("output", "initial", "eligible", "data_eligible_riskscore_i.csv.gz"))
