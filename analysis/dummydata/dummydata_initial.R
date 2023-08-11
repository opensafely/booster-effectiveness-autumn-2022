# create output directory
custom_dummy_path <- here::here("output", "initial", "dummydata")
fs::dir_create(custom_dummy_path)

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){ 
  
  library(tidyverse)
  
  set.seed(123)
  
  source(here::here("analysis", "design.R"))
  
  rbern <- purrr::rbernoulli
  
  # population size for dummy data (note this will be multiplied by 2)
  # this reduces substanitally when applying initial inclusion criteria
  population_size <- 200000
  
  # study dates in days from studystart_date
  study_days <- rapply(
    study_dates, 
    function(x) as.integer(x - study_dates$studystart), 
    how = "list"
  )
  
  # create dummy data with realistic vaccination dates
  data_vax_wide <- tibble(patient_id = 1:population_size) %>%
    mutate(
      
      age = as.integer(runif(population_size, 18,120)),
      
      # dose1
      tmp_dose1 = rbern(population_size, p = 0.9),
      vax_dose1_day = runif(
        n = population_size, 
        study_days$dose1$pfizer - 7, # build in some errors to check code 
        study_days$dose1$pfizer + 6*7
      ),
      vax_dose1_brand = sample(
        x = c("pfizer", "az", "moderna"), 
        size = population_size, 
        replace = TRUE, 
        prob = c(0.45,0.45,0.1)
      ),
      
      # dose 2
      tmp_dose2 = tmp_dose1 & rbern(population_size, p = 0.9),
      tmp_primarycourse_heterologous = rbern(population_size, p = 0.05),
      vax_dose2_day = runif(
        n = population_size, 
        vax_dose1_day, 
        vax_dose1_day + 14*7
      ),
      vax_dose2_brand = sample(
        x = c("pfizer", "az", "moderna"), 
        size = population_size, 
        replace = TRUE, 
        prob = c(0.45,0.45,0.1)
      ),
      vax_dose2_brand = if_else(
        tmp_primarycourse_heterologous, 
        vax_dose2_brand, 
        vax_dose1_brand
        ),
      
      # first booster
      tmp_boosterfirst = tmp_dose2 & rbern(population_size, p = 0.9),
      vax_boosterfirst_day = runif(
        n = population_size,
        pmax(vax_dose2_day, study_days$boosterfirst$pfizerstart - 7),
        pmax(vax_dose2_day, study_days$boosterfirst$end) + 7
      ),
      vax_boosterfirst_brand = sample(
        x = c("pfizer", "az", "moderna"), 
        size = population_size, 
        replace = TRUE, 
        prob = c(0.45,0.1,0.45)
      ),
      
      # spring booster
      tmp_boosterspring = (age >= 75) & rbern(population_size, 0.8),
      vax_boosterspring_day = runif(
        n = population_size, 
        pmax(vax_boosterfirst_day, study_days$boosterspring$start - 7), 
        pmax(vax_boosterfirst_day, study_days$boosterspring$end) + 7
      ),
      vax_boosterspring_brand = sample(
        x = c("pfizer", "moderna"), 
        size = population_size, 
        replace = TRUE, 
        prob = c(0.5,0.5)
      ),
      
      # autumn booster
      tmp_boosterautumn = (age >= 50) & rbern(population_size, 0.5),
      vax_boosterautumn_day = runif(
        n = population_size, 
        pmax(vax_boosterspring_day, study_days$boosterautumn$ages65plus - 7), 
        pmax(vax_boosterspring_day, study_days$boosterautumn$ages65plus) + 7*12
      ),
      vax_boosterautumn_brand = sample(
        x = c("pfizerbivalent", "modernabivalent"), 
        size = population_size, 
        replace = TRUE, 
        prob = c(0.5,0.5)
      )
      
    ) %>%
    mutate(across(vax_dose1_day, ~if_else(tmp_dose1, .x, NA_real_))) %>%
    mutate(across(vax_dose2_day, ~if_else(tmp_dose2, .x, NA_real_))) %>%
    mutate(across(vax_boosterfirst_day, ~if_else(tmp_boosterfirst, .x, NA_real_))) %>%
    mutate(across(vax_boosterspring_day, ~if_else(tmp_boosterspring, .x, NA_real_))) %>%
    mutate(across(vax_boosterautumn_day, ~if_else(tmp_boosterautumn, .x, NA_real_))) %>%
    mutate(across(ends_with("day"), ~ study_dates$studystart + as.integer(.x))) %>%
    rename_with(~str_replace(.x, "day", "date")) %>%
    select(-starts_with("tmp_")) 
  
  # process dummy data to have same format as study definition
  data_vax_long <- data_vax_wide %>%
    pivot_longer(
      cols = matches("vax_\\w+_\\w+"),
      names_pattern = "vax_(.*)_(.*)",
      names_to = c("dose", ".value")
    ) %>%
    filter(!is.na(date)) %>%
    select(patient_id, age, date, brand) %>%
    distinct(patient_id, age, date, .keep_all = TRUE) 
  
  data_vax_disease <- data_vax_long %>%
    select(-brand) %>%
    group_by(patient_id) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(index = row_number()) %>%
    ungroup() %>%
    filter(index <= 5) %>%
    pivot_wider(
      names_from = index,
      values_from = date,
      names_glue = "covid_vax_disease_{index}_date"
    )
  
  data_vax_brand <- data_vax_long %>%
    group_by(patient_id, brand) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(index = row_number()) %>%
    ungroup() %>%
    pivot_wider(
      names_from = c(brand, index),
      names_glue = "covid_vax_{brand}_{index}_date",
      values_from = date
    ) %>%
    select(
      patient_id,
      # select according to the limits set in the study definition
      matches(c("covid_vax_\\w+bivalent_1_date", "covid_vax_(pfizer|moderna)_[1-4]_date", "covid_vax_az_[1-2]_date"))
    )
  
  # bind
  data_initial <- data_vax_disease %>%
    left_join(data_vax_brand, by = "patient_id") 
  
  # duplicate to increase matching success
  data_initial <- bind_rows(data_initial, data_initial) %>%
    # define static variables
    mutate(
      registered = rbern(nrow(.), p = 0.99),
      has_died = rbern(nrow(.), p = 0.01),
      sex = sample(x = c("M", "F"), size = nrow(.), replace = TRUE),
      ethnicity = sample(
        x = c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other", "Unknown"),
        size = nrow(.),
        replace = TRUE,
        prob = c(0.5, 0.12, 0.12, 0.12, 0.12, 0.02)
      ),
      hscworker = rbern(n = nrow(.), p=0.05)
      ) %>%
    mutate(across(patient_id, row_number))
  
  # save
  data_initial %>%
    arrow::write_feather(sink = file.path(custom_dummy_path, "dummydata_initial.feather"))
  
} else {
  
  # save empty outputs to keep the project yaml happy but not waste storage
  arrow::write_feather(
    x = tibble::tibble(),
    sink = file.path(custom_dummy_path, "empty.feather")
    )
  
}
