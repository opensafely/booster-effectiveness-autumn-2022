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
  data_vax_wide <- tibble(
    
    age = as.integer(runif(population_size, 50,90)),
    
    # primary course (recorded for everyone)
    vax_dose1_day = runif(
      n = population_size, 
      study_days$dose1$pfizer, 
      study_days$dose1$pfizer + 120
    ),
    vax_dose1_brand = sample(
      x = c("pfizer", "az", "moderna"), 
      size = population_size, 
      replace = TRUE, 
      prob = c(0.45,0.45,0.1)
    ),
    vax_dose2_day = runif(
      n = population_size, 
      vax_dose1_day + 70, 
      vax_dose1_day + 100
    ),
    vax_dose2_brand = vax_dose1_brand,
    
    # first booster
    boosterfirst = rbern(population_size, p = 0.9),
    vax_boosterfirst_day = runif(
      n = population_size,
      study_days$boosterfirst$pfizerstart,
      study_days$boosterfirst$end
    ),
    vax_boosterfirst_brand = sample(
      x = c("pfizer", "az", "moderna"), 
      size = population_size, 
      replace = TRUE, 
      prob = c(0.45,0.1,0.45)
    ),
    
    # spring booster
    boosterspring = (age >= 75) & rbern(population_size, 0.8),
    vax_boosterspring_day = runif(
      n = population_size, 
      study_days$boosterspring$start, 
      study_days$boosterspring$end
    ),
    vax_boosterspring_brand = sample(
      x = c("pfizer", "moderna"), 
      size = population_size, 
      replace = TRUE, 
      prob = c(0.5,0.5)
    ),
    
    # autumn booster
    boosterautumn = rbern(population_size, 0.5),
    vax_boosterautumn_day = runif(
      n = population_size, 
      study_days$boosterautumn$ages65plus, 
      study_days$boosterautumn$ages65plus + 90
      ),
    vax_boosterautumn_brand = sample(
      x = c("pfizerbivalent", "modernabivalent"), 
      size = population_size, 
      replace = TRUE, 
      prob = c(0.5,0.5)
    )
    
  ) %>%
    mutate(across(vax_boosterfirst_day, ~if_else(boosterfirst, .x, NA_real_))) %>%
    mutate(across(vax_boosterspring_day, ~if_else(boosterspring, .x, NA_real_))) %>%
    mutate(across(vax_boosterautumn_day, ~if_else(boosterautumn, .x, NA_real_))) %>%
    mutate(across(ends_with("day"), ~ study_dates$studystart + as.integer(.x))) %>%
    rename_with(~str_replace(.x, "day", "date")) %>%
    select(-c(boosterfirst, boosterspring, boosterautumn)) %>%
    mutate(patient_id = row_number(), .before = 1) 
  
  # process dummy data to have same format as study definition
  data_vax_long <- data_vax_wide %>%
    pivot_longer(
      cols = matches("vax_\\w+_\\w+"),
      names_pattern = "vax_(.*)_(.*)",
      names_to = c("dose", ".value")
    ) %>%
    mutate(across(brand, ~if_else(is.na(date), NA_character_, .x))) %>%
    filter(!is.na(date)) %>%
    select(patient_id, age, date, brand) %>%
    distinct(patient_id, age, date, .keep_all = TRUE) 
  
  data_vax_disease <- data_vax_long %>%
    # add an extra random vax date to similate errors in the real data
    bind_rows(
      data_vax_long %>%
        slice_sample(n = 0.05*population_size) %>%
        mutate(across(date, ~ .x + as.integer(rnorm(n = 0.05*population_size, sd = 5))))
    ) %>%
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
    mutate(across(patient_id, row_number))
  
  # save
  data_initial %>%
    arrow::write_feather(sink = file.path(custom_dummy_path, "dummydata_initial.feather"))
  
} else {
  
  # save empty outputs to keep the project yaml happy but not waste storage
  tibble() %>%
    arrow::write_feather(sink = file.path(custom_dummy_path, "empty.feather"))
  
}
