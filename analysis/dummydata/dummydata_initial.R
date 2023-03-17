# create output directory
custom_dummy_path <- here::here("output", "initial", "dummydata")
fs::dir_create(custom_dummy_path)

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){ 
  
  library(tidyverse)
  
  set.seed(123)
  
  source(here::here("analysis", "design.R"))
  
  # population size for dummy data
  population_size <- 20000
  
  index_date <- study_dates$studystart
  
  # dose 1 dates
  firstpfizer_date <- study_dates$dose1$pfizer
  firstaz_date <- study_dates$dose1$az
  firstmoderna_date <-  study_dates$dose1$moderna
  
  firstpfizer_day <- as.integer(firstpfizer_date - index_date)
  firstaz_day <- as.integer(firstaz_date - index_date)
  firstmoderna_day <- as.integer(firstmoderna_date - index_date)
  
  # create dummy data for covid_vax_disease_\\d_date variables
  data_vax_disease <- tibble(
    n_doses = sample(x = 2L:6L, size = population_size, replace = TRUE, prob = c(0.1, 0.6, 0.2, 0.05, 0.05)),
    covid_vax_disease_1_day = as.integer(runif(n = population_size, firstpfizer_day, firstmoderna_day+60)),
    covid_vax_disease_2_day = as.integer(runif(n = population_size, covid_vax_disease_1_day+15, covid_vax_disease_1_day+100)),
    covid_vax_disease_3_day = as.integer(runif(n = population_size, covid_vax_disease_2_day+150, covid_vax_disease_2_day+210)),
    covid_vax_disease_4_day = as.integer(runif(n = population_size, covid_vax_disease_3_day+200, covid_vax_disease_3_day+440)),
    covid_vax_disease_5_day = as.integer(runif(n = population_size, covid_vax_disease_4_day+50, covid_vax_disease_4_day+100)),
    covid_vax_disease_6_day = as.integer(runif(n = population_size, covid_vax_disease_5_day+50, covid_vax_disease_5_day+100)),
  ) %>%
    mutate(across(covid_vax_disease_3_day, ~if_else(n_doses >= 3, .x, NA_integer_))) %>%
    mutate(across(covid_vax_disease_4_day, ~if_else(n_doses >= 4, .x, NA_integer_))) %>%
    mutate(across(covid_vax_disease_5_day, ~if_else(n_doses >= 5, .x, NA_integer_))) %>%
    mutate(across(covid_vax_disease_6_day, ~if_else(n_doses >= 6, .x, NA_integer_))) %>%
    select(-n_doses) %>%
    mutate(across(ends_with("day"), ~ index_date + .x)) %>%
    rename_with(~str_replace(.x, "day", "date")) %>%
    mutate(patient_id = row_number(), .before = 1)
  
  
  # create dummy data for covid_vax_brand_\\d_date variables
  data_vax_brand <- data_vax_disease %>%
    mutate(
      covid_vax_disease_1_brand = sample(x = c("pfizer", "az", "moderna"), size = nrow(.), replace = TRUE, prob = c(0.45,0.45,0.1)),
      covid_vax_disease_2_brand = covid_vax_disease_1_brand,
      covid_vax_disease_3_brand = sample(x = c("pfizer", "moderna", "pfizerbivalent", "modernabivalent"), size = nrow(.), replace = TRUE, prob = c(0.45, 0.45, 0.05, 0.05)),
      covid_vax_disease_4_brand = sample(x = c("pfizer", "moderna", "pfizerbivalent", "modernabivalent"), size = nrow(.), replace = TRUE, prob = c(0.05, 0.05, 0.45, 0.45)),
      covid_vax_disease_5_brand = sample(x = c("pfizer", "moderna", "pfizerbivalent", "modernabivalent"), size = nrow(.), replace = TRUE),
      covid_vax_disease_6_brand = sample(x = c("pfizer", "moderna", "pfizerbivalent", "modernabivalent"), size = nrow(.), replace = TRUE)
    ) %>%
    pivot_longer(
      cols = -patient_id,
      names_pattern = "covid_vax_disease_(.)_(.*)",
      names_to = c("dose", ".value")
    ) %>%
    filter(!is.na(date)) %>%
    group_by(patient_id, brand) %>%
    mutate(dose = rank(date)) %>%
    ungroup() %>%
    pivot_wider(
      names_from = c(brand, dose),
      names_glue = "covid_vax_{brand}_{dose}_date",
      values_from = date
    ) %>%
    select(
      patient_id,
      # select according to the limits set in the study definition
      matches(c("covid_vax_\\w+bivalent_1_date", "covid_vax_(pfizer|moderna)_[1-4]_date", "covid_vax_az_[1-2]_date"))
    )
  
  # bind
  data_initial <- data_vax_disease %>%
    left_join(data_vax_brand, by = "patient_id") %>%
    mutate(age = as.integer(runif(nrow(.), 50,90)), .before = 2)
  
  # save
  data_initial %>%
    arrow::write_feather(sink = file.path(custom_dummy_path, "dummydata_initial.feather"))
  
  # clear environment
  rm(list = ls()[!(ls() == "custom_dummy_path")])
  
  # read study definition dummy data
  data_studydef_dummy <- arrow::read_feather(
    here::here("output", "initial", "extract", "input_initial.feather")
    ) %>%
    # because date types are not returned consistently by cohort extractor
    mutate(across(ends_with("_date"), ~ as.Date(.)))
  
  # read custom dummy data generated above
  data_custom_dummy <- arrow::read_feather(file.path(custom_dummy_path, "dummydata_initial.feather"))
  
  not_in_studydef <- names(data_custom_dummy)[!( names(data_custom_dummy) %in% names(data_studydef_dummy) )]
  not_in_custom  <- names(data_studydef_dummy)[!( names(data_studydef_dummy) %in% names(data_custom_dummy) )]
  
  
  if(length(not_in_custom)!=0) stop(
    paste(
      "These variables are in studydef but not in custom: ",
      paste(not_in_custom, collapse=", ")
    )
  )
  
  if(length(not_in_studydef)!=0) stop(
    paste(
      "These variables are in custom but not in studydef: ",
      paste(not_in_studydef, collapse=", ")
    )
  )
  
  # reorder columns
  data_studydef_dummy <- data_studydef_dummy[,names(data_custom_dummy)]
  
  unmatched_types <- cbind(
    map_chr(data_studydef_dummy, ~paste(class(.), collapse=", ")),
    map_chr(data_custom_dummy, ~paste(class(.), collapse=", "))
  )[ (map_chr(data_studydef_dummy, ~paste(class(.), collapse=", ")) != map_chr(data_custom_dummy, ~paste(class(.), collapse=", ")) ), ] %>%
    as.data.frame() %>% rownames_to_column()
  
  
  if(nrow(unmatched_types)>0) stop(
    #unmatched_types
    "inconsistent typing in studydef : dummy dataset\n",
    apply(unmatched_types, 1, function(row) paste(paste(row, collapse=" : "), "\n"))
  )
  
  
} else {
  
  tibble() %>%
    arrow::write_feather(sink = file.path(custom_dummy_path, "empty.feather"))
  
}
