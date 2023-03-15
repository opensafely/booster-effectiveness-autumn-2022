# create dummy data for treated and potential control population ----

library('tidyverse')
library('arrow')
library('here')
library('glue')

source(here("lib", "functions", "utility.R"))

# remotes::install_github("https://github.com/wjchulme/dd4d")
library('dd4d')

source(here("analysis", "design.R"))

source(here("analysis", "dummy", "sim_lst.R"))

population_size <- 20000

# create dummy data for variables defined before baseline ----
bivalentstart_date <- study_dates$bivalent$start_date
index_date <- bivalentstart_date

# dose 1 dates
firstpfizer_date <- study_dates$firstpfizer_date
firstaz_date <- study_dates$firstaz_date
firstmoderna_date <-  study_dates$firstmoderna_date

# convert to days
index_day <- 0L
pfizerstart_day <- as.integer(study_dates$pfizer$start_date - index_date)
modernastart_day <- as.integer(study_dates$moderna$start_date - index_date)

firstpfizer_day <- as.integer(firstpfizer_date - index_date)
firstaz_day <- as.integer(firstaz_date - index_date)
firstmoderna_day <- as.integer(firstmoderna_date - index_date)


known_variables <- c(
  "index_date", "bivalentstart_date", "firstpfizer_date", "firstaz_date", "firstmoderna_date",
  "index_day", "bivalentstart_day", "firstpfizer_day", "firstaz_day", "firstmoderna_day",
  "maxfup"
)

sim_list <- splice(
  sim_list_vax,
  sim_list_jcvi,
  sim_list_covs,
  sim_list_demographic,
  sim_list_pre,
)

bn <- bn_create(sim_list, known_variables = known_variables)

# bn_plot(bn)
# bn_plot(bn, connected_only = TRUE)

set.seed(10)

dummydata <- bn_simulate(bn, pop_size = population_size, keep_all = FALSE, .id = "patient_id")

# create covid_vax_disease variables, as the dependencies are difficult to specify using bn_node
dummydata_vax <- dummydata %>%
  select(patient_id, starts_with("covid_vax")) %>% 
  mutate(
    covid_vax_disease_1_type = if_else(
      !is.na(covid_vax_disease_1_day),
      sample(x = c("pfizer","az","moderna"), size = nrow(.), prob = c(0.5,0.4,0.1), replace=TRUE),
      NA_character_
    ),
    covid_vax_disease_2_type = if_else(
      !is.na(covid_vax_disease_2_day),
      covid_vax_disease_1_type,
      NA_character_
    ),
    covid_vax_disease_3_type = if_else(
      !is.na(covid_vax_disease_3_day),
      sample(x = c("pfizer","az","moderna"), size = nrow(.), prob = c(0.5, 0.1, 0.4), replace=TRUE),
      NA_character_
    ),
    covid_vax_disease_4_type = if_else(
      !is.na(covid_vax_disease_4_day),
      sample(x = c("pfizer","moderna"), size = nrow(.), prob = c(0.5, 0.5), replace=TRUE),
      NA_character_
    ),
    covid_vax_disease_5_type = if_else(
      !is.na(covid_vax_disease_5_day),
      sample(x = c("pfizer", "moderna"), size = nrow(.), prob = c(0.5, 0.5), replace=TRUE),
      NA_character_
    )
  ) %>%
  pivot_longer(
    cols = -patient_id,
    names_to = c("sequence", ".value"),
    names_pattern = "covid_vax_disease_(.)_(.*)",
    values_drop_na = TRUE
  ) %>%
  pivot_wider(
    names_from = c("sequence", "type"),
    names_glue = "covid_vax_{type}_{sequence}_day",
    values_from = day
  )

dummydata_processed <- dummydata  %>%
  left_join(dummydata_vax, by = "patient_id") %>%
  # convert logical to integer as study defs output 0/1 not TRUE/FALSE
  # mutate(across(where(is.logical), ~ as.integer(.))) %>%
  # re-index outcomes on cavid_vax_disease_4_day
  mutate(across(all_of(names(sim_list_outcome)), ~ covid_vax_disease_4_day + .)) %>%
  # convert integer days to dates since index date and rename vars
  mutate(across(ends_with("_day"), ~ as.Date(as.character(index_date + .)))) %>%
  rename_with(~ str_replace(., "_day", "_date"), ends_with("_day"))


# save dummy data files ----

fs::dir_create(here("lib", "dummydata"))

# dummy_treated
dummydata_processed %>% 
  filter(!is.na(covid_vax_disease_3_date)) %>% 
  write_feather(sink = here("lib", "dummydata", "dummy_treated.feather"))

# dummy_control_potential1 (reused for actual)
dummydata_processed %>% 
  select(-all_of(str_replace(names(sim_list_outcome), "_day", "_date"))) %>%
  select(-all_of(names(sim_list_covs))) %>%
  select(-matches("covid_vax_\\w+5_date")) %>%
  write_feather(sink = here("lib", "dummydata", "dummy_control_potential1.feather"))

