# # # # # # # # # # # # # # # # # # # # #
# Purpose: describe match results
# creates "table 1"
# this file takes the following arguments:
# - effect: 
#   - effect="treated":
#       (match_strategy defaults to NULL)
#       table1 for all individuals boosted during the recruitment period
#   - effect="comparative", match_strategy="a": 
#       table1 for the matched pfizer and moderna arms using match_strategy "a"
#   - effect="incremental", match_strategy="a":  
#       table1 for the matched treated and control arms using match_strategy "a"
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

# Import libraries 
library('tidyverse')
library('lubridate')
library('here')
library('glue')
library('arrow')
library('gt')
library('gtsummary')

# import local functions and parameters
source(here("analysis", "design.R"))
source(here("analysis", "process", "process_functions.R"))
source(here("lib", "functions", "utility.R"))

# import command-line arguments 
args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  # use for interactive testing
  effect <- "incremental"
  match_strategy <- "riskscore_i"
} else {
  effect <- args[[1]]
  match_strategy <- args[[2]]
}

# save items in the match_strategy list to the global environment
list2env(
  x = get(glue("match_strategy_{match_strategy}")),
  envir = environment()
)

effect_match_strategy <- str_c(effect, match_strategy, sep = "_")

# create output directories 
output_dir <- here("output", effect_match_strategy, "table1")
fs::dir_create(output_dir)

# get data_table1
if (effect == "treated") {
  # read treated data
  data_table1 <- read_rds(here("output", "treated", "eligible", "data_treated.rds")) %>%
    rename(trial_date = vax_boostautumn_date)
} else {
  # derive data_matched
  source(here("analysis", "process", "process_postmatch.R"))
  data_table1 <- data_matched
  rm(data_matched)
}

if ("vax_lastbeforeindex_date" %in% keep_vars) {
  # derive extra variables / revel variables
  data_table1 <- data_table1 %>%
    # derive extra variables
    mutate(
      # vax_lastbeforeindex_date was a matching variable, but more meaningful 
      # to summarise as timesince_lastvax
      timesince_lastvax = as.integer(trial_date - vax_lastbeforeindex_date)
    )
}

if (effect == "treated") {
  data_table1 <- data_table1 %>% mutate(treated_descr = "All boosted")
} else {
  data_table1 <- data_table1 %>% add_descr(vars = "treated", effect = effect)
}

# table 1 style baseline characteristics ----
var_labels <- list(
  
  N  ~ "Total N",
  treated_descr ~ "Group",
  
  age ~ "Age",
  age_factor ~ "Age (per year)",
  agegroup_match ~ "Age group for matching",
  sex ~ "Sex",
  ethnicity ~ "Ethnicity",
  imd_Q5 ~ "Deprivation",
  region ~ "Region",
  
  learndis ~ "Learning disability",
  sev_mental ~ "Severe mental illness",
  immunosuppressed ~ "Immunouppressed",
  
  # multimorb ~ "Multimorbidity score",
  
  cv ~ "Clinically vulnerable",
  
  vax_primary_brand ~ "Primary course brand",
  vax_boostfirst_brand ~ "First booster brand",
  vax_boostspring_brand ~ "Spring booster brand",
  vax_boostautumn_brand ~ "Autumn booster brand",
  
  # vax_primary_date ~ "Primary course date",
  # vax_boostfirst_date ~ "First booster date",
  # vax_boostspring_date ~ "Spring booster date",
  # vax_boostautumn_date ~ "Autumn booster date",
  
  timesince_lastvax ~ "Days since last dose",
  timesince_coviddischarged ~ "Time since COVID-19 hospitalisation",
  
  flu_vaccine ~ "Flu vaccine",
  bmi ~ "Body mass index"
  
  )

var_labels <- var_labels %>%
  set_names(., map_chr(., all.vars))

# only keep variables that are in data_table1
select_var_labels <- c(
  TRUE, TRUE, 
  names(var_labels[-c(1,2)]) %in% names(data_table1)
  )

var_labels <- var_labels[select_var_labels]

# map_chr(var_labels[-c(1,2)], ~last(as.character(.)))

# use gtsummary to obtain standardized table 1 data
tab_summary_baseline <- data_table1 %>%
  mutate(
    N = 1L,
    # summarise for each year of age
    age_factor = factor(age, levels=sort(unique(age)))
  ) %>%
  select(
    treated_descr,
    any_of(names(var_labels)),
  ) %>%
  tbl_summary(
    by = treated_descr,
    label = unname(var_labels[names(.)]),
    statistic = list(N = "{N}")
  ) 

raw_stats <- tab_summary_baseline$meta_data %>%
  select(var_label, df_stats) %>%
  unnest(df_stats)

raw_stats_midpoint <- raw_stats %>%
  mutate(
    n=roundmid_any(n, threshold),
    N=roundmid_any(N, threshold),
    p=round(100*n/N,1),
    N_miss = roundmid_any(N_miss, threshold),
    N_obs = roundmid_any(N_obs, threshold),
    p_miss = round(100*N_miss/N_obs,1),
    N_nonmiss = roundmid_any(N_nonmiss, threshold),
    p_nonmiss = round(100*N_nonmiss/N_obs,1),
    var_label = factor(var_label, levels=map_chr(var_labels[-c(1,2)], ~last(as.character(.)))),
    variable_levels = replace_na(as.character(variable_levels), "")
  ) 

table1_data <- raw_stats_midpoint %>%
  rowwise() %>%
  transmute(
    var_label,
    # gt creates a column called `label` when run locally, `variable_labels` 
    # when run in opensafely (probs different versions)
    # label,
    variable_levels,
    by,
    value = glue(stat_display)
  ) %>%
  pivot_wider(
    names_from = by,
    values_from = value
  )

write_csv(table1_data, file.path(output_dir, glue("table1_{effect_match_strategy}_midpoint{threshold}.csv")))

