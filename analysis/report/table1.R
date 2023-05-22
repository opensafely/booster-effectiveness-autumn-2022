# # # # # # # # # # # # # # # # # # # # #
# Purpose: describe match results
# creates "table 1"
# this file takes the following arguments:
# - vars: the variables to summarise
#   - vars="match" will only include the variables used in matching, so it can 
#     be run before the other covaraiets are extracted
#   - vars="covs" will only include the model covariates which are extracted at
#     a later stage
# - effect: 
#   - effect="treated" will summarise data for all individuals boosted during the recruitment period
#   - effect="comparative" will summarise data for the matched pfizer and moderna arms
#   - effect="relative" will summarise data for the matched treated and control arms
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
  vars <- "all" # "covs" "match"
  effect <- "treated"
} else {
  vars <- args[[1]]
  effect <- args[[2]]
}

# create output directories 
output_dir <- here("output", effect, "table1")
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

# derive extra variables / revel variables
if (vars %in% c("match", "all")) {
  data_table1 <- data_table1 %>%
    # derive extra variables
    mutate(
      # lastvaxbeforeindex_date was a matching variable, but more meaningful 
      # to summarise as timesincelastvax
      timesincelastvax = as.integer(trial_date - lastvaxbeforeindex_date),
      # relevelling done here to avoid rerunning actions that use process_stage
      timesincecoviddischarged = fct_rev(timesincecoviddischarged)
    )
}
# add covariates if necessary
if (vars %in% c("covs", "all")) {
  
  if (effect %in% c("treated", "comparative")) {
    data_table1 <- data_table1 %>%
      add_vars(vars = "covs", group = "treated")
  }
  
  if (effect == "relative") {
    data_table1 <- data_table1 %>%
      add_vars(vars = "covs", group = c("treated", "control"))
  }
  
}

if (effect == "treated") {
  data_table1 <- data_table1 %>% mutate(treated_descr = "All boosted")
} else {
  data_table1 <- data_table1 %>% add_descr(vars = "treated", effect = effect)
}

# table 1 style baseline characteristics ----

var_labels <- list(
  
  N  ~ "Total N",
  
  treated_descr ~ "Group"
  
  )

if (vars %in% c("match", "all")) {
  
  var_labels <- splice(
    
    var_labels,
    
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
    
    timesincelastvax ~ "Days since last dose",
    timesincecoviddischarged ~ "Time since COVID-19 hospitalisation"
    
  )
    
}

if (vars %in% c("covs", "all")) {
  
  var_labels <- splice(
    
    var_labels,
    
    flu_vaccine ~ "Flu vaccine",
    bmi ~ "Body mass index"
    
  )
  
}
  
var_labels <- var_labels %>%
  set_names(., map_chr(., all.vars))

map_chr(var_labels[-c(1,2)], ~last(as.character(.)))

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

raw_stats_redacted <- raw_stats %>%
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

write_csv(raw_stats_redacted, file.path(output_dir, glue("table1_{vars}_{effect}_rounded.csv")))

table1_data <- raw_stats_redacted %>%
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

# table to help reviewing
table1_data %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_paper() %>%
  kableExtra::kable_styling(
    full_width = FALSE
  ) %>%
  kableExtra::save_kable(file = fs::path(output_dir, glue("table1_{vars}_{effect}_rounded.html")))
