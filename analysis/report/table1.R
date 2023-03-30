# # # # # # # # # # # # # # # # # # # # #
# Purpose: describe match results
# creates "table 1"
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----
## Import libraries ----
library('tidyverse')
library('lubridate')
library('here')
library('glue')
library('arrow')
library('gt')
library('gtsummary')

## import local functions and parameters ---

source(here("analysis", "design.R"))
source(here("analysis", "process", "process_functions.R"))
source(here("lib", "functions", "utility.R"))
# source(here("lib", "functions", "redaction.R"))

# import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  vars <- "match" # "covs" "match
  effect <- "comparative"
  # stage <- "final" 
} else {
  vars <- args[[1]]
  effect <- args[[2]]
}

## create output directories ----

output_dir <- here("output", effect, "table1")
fs::dir_create(output_dir)

# derive data_stage
source(here("analysis", "process", "process_postmatch.R"))

# table 1 style baseline characteristics ----

var_labels <- list(
  
  N  ~ "Total N",
  
  treated_desc ~ "Status"
  
  )

if (vars == "match") {
  
  var_labels <- splice(
    
    var_labels,
    
    age ~ "Age",
    age_factor ~ "Age (per year)",
    agegroup_match ~ "Age group for match",
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
    
    timesincelastvax ~ "Days since last dose"
    
  )
    
}

if (vars == "covs") {
  
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
tab_summary_baseline <- data_stage %>%
  mutate(
    N = 1L,
    treated_desc = factor(treated, levels = as.integer(unname(recoder[[effect]])), names(recoder[[effect]])),
    # summarise for each year of age
    age_factor = factor(age, levels=sort(unique(age)))
  ) %>%
  select(
    treated_desc,
    any_of(names(var_labels)),
  ) %>%
  tbl_summary(
    by = treated_desc,
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

write_csv(raw_stats_redacted, fs::path(output_dir, glue("table1_{vars}_{effect}_rounded.csv")))

table1_data <- raw_stats_redacted %>%
  rowwise() %>%
  transmute(
    var_label,
    # gt creates a column called `label` when run locally, `variable_labels` when run in opensafely (probs different versions)
    # label,
    variable_levels,
    by,
    value = glue(stat_display)
  ) %>%
  pivot_wider(
    names_from = by,
    values_from = value
  )

table1_review <- table1_data %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_paper() %>%
  kableExtra::kable_styling(
    full_width = FALSE
  )

# table to help reviewing
kableExtra::save_kable(table1_review, file = fs::path(output_dir, glue("table1_{vars}_{effect}_rounded.html")))
