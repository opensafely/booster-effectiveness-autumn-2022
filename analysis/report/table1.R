# # # # # # # # # # # # # # # # # # # # #
# Purpose: describe matching results
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
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "redaction.R"))

# import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)


if(length(args)==0){
  # use for interactive testing
  cohort <- "mrna"
} else {
  cohort <- args[[1]]
}


## get cohort-specific parameters study dates and parameters ----
dates <- study_dates[[cohort]]

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("")) {
  
  ## Import released data ----
  release_dir <- ""
  
  output_dir <- here("output", release_dir, "tables")
  fs::dir_create(output_dir)
  
  raw_stats_redacted <- read_csv(fs::path(release_dir, "table1_rounded.csv"))
  
} else {
  ## create output directories ----
  
  output_dir <- here("output", cohort, "table1")
  fs::dir_create(output_dir)
  
  ## Import data and derive some variables ----
  
  data_matched <- read_rds(ghere("output", cohort, "match", "data_matched.rds")) %>%
    mutate(dayssincevax2 = as.integer(trial_date - vax2_date))
  
  data_treatedeligible_matchstatus <- read_rds(here("output", cohort, "match", "data_treatedeligible_matchstatus.rds"))
  
  # cross tab cv and cev ----
  data_matched %>%
    group_by(cv, cev) %>%
    count() %>%
    ungroup() %>%
    mutate(percent = round(100*n/sum(n),1))
  
  # table 1 style baseline characteristics ----
  
  var_labels <- list(
    N  ~ "Total N",
    
    treated_desc ~ "Status",
    
    age ~ "Age",
    age_factor ~ "Age (per year)",
    jcvi_ageband ~ "JCVI ageband",
    sex ~ "Sex",
    ethnicity ~ "Ethnicity",
    imd_Q5 ~ "Deprivation",
    region ~ "Region",
    
    bmi ~ "Body mass index",
    
    learndis ~ "Learning disability",
    sev_mental ~ "Severe mental illness",
    immunosuppressed ~ "Immunouppressed",
    
    multimorb ~ "Multimorbidity score",
    
    pregnancy ~ "Pregnancy",
    
    cev_cv ~ "Clinically vulnerability status",
    
    flu_vaccine ~ "Flu vaccine",
    
    vax3_type_descr ~ "Third dose vaccine type",
    vax12_type_descr ~ "Primary course vaccine type",
    vax12_gap ~ "Days between first and second dose",
    dayssincevax2 ~ "Days since second dose",
    
    prior_test_cat ~ "Number of SARS-CoV-2 tests during unvaccinated period",
    prior_covid_infection ~ "Prior documented SARS-CoV-2 infection",
    time_since_infection ~ "Time since last evidence of SARS-CoV-2 infection"
    
    
    
  ) %>%
    set_names(., map_chr(., all.vars))
  
  map_chr(var_labels[-c(1,2)], ~last(as.character(.)))
  
  # use gtsummary to obtain stnadardised table 1 data
  tab_summary_baseline <-
    data_matched %>%
    mutate(
      N = 1L,
      treated_desc = factor(treated, levels = as.integer(unname(recoder$treated)), names(recoder$treated)),
      # summarise for each year of age
      age_factor = factor(age, levels=sort(unique(age)))
    ) %>%
    select(
      treated_desc,
      all_of(names(var_labels)),
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
  
  write_csv(raw_stats_redacted, fs::path(output_dir, "table1_rounded.csv"))
}

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
kableExtra::save_kable(table1_review, file = fs::path(output_dir, "table1_rounded.html"))

# table for manuscript
if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("")) {

  table1_review_manuscript <- table1_data %>%
    filter(var_label != "Age (per year)") %>%
    flextable::flextable()
   # TODO formatting

}

