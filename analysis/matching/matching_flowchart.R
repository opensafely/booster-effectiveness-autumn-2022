# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This script:
# calculates the counts for the flowchart in the manuscript
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Preliminaries ----

# import libraries
library(tidyverse)
library(glue)
library(here)

# load functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "fuzzy_join.R"))

# import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  # use for interactive testing
  cohort <- "mrna"
  
} else {
  cohort <- args[[1]]
}

# create output directories
outdir <- here("output", cohort, "flowchart")
fs::dir_create(outdir)

# define all flow categories for sequential trial cohorts
flow_categories <- tribble(
  ~crit, ~criteria,
  # those who are boosted with the brand on day 1 of recruitment
  "A", "boosted and unmatched",
  "B", "boosted and matched",
  # those who are boosted with the brand during the recruitment period but not on day 1
  "C", "unboosted and unmatched then boosted and unmatched",
  "D", "unboosted and unmatched then boosted and matched",
  "E", "unboosted and matched then boosted and unmatched",
  "F", "unboosted and matched then boosted and matched",
  # those who remain unboosted at the end of the recruitment period
  "H", "unboosted and matched",
) 

# define boxes for sequential trial flow
flow_boxes <- tribble(
  ~box_crit, ~box_descr,
  "ABCDEF", "Boosted during recruitment period",
  "AC", "Boosted during recruitment period, unmatched as treated, unmatched as control",
  "BDF", "Boosted during recruitment period, matched as treated",
  "E", "Boosted during recruitment period, matched as treated, matched as control",
  "F", "Boosted during recruitment period, matched as treated, matched as control",
  "EFH", "Unboosted, matched as control",
  "H", "Unboosted up to recruitment end, matched as control",
)

# read data_treatedeligible_matchstatus to count unmatched treated individuals (we don't count unmatched controls)
data_treatedeligible_matchstatus <- readr::read_rds(here("output", cohort, "match", "data_treatedeligible_matchstatus.rds")) %>%
  select(patient_id, treated, matched, vax3_date)

# reshape so one row per patient, and logical columns to indicate if matched as treated, control or both
data_matched <- readr::read_rds(here("output", cohort, "match", "data_matched.rds")) %>%
  select(patient_id, treated) %>%
  mutate(matched = 1) %>%
  pivot_wider(
    names_from = treated,
    values_from = matched
  ) %>%
  rename("treated" = "1", "control" = "0") %>%
  full_join(data_treatedeligible_matchstatus, by = c("patient_id", "treated")) %>%
  mutate(across(c(treated, control, matched), ~ replace_na(as.logical(.x), replace=FALSE))) %>%
  mutate(treated=matched) %>%
  select(-matched)

cat("Check there are the same number of treated and control:\n")
data_matched %>%
  summarise(
    treated = sum(treated, na.rm = TRUE),
    control = sum(control, na.rm = TRUE)
  ) %>%
  print()

# categorise individuals
data_match_flow  <- data_matched  %>%
  mutate(
    crit = case_when(
      # those who are vaccinated on day 1 of recruitment
      vax3_date == study_dates[[cohort]]$start_date & !control & !treated ~ "A",
      vax3_date == study_dates[[cohort]]$start_date & !control & treated ~ "B",
      # those who are vaccinated during the recruitment period but not on day 1
      vax3_date <= study_dates$studyend_date & !control & !treated ~ "C",
      vax3_date <= study_dates$studyend_date & !control & treated ~ "D",
      vax3_date <= study_dates$studyend_date & control & !treated ~ "E",
      vax3_date <= study_dates$studyend_date & control & treated ~ "F",
      # those who remain unvaccinated at end of recruitment period
      TRUE ~ "H"
    )
  )

# count number in each category
flowchart_matching <- data_match_flow %>% group_by(crit) %>% count() %>%
  right_join(flow_categories, by = "crit") %>%
  arrange(crit) %>% 
  mutate(across(n, ~if_else(is.na(.x), 0L, .x))) 

# brand-specific
flow_match_final <- flow_boxes %>%
  # join to the counts for each criteria
  fuzzy_join(
    flowchart_matching, 
    by = c("box_crit" = "crit"), 
    match_fun = str_detect,
    mode = "left"
  ) %>%
  # sum across all criteria in each box
  group_by(box_crit, box_descr) %>%
  summarise(n = sum(n), .groups = "keep")  %>%
  ungroup() %>%
  mutate(across(n, roundmid_any, to=threshold)) %>%
  rename(
    # rename to match flowcharttreatedeligible
    criteria = box_descr,
    crit = box_crit
  )

flowchart_final_rounded <- bind_rows(
  # read unrounded as rounding different (using ceiling_any in process_data.R)
  read_rds(here("output", "treated", "eligible", "flowchart_treatedeligible.rds"))  %>%
    # round to match flow_match_final
    transmute(
      criteria, crit, 
      n = roundmid_any(n, to=threshold),
      n_exclude = lag(n) - n,
      pct_exclude = n_exclude/lag(n),
      pct_all = n / first(n),
      pct_step = n / lag(n),
    ) %>%
    mutate(across(starts_with("pct_"), round, 3)),
  flow_match_final
) %>%
  # for easy review, join back after release
  select(-criteria) %>%
  select(crit, everything())

# save flowchart_final  
write_csv(
  flowchart_final_rounded,
  file.path(outdir, "flowchart_final_rounded.csv")
)
