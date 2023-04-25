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
source(here("analysis", "process", "process_functions.R"))

# import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  # use for interactive testing
  effect <- "comparative"
  
} else {
  effect <- args[[1]]
}

# create output directories
outdir <- here("output", effect, "flowchart")
fs::dir_create(outdir)

# read data
if (effect == "comparative") {
  
  data_matchstatus <- readr::read_rds(here("output", "comparative", "match", "data_matchstatus.rds")) %>%
    select(patient_id, treated, matched, trial_date)
  
  flowchart_comparative <- data_matchstatus %>%
    add_descr(vars = "treated", effect = effect, remove = TRUE) %>%
    group_by(treated_descr, matched) %>%
    count() %>%
    ungroup(matched) %>%
    mutate(treated_n = sum(n)) %>%
    ungroup() %>%
    mutate(total_n = sum(n)) 
  
  # TODO
  
}

if (effect == "relative") {
  
  data_matchstatus_allrounds <- read_rds(ghere("output", "matchround{n_match_rounds}", "controlactual", "match", "data_matchstatus_allrounds.rds"))
  
  data_matchstatus <- data_matchstatus_allrounds %>%
    mutate(matched = !is.na(match_id)) %>% # always TRUE
    select(patient_id, treated, matched) %>%
    pivot_wider(
      names_from = treated,
      values_from = matched
    ) %>%
    rename("treated" = "1", "control" = "0") %>%
    # include all aligible individuals
    full_join(
      read_rds(here("output", "initial", "eligible", "data_vax.rds")) %>%
        select(patient_id, age),
      by = "patient_id"
    ) %>%
    # get vax_boostautumn_date from treated and eligible individuals
    left_join(
      read_rds(here("output", "treated", "eligible", "data_treated.rds")) %>%
        select(patient_id, vax_boostautumn_date),
      by = "patient_id"
    ) %>%
    mutate(across(c(treated, control), ~ replace_na(as.logical(.x), replace=FALSE))) 
  
  # categorise individuals
  data_match_flow  <- data_matchstatus %>%
    mutate(
      start_date = if_else(
        age <= 64,
        study_dates$boosterautumn[["ages50to64"]], 
        study_dates$boosterautumn[["ages65plus"]]
      )
    ) %>%
    mutate(
      crit = case_when(
        # those who are vaccinated on day 1 of recruitment
        vax_boostautumn_date == start_date & !control & !treated ~ "A",
        vax_boostautumn_date == start_date & !control & treated ~ "B",
        # those who are vaccinated during the recruitment period but not on day 1
        vax_boostautumn_date <= study_dates$studyend & !control & !treated ~ "C",
        vax_boostautumn_date <= study_dates$studyend & !control & treated ~ "D",
        vax_boostautumn_date <= study_dates$studyend & control & !treated ~ "E",
        vax_boostautumn_date <= study_dates$studyend & control & treated ~ "F",
        # those remain unvaccinated at end of recruitment period
        control ~ "H",
        !control ~ "I",
        TRUE ~ NA_character_
      )
    )
  
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
    "H", "unboosted and matched",
    "I", "unboosted and unmatched"
  ) 
  
  # define boxes for sequential trial flow
  flow_boxes <- tribble(
    ~box_crit, ~box_descr,
    "ABCDEF", "Boosted during recruitment period",
    "AC", "Boosted during recruitment period, unmatched as treated, unmatched as control",
    "BDF", "Boosted during recruitment period, matched as treated",
    "E", "Boosted during recruitment period, matched as treated, matched as control",
    "F", "Boosted during recruitment period, matched as treated, matched as control",
    "EFH", "Boosted during recruitment period, matched as control",
    "I", "Unboosted up to recruitment end, unmatched as control",
  )
  
  # count number in each category
  flowchart_match <- data_match_flow %>% group_by(crit) %>% count() %>%
    right_join(flow_categories, by = "crit") %>%
    arrange(crit) %>% 
    mutate(across(n, ~if_else(is.na(.x), 0L, .x))) 
  
  # boxes
  flow_match_final <- flow_boxes %>%
    # join to the counts for each criteria
    fuzzyjoin::fuzzy_join(
      flowchart_match, 
      by = c("box_crit" = "crit"), 
      match_fun = str_detect,
      mode = "left"
    ) %>%
    # sum across all criteria in each box
    group_by(box_crit, box_descr) %>%
    summarise(n = sum(n), .groups = "keep")  %>%
    ungroup() %>%
    rename(
      # rename to match flowcharttreatedeligible
      criteria = box_descr,
      crit = box_crit
    )
  # relative matching flowchart all good up to this point
  
}

# TODO
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
