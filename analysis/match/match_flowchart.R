# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This script:
# calculates the counts for the flowchart in the matching stage
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
  match_strategy <- "a"
  
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
outdir <- here("output", effect_match_strategy, "flowchart")
fs::dir_create(outdir)

# read data
if (effect == "comparative") {
  
  data_matchstatus <- 
    readr::read_rds(here("output", effect_match_strategy, "match", "data_matchstatus.rds")) %>%
    select(patient_id, treated, matched, trial_date)
  
  flowchart_final <- data_matchstatus %>%
    add_descr(vars = "treated", effect = effect, remove = TRUE) %>%
    group_by(treated_descr, matched) %>%
    count(name = "n_matched") %>%
    ungroup(matched) %>%
    mutate(n_treated = sum(n_matched)) %>%
    ungroup() %>%
    mutate(n_total = sum(n_matched)) %>%
    pivot_longer(cols = starts_with("n_")) %>%
    mutate(
      criteria = case_when(
        name %in% "n_total" 
        ~ "A: Boosted and eligible",
        name %in% "n_treated" ~ str_c("B: Boosted with ", treated_descr),
        name %in% "n_matched" & !matched ~ str_c("C: Boosted with ", treated_descr, " and unmatched"),
        name %in% "n_matched" & matched ~ str_c("D: Boosted with ", treated_descr, " and matched"),
      )
    ) %>%
    select(criteria, n=value) %>%
    distinct() %>%
    arrange(criteria) %>%
    mutate(across(criteria, ~str_remove(.x, "\\w\\:\\s")))
  
}

if (effect == "incremental") {
  
  # derive data_matched
  source(here("analysis", "process", "process_postmatch.R"))
  
  # get data from all patients who meet initial eligibility criteria
  data_initial <- 
    read_csv(here("output", "initial", "eligible", "data_eligible.csv.gz")) %>%
    select(patient_id) %>%
    left_join(
      read_rds(here("output", "initial", "processed", "data_vax.rds")) %>%
        select(patient_id, age, vax_boostautumn_date),
      by = "patient_id"
    )
  
  # get matached data and reshape to wide so 1 row = 1 matchde pair
  data_matched_wide <- data_matched %>%
    mutate(matched = !is.na(match_id)) %>% # always TRUE
    select(patient_id, treated, matched) %>%
    pivot_wider(
      names_from = treated,
      values_from = matched
    ) %>%
    rename("treated" = "1", "control" = "0")
  
  data_matchstatus <- data_initial %>%
    # include all eligible individuals
    left_join(data_matched_wide, by = "patient_id") %>%
    mutate(across(c(treated, control), ~ replace_na(as.logical(.x), replace=FALSE))) 
  
  # tidy up
  rm(data_initial, data_matched, data_matched_wide)
  
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
        # those who are vaccinated on day 1 of recruitment (can only be treated)
        vax_boostautumn_date == start_date & !control & !treated ~ "A",
        vax_boostautumn_date == start_date & !control & treated ~ "B",
        # those who are vaccinated during the recruitment period but not on day 1
        # (can be treated and/or control)
        vax_boostautumn_date <= study_dates$recruitmentend & !control & !treated ~ "C",
        vax_boostautumn_date <= study_dates$recruitmentend & !control & treated ~ "D",
        vax_boostautumn_date <= study_dates$recruitmentend & control & !treated ~ "E",
        vax_boostautumn_date <= study_dates$recruitmentend & control & treated ~ "F",
        # those remain unvaccinated at end of recruitment period (can only be control)
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
    "ABCDEF", "Boosted and eligible",
    "AC", "Boosted during recruitment period, unmatched as treated, unmatched as control",
    "BDF", "Boosted during recruitment period, matched as treated",
    "E", "Boosted during recruitment period, matched as treated, matched as control",
    "F", "Boosted during recruitment period, matched as treated, matched as control",
    "EFH", "Boosted during recruitment period, matched as control",
    "I", "Unboosted up to recruitment end, unmatched as control",
  )
  
  # count number in each category
  flowchart <- data_match_flow %>% group_by(crit) %>% count() %>%
    right_join(flow_categories, by = "crit") %>%
    arrange(crit) %>% 
    mutate(across(n, ~if_else(is.na(.x), 0L, .x))) 
  
  # boxes
  flowchart_final <- flow_boxes %>%
    # join to the counts for each criteria
    fuzzyjoin::fuzzy_join(
      flowchart, 
      by = c("box_crit" = "crit"), 
      match_fun = str_detect,
      mode = "left"
    ) %>%
    # sum across all criteria in each box
    group_by(box_crit, box_descr) %>%
    summarise(n = sum(n), .groups = "keep")  %>%
    ungroup() %>%
    rename(
      # rename to match inital and treated flowcharts
      criteria = box_descr,
      crit = box_crit
    )
  
}

# save flowchart_match
write_csv(
  flowchart_final,
  file.path(outdir, "flowchart_unrounded.csv")
)

flowchart_final %>%
  mutate(across(n, ~roundmid_any(n, threshold))) %>%
  write_csv(
    file.path(outdir, glue("flowchart_midpoint{threshold}.csv"))
  )
