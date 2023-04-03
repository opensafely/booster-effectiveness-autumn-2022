######################################
# This script:
# binds datasets of all successfully matched individuals for reading into 
# subsequent study definitions
######################################

# Preliminaries ----

# Import libraries
library(tidyverse)
library(lubridate)
library(glue)
library(here)

# import local functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))

# create output directory
outdir <- here("output", "postmatch", "eligible")
fs::dir_create(outdir)

# read matched data for comparative and relative effectiveness
# comparative:
data_matchstatus_comparative <-read_rds(here("output", "comparative", "match", "data_matchstatus.rds"))
# relative:
data_matchstatus_relative <-read_rds(ghere("output", "matchround{n_match_rounds}", "controlactual", "match", "data_matchstatus_allrounds.rds"))

# treated and matched in ether comparative or relative matching
data_matchedtreated <- bind_rows(
  data_matchstatus_comparative %>% 
    filter(matched),
  data_matchstatus_relative %>% filter(treated == 1)
) %>%
  distinct(patient_id, trial_date)

cat("Number of duplicate IDs (should be zero):\n")
data_matchedtreated %>% 
  group_by(patient_id) %>%
  summarise(n=n(), .groups = "keep") %>% 
  filter(n>1) %>%
  nrow() %>%
  print()
  
# control and matched in relative matching:
data_matchedcontrol <- data_matchstatus_relative %>% 
  filter(treated == 0) %>%
  select(patient_id, trial_date)

# save datasets
data_matchedtreated %>%
  write_csv(file.path(outdir, "data_matchedtreated.csv.gz"))

data_matchedcontrol %>%
  write_csv(file.path(outdir, "data_matchedcontrol.csv.gz"))
