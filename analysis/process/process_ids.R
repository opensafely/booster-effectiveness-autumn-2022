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

# read data for all treated individuals
data_alltreated <- read_rds(here("output", "treated", "eligible", "data_treated.rds")) %>%
  select(patient_id, trial_date = vax_boostautumn_date)

# read data from relative martching
data_matchstatus_relative <-read_rds(ghere("output", "matchround{n_match_rounds}", "controlactual", "match", "data_matchstatus_allrounds.rds"))
  
# control and matched in relative matching:
data_matchedcontrol <- data_matchstatus_relative %>% 
  filter(treated == 0) %>%
  select(patient_id, trial_date)

# save datasets
data_alltreated %>%
  write_csv(file.path(outdir, "data_alltreated.csv.gz"))

data_matchedcontrol %>%
  write_csv(file.path(outdir, "data_matchcontrol.csv.gz"))
