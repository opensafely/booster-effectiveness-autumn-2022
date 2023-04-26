# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This script:
# combines the flowcharts into a single file for review and release
# also applies rounding
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

# create output directories
outdir <- here("output", "report", "flowchart")
fs::dir_create(outdir)

# read in flowcharts
flowcharts_rounded <- list("initial", "treated", "comparative", "relative") %>%
  map_dfr(
    ~read_csv(here("output", .x, "flowchart", "flowchart_unrounded.csv")) %>%
      mutate(stage = .x, .before=1)
      ) %>%
  group_by(stage) %>%
  flow_stats_rounded(to = threshold) %>%
  ungroup() %>%
  # remove the other statistics for matched stages
  mutate(across(
    c(n_exclude, starts_with("pct_")), 
    ~if_else(stage %in% c("comparative", "relative"), NA_real_, .x)
    )) %>%
  select(stage, crit, criteria, n, everything())

# save for release
write_csv(flowcharts_rounded, file.path(outdir, "flowchart_rounded.csv"))
