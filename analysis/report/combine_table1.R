# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This script:
# combines the table1s into a single file for review and release
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

table1_combined <- expand_grid(
  effect = c("comparative", "relative"),
  vars = c("match", "covs")
) %>%
  pmap_dfr(
    function(effect, vars) 
      read_csv(here("output", effect, "table1", glue("table1_", vars, "_", effect, "_rounded.csv")))
  )

variable_order <- unique(table1_combined$variable)
by_order <- names(c(recoder$relative, recoder$comparative))

table1_combined_final <- table1_combined %>%
  # age_factor just for checking matching, don't need to release
  filter(variable != "age_factor") %>%
  mutate(across(by, factor, levels = by_order)) %>%
  mutate(across(variable, factor, levels = variable_order)) %>%
  arrange(variable, by) %>%
  mutate(across(starts_with("N"), ~scales::comma(.x, accuracy=1))) %>%
  rowwise() %>%
  mutate(across(stat_display, glue)) %>%
  select(var_label, variable_levels, by, stat_display) %>%
  # group counts read in twice
  distinct() %>%
  pivot_wider(names_from = by, values_from = stat_display)

# TODO save output
