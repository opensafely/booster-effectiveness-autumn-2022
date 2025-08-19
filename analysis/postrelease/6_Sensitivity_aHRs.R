# # # # # # # # # # # # # # # # # # # # #
# Purpose: Create table of primary and sensitivity aHRs for comparison
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

# import libraries
library(tidyverse)
library(glue)
library(here)
library(dplyr)
library(stringr)
library(readr)
library(fs)

# load functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))

# define subgroups
subgroup_levels_labels <- unlist(unname(recoder[subgroups]))

# create list of estimates for each effect and match strategy
cox_contrasts<-list()
i <- 0 
for(effect in c("incremental", "comparative")) {
  for(match_strategy in c("a", "b")) {
    i <- i + 1
    
    output_dir <-  ghere("output", "postrelease", "{effect}_{match_strategy}", "model")
    
    cox_contrasts[[i]] <- list(
      effect = effect, 
      match_strategy = match_strategy, 
      data = read_csv(fs::path(output_dir, glue("HR_restimate_output.csv"))) %>%
        mutate(!!glue("HR_{effect}_{match_strategy}") := paste0(sprintf("%5.2f", mantel_cox_hr), " (", 
                                                                 sprintf("%5.2f", hr_lower), ", ", 
                                                                 sprintf("%5.2f", hr_upper), ")"), 
                # create factors for outcome, subgroup, and subgroup level   
                outcome = fct_recoderelevel(outcome,  recoder$outcome),
                subgroup = fct_recoderelevel(subgroup, recoder$subgroups),
                subgroup_level = fct_recoderelevel(subgroup_level, subgroup_levels_labels)
          ) %>% 
        arrange(outcome, subgroup, subgroup_level) %>% 
        select(outcome, subgroup, subgroup_level, !!glue("HR_{effect}_{match_strategy}"))
    )    
  }
}

data_list <- map(cox_contrasts, "data")
merged_data <- reduce(data_list, full_join, by = c("subgroup", "outcome", "subgroup_level")) 

outdir<- ghere("output", "incremental_a", "model")
write_csv(merged_data, fs::path(outdir, glue("Table_sensitivity_Mantel_Cox_HRs.csv")))