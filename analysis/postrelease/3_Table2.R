# # # # # # # # # # # # # # # # # # # # #
# Purpose: Output table of person time, events, risk and risk difference
# # # # # # # # # # # # # # # # # # # # #

# import libraries
library(tidyverse)
library(glue)
library(here)

# load functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))

# set effect and match strategy - file run separately for each effect and match strategy
effect = "incremental" # "comparative" or "incremental"
match_strategy = "a" # "a" or "b"

# define output directory
output_dir <- ghere("output", "postrelease", "{effect}_{match_strategy}", "model")

# define subgroups
subgroup_levels_labels <- unlist(unname(recoder[subgroups]))

# load data produced in Reanalysis_KM_output.r
dat <- read_csv(fs::path(output_dir, glue("KM_restimate_wide_output.csv")))
#view(dat)

# limit to last day of follow up and prepare for output 
dat_eof <- dat %>% 
    mutate(

        # create factors for outcome, subgroup, and subgroup level   
        outcome = fct_recoderelevel(outcome,  recoder$outcome),
        subgroup = fct_recoderelevel(subgroup, recoder$subgroups),
        subgroup_level = fct_recoderelevel(subgroup_level, subgroup_levels_labels),
    

        # create string variables for risk and risk difference
        risk_0_str = paste0(sprintf("%5.2f", 1000*risk_0), " (",
                            sprintf("%5.2f", 1000*risk.ll_0), ",",
                            sprintf("%5.2f", 1000*risk.ul_0), ")"
        ),
        risk_1_str = paste0(sprintf("%5.2f", 1000*risk_1), " (",
                            sprintf("%5.2f", 1000*risk.ll_1), ",",
                            sprintf("%5.2f", 1000*risk.ul_1), ")"
        ),
        risk_diff_str = paste0(sprintf("%5.2f", 1000*rd), " (",
                               sprintf("%5.2f", 1000*rd.ll), ",",
                               sprintf("%5.2f", 1000*rd.ul), ")"
        )
    ) %>%
    group_by(outcome, subgroup, subgroup_level) %>%
    filter(time == max(time)) %>%
    ungroup() %>%
    arrange(outcome, subgroup, subgroup_level) %>%
    select(outcome, subgroup, subgroup_level, 
           cml.persontime_0, cml.event.updated_0, risk_0_str,
           cml.persontime_1, cml.event.updated_1, risk_1_str, 
            risk_diff_str
            ) 

#view(dat_eof)
# write to csv
write_csv(dat_eof, fs::path(output_dir, glue("Risk_table.csv")))


# repeat for HR estimates
HR_dat <- read_csv(fs::path(output_dir, glue("HR_restimate_output.csv")))  %>%  
  select(outcome, subgroup, subgroup_level, hr_str, period) %>%
  mutate(period_fct = "1-350 days") 
HR_dat_period <- read_csv(fs::path(output_dir, glue("HR_restimate_period_output.csv"))) %>% 
  select(outcome, subgroup, subgroup_level, hr_str, period, period_fct)  
HR_dat_all <- rbind(HR_dat, HR_dat_period)

HR_dat_all <- HR_dat_all %>% 
    mutate(
        # create factors for outcome, subgroup, and subgroup level   
        outcome = fct_recoderelevel(outcome,  recoder$outcome),
        subgroup = fct_recoderelevel(subgroup, recoder$subgroups),
        subgroup_level = fct_recoderelevel(subgroup_level, subgroup_levels_labels),
    ) %>% 
     arrange(outcome, subgroup, subgroup_level, period) %>% 
     select(outcome, subgroup, subgroup_level, period_fct, hr_str)

#view(HR_dat_all)
write_csv(HR_dat_all, fs::path(output_dir, glue("HR_table.csv")))
