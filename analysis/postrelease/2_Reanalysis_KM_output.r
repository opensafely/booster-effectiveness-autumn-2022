# # # # # # # # # # # # # # # # # # # # #
# Purpose: Reanalyse KM output to remove events that occurred on day 0
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

# import libraries
library(tidyverse)
library(glue)
library(here)

# load functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))

# set effect and match strategy - file run separately for each effect and match strategy
effect = "comparative" # "comparative" or "incremental"
match_strategy = "a" # "a" or "b"

# define output directory
output_dir <- ghere("output", "postrelease", "{effect}_{match_strategy}", "model")

# metaparams for all models that have been run for the given effect
model_args <- model_args[model_args$effect == effect,]

# get all subgroups
subgroups <- model_args %>% distinct(subgroup) %>% pull() 

# define periods for when the outcome occurs 
period_intervals <- c(1, 14, 70, 126, 182, 238, 294, 350)

# load cuminc dataset 
km_estimates_rounded <- read_csv(fs::path(output_dir, glue("km_estimates_midpoint6.csv"))) 
    
# re-estimate survival and cumulative incidence estimates    
km_restimated <- km_estimates_rounded %>%
  # remove estimates already made 
  select(effect, model, subgroup, outcome, filename, subgroup_level, 
         treated, time, lagtime, leadtime, interval, cml.event, cml.censor,  
         n.entry, n.risk, n.event) %>% 
  # group by subgroup, outcome and treated 
  group_by(subgroup_level, outcome, treated) %>%
  # save original cumulative estimates from day 0-1
  mutate(
    event_day0_1 = min(cml.event, na.rm = TRUE),
    censor_day0_1 = min(cml.censor, na.rm = TRUE),
    cml.event.updated = cml.event - event_day0_1,
    cml.censor.updated = cml.censor - censor_day0_1
  ) %>%
  # remove events that occurred on day 0-1
  filter(!lagtime == 0) %>%
  # recalclulate estimates
  mutate( 

    N = max(n.risk, na.rm = TRUE),
    

    # KM estimate for event of interest, combining censored and competing events as censored
    summand = (1/(n.risk-n.event)) - (1/n.risk), # = n.event / ((n.risk - n.event) * n.risk) but re-written to prevent integer overflow
    surv = cumprod(1 - n.event / n.risk),
    surv.se = surv * sqrt(cumsum(summand)), # Greenwood's formula
    surv.ln.se = surv.se/surv,

     ## standard errors on log scale
    #surv.ll = exp(log(surv) + qnorm(0.025)*surv.ln.se),
    #surv.ul = exp(log(surv) + qnorm(0.975)*surv.ln.se),
    
    llsurv = log(-log(surv)),
    llsurv.se = sqrt((1 / log(surv)^2) * cumsum(summand)),
    
    ## standard errors on complementary log-log scale
    surv.ll = exp(-exp(llsurv + qnorm(0.975)*llsurv.se)),
    surv.ul = exp(-exp(llsurv + qnorm(0.025)*llsurv.se)),
    
    risk = 1 - surv,
    risk.se = surv.se,
    risk.ln.se = surv.ln.se,
    risk.ll = 1 - surv.ul,
    risk.ul = 1 - surv.ll,
    
    # cumulative person time in years
    persontime_interval = (n.risk * interval)/365.25, 
    cml.persontime = round(cumsum(persontime_interval)),

    # identify the period 
    period = case_when(
      lagtime >= period_intervals[1] & time <= period_intervals[2] ~ 1,
      lagtime >= period_intervals[2] & time <= period_intervals[3] ~ 2,
      lagtime >= period_intervals[3] & time <= period_intervals[4] ~ 3,
      lagtime >= period_intervals[4] & time <= period_intervals[5] ~ 4,
      lagtime >= period_intervals[5] & time <= period_intervals[6] ~ 5,
      lagtime >= period_intervals[6] & time <= period_intervals[7] ~ 6,
      lagtime >= period_intervals[7] & time <= period_intervals[8] ~ 7,
      TRUE ~ NA_real_
    ), 

    period_fct = factor(period,
    levels = 1:(length(period_intervals)-1),
    labels = paste0(period_intervals[1:(length(period_intervals)-1)], "-" , period_intervals[2:length(period_intervals)], " days"),
    )
  ) %>%
  relocate(lagtime, .before = time) %>%
  relocate(period, .after = time) %>%
  relocate(period_fct, .after = time) %>%
  ungroup() 

#view(km_restimated)
write_csv(km_restimated, fs::path(output_dir, glue("KM_restimate_output.csv")))

# convert to wide format 
km_restimated_wide <- km_restimated %>%
  pivot_wider(id_cols = all_of(c("subgroup", "subgroup_level", "outcome", "time", "lagtime", "leadtime", "period_fct", "period")),
              names_from = treated,
              names_glue="{.value}_{treated}",
              values_from = c(cml.event.updated, cml.censor.updated,
                              n.entry, n.risk, n.event,
                              risk, risk.se, risk.ln.se,
                              risk.ll, risk.ul,
                              surv, surv.se, surv.ln.se,
                              surv.ll, surv.ul,
                              persontime_interval, cml.persontime)
  ) %>%
    mutate(
      
      n.nonevent_0 = n.risk_0 - n.event_0,
      n.nonevent_1 = n.risk_1 - n.event_1,

      # risk difference, standard error and confidence limits, using delta method
      rd = risk_1 - risk_0,
      rd.se = sqrt( (risk.se_0^2) + (risk.se_1^2) ),
      rd.ll = rd + qnorm(0.025)*rd.se,
      rd.ul = rd + qnorm(0.975)*rd.se,

    # values needed to calculate Mantel-Cox (log-rank) hazard ratio
      # Kirkwood and Sterne (2003) page 283 - Section 26.5 Comparison of Hazards Using Mantel-Cox Methods: The Log Rank Test
    q = (n.event_1 * (n.risk_0 - n.event_0)) / (n.risk_0 + n.risk_1),
    r = (n.event_0 * (n.risk_1 - n.event_1)) / (n.risk_0 + n.risk_1),
    v = ((n.event_0 + n.event_1) * n.risk_0 * n.risk_1) / (n.risk_0 + n.risk_1)^2,
    )

#view(km_restimated_wide)
write_csv(km_restimated_wide, fs::path(output_dir, glue("KM_restimate_wide_output.csv")))

# calculate Mantel-Cox (log-rank) hazard ratio
HR_restimate <- km_restimated_wide %>%
  group_by(subgroup, subgroup_level, outcome) %>%
  summarise(
    Q = sum(q, na.rm = TRUE),
    R = sum(r, na.rm = TRUE),
    V = sum(v, na.rm = TRUE),
  ) %>%
  mutate(
    mantel_cox_hr = Q/R,
    se_log_hr = sqrt(V/(Q*R)),
    hr_lower = exp(log(mantel_cox_hr) + qnorm(0.025) * se_log_hr),
    hr_upper = exp(log(mantel_cox_hr) + qnorm(0.975) * se_log_hr), 
    hr_str = paste0(sprintf("%5.2f", mantel_cox_hr), " (",
                    sprintf("%5.2f", hr_lower), ",",
                    sprintf("%5.2f", hr_upper), ")"),
    period = 0
  ) %>% arrange(outcome, subgroup, subgroup_level) %>%
  ungroup() 
#view(HR_restimate)
write_csv(HR_restimate, fs::path(output_dir, glue("HR_restimate_output.csv")))


# Estimate HR for each period
HR_restimate_period <- km_restimated_wide %>%
  group_by(subgroup, subgroup_level, outcome, period, period_fct) %>%
  summarise(
    Q = sum(q, na.rm = TRUE),
    R = sum(r, na.rm = TRUE),
    V = sum(v, na.rm = TRUE),
  ) %>%
  mutate(
    mantel_cox_hr = Q/R,
    se_log_hr = sqrt(V/(Q*R)),
    hr_lower = exp(log(mantel_cox_hr) + qnorm(0.025) * se_log_hr),
    hr_upper = exp(log(mantel_cox_hr) + qnorm(0.975) * se_log_hr), 
    hr_str = paste0(sprintf("%5.2f", mantel_cox_hr), " (",
                sprintf("%5.2f", hr_lower), ",",
                sprintf("%5.2f", hr_upper), ")"),
  ) %>% arrange(outcome, subgroup, subgroup_level, period_fct) %>%
  ungroup() 
#view(HR_restimate_period)
write_csv(HR_restimate_period, fs::path(output_dir, glue("HR_restimate_period_output.csv")))
