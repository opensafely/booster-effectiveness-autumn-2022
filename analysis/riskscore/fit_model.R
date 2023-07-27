# code for risk score

library(tidyverse)
library(here)
library(glue)

outdir <- here("output", "riskscore", "model")
fs::dir_create(outdir)

source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))

data_riskscore <- read_rds(here("output", "riskscore", "eligible", "data_riskscore.rds"))

# fit model separately in age groups
agegroup_levels <- levels(data_riskscore$agegroup_match)
for (i in seq_along(agegroup_levels)) {
  
  # dataset for model
  data_mod <- data_riskscore %>% filter(agegroup_match %in% agegroup_levels[i])
  
  # fit model
  mod <- glm(
    formula = death ~ poly(age, degree = 2) + asthma + bmi + 
      chronic_heart_disease + chronic_kidney_disease + chronic_liver_disease +
      chronic_neuro_disease + chronic_resp_disease + diabetes + immunosuppressed +
      learndis + sev_mental + timesince_discharged,
    data = data_mod,
    family = binomial("logit")
  )
  
  # get predictions
  data_mod <- data_mod %>%
    mutate(
      probability =  predict(mod, newdata = data_mod, type = "response")
      )
  
  # distribution of predictions but true outcome
  data_mod %>%
    ggplot(aes(x = probability, y = after_stat(density), colour = death)) +
    geom_freqpoly() +
    labs(x = "Predicted probability of death") +
    theme_bw()
  
  # calibration (check methods are correct)
  by_val = 0.01
  cuts <- seq(0,1,by_val)
  data_mod %>%
    mutate(
      probability_cut = cut(probability, seq(0,1,0.01), labels = FALSE)
    ) %>%
    group_by(probability_cut) %>%
    summarise(deaths = sum(death), n = n(), .groups = "drop") %>%
    mutate(
      p = deaths/n,
      se = sqrt((p*(1-p))/n),
      ll = p - qnorm(0.975)*se,
      ul = p + qnorm(0.975)*se
      ) %>%
    mutate(across(probability_cut, ~ cuts[.x] + (by_val/2))) %>%
    ggplot(aes(x = probability_cut, y = p)) +
    geom_abline(slope = 1, intercept = 0) +
    geom_errorbar(aes(ymin = ll, ymax = ul), alpha = 0.5) +
    geom_point(alpha = 0.5) +
    lims(x = c(0,1), y = c(0,1)) +
    labs(
      x = glue("Predicted probability of death (binwidth = {by_val})"),
      y = "Proportion of deaths"
      ) +
    theme_bw()
  
}



