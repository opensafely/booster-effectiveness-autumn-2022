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
  
  # load model
  mod <- read_rds(file.path(outdir, glue("model_agegroup_{i}.rds")))
  
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
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.5) +
    geom_point() +
    lims(x = c(0,1), y = c(0,1)) +
    labs(
      x = glue("Predicted probability of death (binwidth = {by_val})"),
      y = "True proportion of deaths"
    ) +
    theme_bw() 
  
   
}