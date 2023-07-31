# code for risk score

# setup ------------------------------------------------------------------------
library(tidyverse)
library(here)
library(glue)
library(RColorBrewer)

# import command-line arguments 
args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  # use for interactive testing
  agegroup_index <- 1L
} else {
  agegroup_index <- as.integer(args[[1]])
}

outdir <- here("output", "riskscore", "model")
fs::dir_create(outdir)

source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))

data_riskscore <- read_rds(here("output", "riskscore", "eligible", "data_riskscore.rds"))

# fit model separately in age groups
agegroup_levels <- levels(data_riskscore$agegroup_match)

# dataset for model
data_mod <- data_riskscore %>% 
  filter(agegroup_match %in% agegroup_levels[agegroup_index])
rm(data_riskscore)

categorical_predictors <- c(
  "asthma", "bmi", "chronic_heart_disease", "chronic_kidney_disease",
  "chronic_liver_disease", "chronic_neuro_disease", "chronic_resp_disease",
  "diabetes", "immunosuppressed", "learndis", "sev_mental", "timesince_discharged"
)

# pre model checks -------------------------------------------------------------
# for each of the categorical predictors, make sure that all levels have at 
# least one event, and merge if not (make note of merges)
remove_vars <- character()
for (x in categorical_predictors) {
  
  if (is.logical(data_mod[[x]])) {
    levs_all <- unique(data_mod[[x]])
  } else if (is.factor(data_mod[[x]])) {
    levs_all <- levels(data_mod[[x]])
  } else {
    stop("Categorical variables should be logical of factors")
  }
  
  levs_with_events <- data_mod %>%
    group_by(!!sym(x)) %>%
    summarise(events = sum(death), .groups = "drop") %>%
    filter(events > 0) %>%
    pull(!!sym(x))
  
  check_levs <- all(as.character(levs_all) %in% as.character(levs_with_events))
  
  # if all true for to the next variable
  if (check_levs) next
  
  if (length(levs_with_events) == 1) {
    remove_vars <- c(remove_vars, x)
  } else {
    # merge
    # the only ones that can be merged are bmi and timesince_discharged
    
    
  }
  
}

# fit model --------------------------------------------------------------------
mod <- glm(
  formula = death ~ poly(age, degree = 2) + asthma + bmi + 
    chronic_heart_disease + chronic_kidney_disease + chronic_liver_disease +
    chronic_neuro_disease + chronic_resp_disease + diabetes + immunosuppressed +
    learndis + sev_mental + timesince_discharged,
  data = data_mod,
  family = binomial("logit")
)

# get predictions --------------------------------------------------------------
by_val = 0.01
cuts <- seq(0,1,by_val)
data_mod <- data_mod %>%
  mutate(
    residual = mod$residuals,
    probability =  mod$fitted.values,
    probability_cut = cut(probability, seq(0,1,0.01))#, labels = FALSE)
  )

# distribution of predictions by true outcome ----------------------------------
data_mod %>%
  ggplot(aes(x = probability, y = after_stat(density), colour = death)) +
  geom_freqpoly(binwidth = by_val) +
  labs(x = "Predicted probability of death") +
  theme_bw()
ggsave(
  filename = file.path(outdir, glue("dist_predictions_{agegroup_index}.png"))
)

# calibration plot -------------------------------------------------------------
data_calibration <- data_mod %>%
  group_by(probability_cut) %>%
  summarise(
    mean_probability = mean(probability),
    deaths = sum(death), 
    n = n(), 
    .groups = "drop"
    ) %>%
  mutate(
    p = deaths/n,
    se = sqrt((p*(1-p))/n),
    ll = p - qnorm(0.975)*se,
    ul = p + qnorm(0.975)*se
  ) 
upper_lim <- max(c(data_calibration$mean_probability, data_calibration$ul))
data_calibration %>%
  # mutate(across(probability_cut, ~ cuts[.x] + (by_val/2))) %>%
  # ggplot(aes(x = probability_cut, y = p)) +
  ggplot(aes(x = mean_probability, y = p)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.5) +
  geom_point() +
  labs(
    x = glue("Predicted probability of death (mean within bins of width {by_val})"),
    y = "True proportion of deaths"
  ) +
  theme_bw() +
  coord_cartesian(xlim = c(0,upper_lim), ylim = c(0,upper_lim)) 
ggsave(
  filename = file.path(outdir, glue("calibration_{agegroup_index}.png"))
)

# binned residual plots
# overall and categorical data
tmp_cat_data <- data_mod %>%
  select(
    x = probability, 
    x_cut = probability_cut, 
    residual, 
    all_of(categorical_predictors)
  ) %>%
  mutate(
    overall = "overall",
    x_label = "probability"
  ) %>%
  mutate(
    across(
      all_of(categorical_predictors),
      ~as.character(.x)
    )
  ) %>%
  mutate(across(x_cut, as.character)) %>%
  pivot_longer(cols = all_of(c("overall", categorical_predictors))) 
tmp_age_data <- data_mod %>%
  transmute(
    x = age,
    x_cut = as.character(age),
    residual,
    x_label = "age",
    name = "age",
    value = "age"
  ) 
binned_residuals_data <- bind_rows(tmp_cat_data, tmp_age_data) %>%
  group_by(name, value, x_cut) %>%
  summarise(
    mean_x = mean(x),
    mean_residual = mean(residual),
    sd_residual = sd(residual),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    ll_residual = mean_residual - qnorm(0.975)*sd_residual/sqrt(n),
    ul_residual = mean_residual + qnorm(0.975)*sd_residual/sqrt(n)
  ) 


# residual vs probability
binned_residuals_data %>%
  filter(name %in% "overall") %>%
  ggplot(aes(x = mean_x), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  geom_line(aes(y = ll_residual), linetype = "dashed") +
  geom_line(aes(y = ul_residual), linetype = "dashed") +
  geom_point(aes(y = mean_residual)) +
  labs(
    x = "Predicted probability of death",
    y = "Mean residual"
  ) +
  theme_bw() 
ggsave(
  filename = file.path(outdir, glue("binned_residuals_overall_{agegroup_index}.png"))
)

# residual vs probability by categorical predictor
binned_residuals_data %>%
  filter(name %in% categorical_predictors) %>%
  mutate(
    across(
      name,
      ~factor(.x, levels = categorical_predictors)
      )
    ) %>%
  mutate(
    across(
      value,
      ~ factor(.x, levels = c("FALSE", "TRUE", levels(data_mod$bmi), levels(data_mod$timesince_discharged)))
    )
    ) %>% 
  ggplot(aes(x = mean_x, colour = value), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  geom_line(aes(y = ll_residual), linetype = "dashed") +
  geom_line(aes(y = ul_residual), linetype = "dashed") +
  geom_point(aes(y = mean_residual)) +
  facet_wrap(facets = vars(name), ncol = 4) +
  guides(colour = guide_legend(title = NULL, nrow = 2, byrow = TRUE)) +
  labs(
    x = "Predicted probability of death",
    y = "Mean residual"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(
  filename = file.path(outdir, glue("binned_residuals_categorical_{agegroup_index}.png"))
)

# residual vs age
binned_residuals_data %>%
  filter(name %in% "age") %>%
  ggplot(aes(x = mean_x), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  geom_line(aes(y = ll_residual), linetype = "dashed") +
  geom_line(aes(y = ul_residual), linetype = "dashed") +
  geom_point(aes(y = mean_residual)) +
  labs(
    x = "Age",
    y = "Mean residual"
  ) +
  theme_bw() 
ggsave(
  filename = file.path(outdir, glue("binned_residuals_age_{agegroup_index}.png"))
)

object.size(mod)

# remove unneeded items from mod to reduce storage requirement
print(glue("Original object size: {object.size(mod)}"))
remove_items <- c(
  "data", "model", "fitted.values", "residuals", "effects", 
  "linear.predictors", "weights", "prior.weights", "y"
)
mod[remove_items] <- NULL
print(glue("Reduced object size: {object.size(mod)}"))

# save model
write_rds(
  mod, 
  file.path(outdir, glue("model_agegroup_{agegroup_index}.rds")),
  compress = "gz"
)
