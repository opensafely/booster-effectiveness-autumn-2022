######################################
# This script:
# - fits a model to predict the risk score one of the three age groups 
#   (specified by the agegroup_index argument)
# - runs some model diagnostics
# - saves the models

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

source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))

# save items in the match_strategy list to the global environment
match_strategy <- "riskscore_i"
list2env(
  x = get(glue("match_strategy_{match_strategy}")),
  envir = environment()
)

outdir <- ghere("output", "riskscore_i", "agegroup_{agegroup_index}")
fs::dir_create(outdir)

data_riskscore <- read_rds(here("output", "riskscore_i", "eligible", "data_riskscore_i.rds"))

# fit model separately in age groups
agegroup_levels <- levels(data_riskscore$agegroup_match)

# dataset for model
data_mod <- data_riskscore %>% 
  filter(agegroup_match %in% agegroup_levels[agegroup_index])
rm(data_riskscore)

# age currently the only continuous predictor, need to update this if that changes
categorical_predictors <- riskscore_vars[!(riskscore_vars %in% "age")]

# pre model checks -------------------------------------------------------------
# for each of the categorical predictors:

# make sure they are in the data and are encoded as logical or factors
check_categorical_vars <- data_mod %>% 
  select(all_of(categorical_predictors)) %>%
  select(where(~!(is.factor(.x)|is.logical(.x))))
if (length(check_categorical_vars) > 0) {
  stop(
    "All categorical variables must be factor or logical type, fix the following:\n",
    map_chr(
      names(check_categorical_vars), 
      ~ str_c(.x, ": ", class(check_categorical_vars[[.x]]), "\n")
    )
    )
}

# make sure that all levels have at least one event, and merge if not 
# (make note of merges)
remove_vars <- character()
for (x in categorical_predictors) {
  
  if (is.logical(data_mod[[x]])) {
    levs_all <- unique(data_mod[[x]])
  } 
  if (is.factor(data_mod[[x]])) {
    levs_all <- levels(data_mod[[x]])
  } 
  levs_all <- as.character(levs_all)
  
  levs_with_events <- data_mod %>%
    group_by(!!sym(x)) %>%
    summarise(events = sum(death), .groups = "drop") %>%
    filter(events > 0) %>%
    pull(!!sym(x)) %>%
    as.character()
  
  check_levs <- all(levs_all %in% levs_with_events)
  
  # if all true for to the next value of x
  if (check_levs) next
  # if not, remove the vairable
  if (!check_levs) remove_vars <- c(remove_vars, x)
  # NOTE: the code below merges levels, but this was too complicated when
  # using the model to make predictions in other cohorts, so for now I've 
  # commented it out. If anyone has time/can be bothered they could save the
  # new levels and derive them in process_stage.R when they get the model
  # predictions for matching.
  # 
  # if (length(levs_with_events) == 1) {
  #   # if only one level with events, flag to remove variable
  #   remove_vars <- c(remove_vars, x)
  #   next
  # } 
  # 
  # print(glue("Variable: {x}"))
  # print(glue("   Original levels: ", str_c(levs_all, collapse = "; ")))
  # while(length(levs_with_events) < length(levs_all)) {
  #   
  #   # merge up, unless top level, in which case merge down
  #   i <- which(!(levs_all %in% levs_with_events))
  #   shift <- if_else(i < length(levs_all), 1, -1)
  #   new_levs <- levs_all
  #   new_levs[i+shift] <- str_c(new_levs[sort(c(i, i+shift))], collapse = " & ")
  #   # do merging
  #   data_mod <- data_mod %>%
  #     mutate(
  #       across(
  #         all_of(x), 
  #         ~factor(
  #           if_else(
  #             .x %in% levs_all[c(i, i+shift)], 
  #             new_levs[i+shift], 
  #             as.character(.x)
  #           ),
  #           levels = new_levs[-i]
  #         )
  #       )
  #     )  
  #   levs_all <- new_levs[-i]
  #   levs_with_events <- data_mod %>%
  #     group_by(!!sym(x)) %>%
  #     summarise(events = sum(death), .groups = "drop") %>%
  #     filter(events > 0) %>%
  #     pull(!!sym(x)) %>%
  #     as.character()
  #   
  # }
  # print(glue("   New levels: ", str_c(levs_all, collapse = "; ")))
  
}

print(glue("Removed variables: ", str_c(remove_vars, collapse = ", ")))
if (length(remove_vars) > 0) {
  categorical_predictors <- categorical_predictors[-which(categorical_predictors %in% remove_vars)]
}

# fit model --------------------------------------------------------------------
mod_formula <- as.formula(
  str_c(
    "death ~ ", str_c(categorical_predictors, collapse = " + ")
  )
)

# need to update this if any more continuous predictors added
if ("age" %in% riskscore_vars) {
  mod_formula <- mod_formula %>% update.formula(. ~ . + poly(age, degree = 2)) 
}

mod <- glm(
  formula = mod_formula,
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
    probability_cut = cut(probability, seq(0,1,0.01))
  )

# get breaks for percentiles of predicted probabilities of death
percentile_breaks <- quantile(
  data_mod$probability,
  probs = seq(0,1,0.01)
)
percentile_breaks_unique <- unique(percentile_breaks)
n_breaks <- length(percentile_breaks)
n_breaks_unique <- length(percentile_breaks_unique)
print(glue("{n_breaks_unique} of {n_breaks} percentile breaks are unique."))

# replace bottom with zero and top with 1 to ensure all predictions captured in other datasets
percentile_breaks_unique[1] <- 0
percentile_breaks_unique[length(percentile_breaks_unique)] <- 1

write_rds(
  percentile_breaks_unique,
  file.path(outdir, glue("percentile_breaks_{agegroup_index}.rds"))
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
