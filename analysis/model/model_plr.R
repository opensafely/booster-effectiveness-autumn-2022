
# # # # # # # # # # # # # # # # # # # # #
# This script:
# imports processed data and restricts it to patients in "cohort"
# fits some pooled logistic regression models, with different adjustment sets
#
# The script should be run via an action in the project.yaml
# The script must be accompanied by four arguments,
# `effect`: comparative, incremental
# `match_strategy`: a, b, riskscore_i
# `subgroup`: all, age groups
# `outcome` : the dependent variable in the regression model
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----
## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')
library('splines')
library('parglm')
library('sandwich')
## Import custom user functions from lib
source(here("analysis", "design.R"))
source(here("analysis", "process", "process_functions.R"))
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "redaction.R"))
source(here("lib", "functions", "survival.R"))

# import command-line arguments ----
args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  # use for interactive testing
  removeobs <- FALSE
  effect <- "comparative"
  match_strategy <- "a" 
  subgroup <- "all" 
  outcome <- "covidadmitted"
} else {
  removeobs <- TRUE
  effect <- args[[1]]
  match_strategy <- args[[2]]
  subgroup <- args[[3]]
  outcome <- args[[4]]
}

timescale <- "timesincevax" # running all models on the time since vaccination scale 


# save items in the match_strategy list to the global environment
list2env(
  x = get(glue("match_strategy_{match_strategy}")),
  envir = environment()
)

# derive symbolic arguments for programming with
subgroup_sym <- sym(subgroup)

# create output directories ----
output_dir <- ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome)
fs::dir_create(output_dir)


## create special log file ----
cat(glue("## script info for {outcome} ##"), "  \n", 
    file = ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome, glue("modelplr_log_{outcome}.txt")), append = FALSE)
## function to pass additional log text
logoutput <- function(...){
  cat(...,  file = ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome, glue("modelplr_log_{outcome}.txt")), sep = "\n  ", append = TRUE)
  cat("\n", file = ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome, glue("modelplr_log_{outcome}.txt")), sep = "\n  ", append = TRUE)
}


# read and process data_matched ----
read_final <- TRUE
source(here("analysis", "process", "process_postmatch.R"))
model <- "plr"
source(here("analysis", "process", "process_premodel.R"))


### print dataset size and save ----
logoutput(
  glue("data_plr data size = ", nrow(data_plr)),
  glue("data_plr memory usage = ", format(object.size(data_plr), units="GB", standard="SI", digits=3L))
)

write_rds(data_plr, ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome, glue("modelplr_{outcome}_data.rds")), compress="gz")

## make formulas ----

### model 0 - vaccination + timescale only, no adjustment variables
### model 1 - minimally adjusted vaccination effect model, stratification by region only
### model 2 - minimally adjusted vaccination effect model, baseline demographics only
### model 3 - fully adjusted vaccination effect model, baseline demographics + clinical characteristics

model_names = c(
  "Unadjusted" = "0",
  "Adjusting for time" = "1",
  "Adjusting for time + demographics" = "2"
)


## TODO - define knots based on event times, not on all follow-up time
nsevents <- function(x, events, df){
  # this is the same as the `ns` function,
  # except the knot locations are chosen
  # based on the event times only, not on all person-time
  probs <- seq(0,df)/df
  q <- quantile(x[events==1], probs=probs)
  ns(x, knots=q[-c(1, df+1)], Boundary.knots = q[c(1, df+1)])
}


# base formula to be built on 
formula_outcome <- outcome_event ~ 1

if(match_strategy == "a") formula_demog <- . ~ . + poly(age, degree=2, raw=TRUE) + sex + ethnicity + imd_Q5 + bmi + asthma + learndis + sev_mental + immunosuppressed + multimorb + timesince_coviddischarged + flu_vaccine_2122 + cancer
if(match_strategy == "b") formula_demog <- . ~ . + poly(age, degree=2, raw=TRUE) 
if(match_strategy == "riskscore_i") formula_demog <- . ~ . + poly(age, degree=2, raw=TRUE) 


# mimicking timescale / stratification in simple cox models
formula_timescale_ns <- . ~ . + nsevents(tstop, outcome_event, 4) # spline for timescale only
formula_spacetime <- . ~ . + ns(trial_date, 3)*stp # spline for space-time adjustment - check with Will if the right time variable has been used
formula_timesincevax_ns <- . ~ . + treated + treated:nsevents(tstop, outcome_event, 4)

### natural cubic spline formulae ----
### estimands
formula_vaxonly_ns <- formula_outcome  %>% update(formula_timesincevax_ns) %>% update(formula_timescale_ns)

formula0_ns <- formula_vaxonly_ns
formula1_ns <- formula_vaxonly_ns %>% update(formula_spacetime)
formula2_ns <- formula_vaxonly_ns %>% update(formula_spacetime) %>% update(formula_demog)


## optimisation options ----
parglmparams <- parglm.control(
  method = "LINPACK",
  nthreads = 8,
  maxit = 40 # default = 25
)

plr_process <- function(plrmod, number, cluster){

  print(warnings())
  logoutput(
    glue("model{number} data size = ", plrmod$n),
    glue("model{number} memory usage = ", format(object.size(plrmod), units="GB", standard="SI", digits=3L)),
    glue("convergence status: ", plrmod$converged)
  )

  glance <-
    glance_plr(plrmod) %>%
    add_column(
      model = number,
      convergence = plrmod$converged,
      ram = format(object.size(plrmod), units="GB", standard="SI", digits=3L),
      .before=1
    )
  #write_rds(glance, here("output", "model", outcome, timescale, glue("modelplr_glance{number}.rds")), compress="gz")

  tidy <- broom.helpers::tidy_plus_plus(
    plrmod,
    tidy_fun = tidy_plr,
    exponentiate = FALSE,
    cluster = cluster
  ) %>%
  add_column(
    model=number,
    .before=1
  )
  #write_rds(tidy, here("output", "model", outcome, timescale, glue("modelplr_tidy{number}{splinetype}.rds")), compress="gz")

  vcov <- vcovCL(plrmod, cluster = cluster, type = "HC0")
  write_rds(vcov,  ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome, glue("modelplr_vcov{number}.rds")), compress="gz")

  ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome, glue("modelplr_model{number}.rds"))
  
  plrmod$data <- NULL
  write_rds(plrmod, ghere("output", glue("{effect}_{match_strategy}"), "model", "plr", subgroup, outcome, glue("modelplr_model{number}.rds")), compress="gz")

  lst(glance, tidy)
}


## continuous estimands ----


plrmod0 <- parglm(
  formula = formula0_ns,
  data = data_plr,
  #weights = sample_weights,
  family = binomial,
  control = parglmparams,
  na.action = "na.fail",
  model = FALSE
)
summary0 <- plr_process(
  plrmod0, 0,
  data_plr$patient_id #, "ns"
)
if(removeobs){remove(plrmod0)}

plrmod1 <- parglm(
  formula = formula1_ns,
  data = data_plr,
  #weights = sample_weights,
  family = binomial,
  control = parglmparams,
  na.action = "na.fail",
  model = FALSE
)
summary1 <- plr_process(
  plrmod1, 1,
  data_plr$patient_id#, "ns"
)
if(removeobs){remove(plrmod1)}

plrmod2 <- parglm(
  formula = formula2_ns,
  data = data_plr,
  #weights = sample_weights,
  family = binomial,
  control = parglmparams,
  na.action = "na.fail",
  model = FALSE
)
summary2 <- plr_process(
  plrmod2, 2,
  data_plr$patient_id#, "ns"
)
if(removeobs){remove(plrmod2)}


### combine results ----
model_glance <- bind_rows(summary0$glance, summary1$glance, summary2$glance, summary3$glance) %>%
  mutate(
    model_name = fct_recode(as.character(model), !!!model_names),
    outcome = outcome
  )
write_csv(model_glance, here::here("output", "model", outcome, timescale, sens_path, glue("modelplr_glance_ns.csv")))

model_tidy <- bind_rows(summary0$tidy, summary1$tidy, summary2$tidy, summary3$tidy) %>%
  mutate(
    model_name = fct_recode(as.character(model), !!!model_names),
    outcome = outcome
  )
write_csv(model_tidy, here::here("output", "model", outcome, timescale, sens_path, glue("modelplr_tidy_ns.csv")))
write_rds(model_tidy, here::here("output", "model", outcome, timescale, sens_path, glue("modelplr_tidy_ns.rds")))

## print warnings ----
print(warnings())
cat("  \n")
print(gc(reset=TRUE))



