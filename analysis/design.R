# # # # # # # # # # # # # # # # # # # # # #
# # This script:
# # creates metadata for aspects of the study design
# # # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

## Import libraries ----
library(tidyverse)
library(here)

## create output directories ----
fs::dir_create(here("lib", "design"))

# redaction threshold ----
threshold <- 6

# define key dates ----

# https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/
# https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/COVID-19-weekly-announced-vaccinations-20-April-2023.xlsm

study_dates <- lst(
  
  boosterautumn = lst(
    ages65plus = "2022-09-12",
    ages50to64 = "2022-10-15"
  ),
  
  boosterspring = lst(
    start = "2022-03-23",
    end = "2022-07-31" # when spring booster uptake reached peak coverage of 79.1% of people over 75 years
  ),
  
  boosterfirst = lst(
    pfizerstart = "2021-09-16", # first pfizer vaccination in national roll-out
    modernastart = "2021-10-29", # first moderna vaccination in national roll-out
    end = as.Date(boosterspring$start) - 7, # 7 days before start of spring boost
  ),
  
  dose2 = lst(
    # don't recruit anyone with second vaccination after this date
    end = as.Date(boosterfirst$pfizerstart) - 1, 
  ),

  # vaccine schedule dates
  dose1 = lst(
    pfizer = "2020-12-08", # first pfizer vaccination in national roll-out
    az = "2021-01-04", # first az vaccination in national roll-out
    moderna = "2021-04-13", # first moderna vaccination in national roll-out
  ),
  
  studystart = min(
    as.Date(boosterautumn$ages65plus), 
    as.Date(boosterautumn$ages50to64)
    ),
  
  recruitmentend = "2022-12-24", # based on plot of weekly vaccinations in England
  hospitalisationend = "2023-01-31", # end of available hospitalization data
  deathend = "2023-02-28", # end of available death data
  
  riskscore_i = lst(
    start = "2022-04-01",
    end = as.Date(boosterautumn$ages65plus) - 1
  )
  
)

study_dates <- rapply(
  study_dates, 
  function(x) as.Date(x, format = "%Y-%m-%d"), 
  how = "list"
  )

# the gap between incremental matching rounds
extract_increment <- 14

study_dates$control_extract = seq(study_dates$studystart, study_dates$recruitmentend, extract_increment)

# reduce the match rounds for testing
# study_dates$control_extract <- study_dates$control_extract[1:2]
# 
# # number of match rounds to perform for each cohort
# 
# n_match_rounds <- length(study_dates[["control_extract"]])

jsonlite::write_json(study_dates, path = here("lib", "design", "study-dates.json"), auto_unbox=TRUE, pretty =TRUE)

# define outcomes ----

events_lookup <- tribble(
  ~event, ~event_var, ~event_descr,

  # other
  "dereg", "dereg_date", "Deregistration date",

  # effectiveness
  "covidadmitted", "covidadmitted_date", "COVID-19 hospitalisation",
  # "covidcritcare", "covidcritcare_date", "COVID-19 critical care",
  "coviddeath", "coviddeath_date", "COVID-19 death",
  # "covidcritcareordeath", "covidcritcareordeath_date", "COVID-19 critical care or death",

  # other
  "noncoviddeath", "noncoviddeath_date", "Non-COVID-19 death",
  # "cvdnoncoviddeath", "cvdnoncoviddeath_date", "CVD-related non-COVID-19 death",
  # "cancernoncoviddeath", "cancernoncoviddeath_date", "Cancer-related non-COVID-19 death",
  # "death", "death_date", "Any death",
  "fracture", "fracture_date", "Fracture"

)

outcomes <- c("covidadmitted", "coviddeath", "noncoviddeath", "fracture")

# define treatments ----

treatment_lookup <- tribble(
  ~course, ~treatment, ~treatment_descr,
  "boostautumn","pfizerbivalent", "Bivalent BNT162b2",
  "boostautumn", "modernabivalent", "Bivalent mRNA-1273",
  "boostspring","pfizer", "BNT162b2",
  "boostspring", "moderna", "mRNA-1273",
  "boostfirst","pfizer", "BNT162b2",
  "boostfirst", "moderna", "mRNA-1273",
  "primary", "pfizer", "BNT162b2",
  "primary", "az", "ChAdOx1-S",
  "primary", "moderna", "mRNA-1273"
)

comparison_definition <- tribble(
  ~comparison, ~level0, ~level0_descr, ~level1, ~level1_descr,
  "comparative", "pfizerbivalent", "Bivalent BNT162b2", "modernabivalent", "Bivalent mRNA-1273",
  "incremental", "unboosted", "Unboosted", "boosted", "Boosted",
)

# lookups to convert coded variables to full, descriptive variables ----
recoder <-
  lst(
    subgroups = c(
      `Main` = "all",
      `Age` = "agegroup"
    ),
    status = c(
      `Matched` = "matched",
      `Unmatched`= "unmatched"
    ),
    outcome = set_names(events_lookup$event, events_lookup$event_descr),
    all = c(`Main` = "all"),
    agegroup_match = c(
      `50-64 years` = "50-64",
      `65-74 years` = "65-74",
      `75+ years` = "75+"
    )
  )

subgroups <- c("all", "agegroup_match")

# for the treated variables which are coded as 0 or 1
for (i in c("comparative", "incremental")) {
  treatment_levels <- comparison_definition %>% filter(comparison==i) %>% select(matches("level\\d_descr")) %>% unlist() 
  recoder[[i]] <- set_names(
    as.integer(str_extract(names(treatment_levels), "\\d")),
    unname(treatment_levels)
  )
  rm(treatment_levels)
}

## follow-up time ----
# TODO these need to be reviewed depending on how much follow-up data we have
fup_params <- lst(
  # length of baseline period
  baselinedays = 14,
  # length of follow-up period
  postbaselinedays = 28,
  # number of follow-up periods
  postbaselineperiods = 6,
  # where to split follow-up time after recruitment
  postbaselinecuts = c(0, baselinedays, baselinedays + (1:postbaselineperiods)*postbaselinedays),
  # maximum follow-up
  maxfup = max(postbaselinecuts),
)

# matching ----
create_match_strategy <- function(
    name,
    n_match_rounds = 4, # need to update, but use 4 for testing code
    exact_vars = NULL,
    caliper_vars = NULL,
    riskscore_vars = NULL, # variable to be included as covariates in risk score model
    riskscore_fup_vars = NULL, # includes outcome and censoring events
    adj_vars = NULL,
    strata_vars = NULL
) {
  out <- lst(
    n_match_rounds = n_match_rounds,
    exact_vars = exact_vars,
    caliper_vars = caliper_vars,
    riskscore_vars = riskscore_vars,
    riskscore_fup_vars,
    # these variables only need to be extracted in controlfinal, although
    # they may have been extracted earlier
    adj_vars = adj_vars,
    strata_vars = strata_vars,
    # group the variables so that ...
    match_vars = unique(c(
      exact_vars, names(caliper_vars), riskscore_vars, riskscore_fup_vars
    )),
    final_vars = unique(c(adj_vars, strata_vars)),
    # variables to keep in the dataset throughout stages
    keep_vars = c("age", "agegroup_match", "sex", "imd")
  )
  
  out %>%
    jsonlite::write_json(
      path = here::here("lib", "design", glue::glue("match-strategy-{name}.json")), 
      auto_unbox=TRUE, pretty =TRUE
      )
  
  return(out)
}

match_strategy_none <- create_match_strategy(
  name = "none",
  n_match_rounds = NULL,
  # all possible vars used across matching strategies
  exact_vars = c(
    # defined in or derived from analysis/study_definition_initial.py
    "vax_primary_brand", "vax_boostfirst_brand", "vax_boostspring_brand",
    "vax_lastbeforeindex_date", "sex", "ethnicity", "hscworker",
    # defined in or derived from analysis/variables_elig.py
    "age", "agegroup_match", "timesince_coviddischarged", "imd_Q5",
    # defined in or derived from analysis/variables_jcvi.py
    "asthma", "chronic_neuro_disease", "chronic_resp_disease", "bmi",
    "diabetes", "sev_mental", "chronic_heart_disease", "chronic_kidney_disease",
    "chronic_liver_disease", "immunosuppressed", "learndis", "multimorb", "cv",
    "cancer",
    # "asplenia", "bmi_value", "sev_obesity",
    # optional variables in analysis/variables_vars.py
    "region", "flu_vaccine", "timesince_discharged", 
    # defined in or derived from analysis/study_definitionriskscore_i.py
    "death", "dereg", "riskscore_i", "riskscore_i_percentile"
  )
)

match_strategy_riskscore_i <- create_match_strategy(
  name = "riskscore_i",
  n_match_rounds = 3,
  exact_vars = "riskscore_i_percentile",
  # caliper_vars = c("riskscore_i" = 0.1), 
  # riskscore_vars are the variables used in the model to predict the risk score
  riskscore_vars = c(
    "age", "sex", "asthma", "chronic_neuro_disease", "chronic_resp_disease", "bmi",
    "diabetes", "sev_mental", "chronic_heart_disease", "chronic_kidney_disease",
    "chronic_liver_disease", "immunosuppressed", "learndis", "cancer",
    "ethnicity", "imd_Q5", "region", "flu_vaccine", "timesince_discharged",
    "vax_boostfirst_brand" # maybe edit this so any/none rather than pfizer/moderna/none
    ),
  riskscore_fup_vars = c("death", "dereg"),
  adj_vars = c(
    "sex", "ethnicity", "imd_Q5", "bmi", "learndis", "sev_mental",
    "immunosuppressed", "multimorb",  "timesince_coviddischarged",
    "flu_vaccine", "cancer"
  ),
  strata_vars = c("trial_date", "region")
)

match_strategy_a <- create_match_strategy(
  name = "a",
  n_match_rounds = 4,
  exact_vars = c(
    "agegroup_match", "vax_primary_brand", "vax_boostfirst_brand",
    "vax_boostspring_brand", "cv", "region"
    ),
  caliper_vars = c(
    age = 3,
    # match on `lastvaxbeforeindex_day` rather than `timesincelastvax` as the 
    # potential matches are less likely to fail in the actual stage
    vax_lastbeforeindex_date = 14,
    NULL
  ),
  adj_vars = c(
    "sex", "ethnicity", "imd_Q5", "bmi", "learndis", "sev_mental",
    "immunosuppressed", "multimorb",  "timesince_coviddischarged",
    "flu_vaccine", "cancer"
  ),
  strata_vars = c("trial_date", "region")
)

# check if all variables from all matching strategies are in match_strategy_none$keep_vars
local({
  all_vars <- unique(c(
    match_strategy_a$match_vars, match_strategy_a$final_vars, 
    match_strategy_riskscore_i$match_vars, match_strategy_riskscore_i$final_vars
  ))
  all_vars <- all_vars[!(all_vars %in% c("trial_date"))]
  check_all_present <- all_vars %in% unique(c(match_strategy_none$match_vars, match_strategy_none$final_vars))
  if (!all(check_all_present)) {
    stop(
      "The following variables are specified in a matching strategy but not in match_strategy_none:\n",
      str_c(all_vars[!check_all_present], sep = ", ")
    )
  }
})

# match_strategy_C <- lst(
#   score_vars = xxx,
#   model_vars = xxx,
#   strata_vars = xxx
# )

# list2env

# match variables ----

# # exact variables
# exact_variables_incremental <- c(
#   "agegroup_match",
#   "vax_primary_brand",
#   "vax_boostfirst_brand",
#   "vax_boostspring_brand",
#   "cv",
#   "region",
#   NULL
# )
# 
# exact_variables_comparative <- c(
#   exact_variables_incremental,
#   "vax_boostautumn_date", 
#   NULL
# )
# 
# # caliper variables
# caliper_variables <- c(
#   age = 3,
#   # match on `lastvaxbeforeindex_day` rather than `timesincelastvax` as the 
#   # potential matches are less likely to fail in the actual stage
#   vax_lastbeforeindex_date = 14,
#   NULL
# )
# 
# match_variables_incremental <- c(exact_variables_incremental, names(caliper_variables))
# match_variables_comparative <- c(exact_variables_comparative, names(caliper_variables))
# 
# # covariates ----
# 
# covariates_model <- c(
#   "sex",
#   "ethnicity",
#   "imd_Q5",
#   "bmi",
#   "learndis",
#   "sev_mental",
#   "immunosuppressed",
#   "multimorb", 
#   "timesince_coviddischarged",
#   "flu_vaccine"
# )
# 
# strata_vars <- c("trial_date", "region")

censor_vars <- list(
  comparative = c(
    "death_date",
    "dereg_date"
  )
)
censor_vars[["incremental"]] <- c(censor_vars[["comparative"]], "controlistreated_date")

#
# # other variables -----
# # keep all variables starting with these strings
# other_variables <- c("trial", "treated", "control", "match", "vax", "jcvi")
# 
# analysis table
model_args <- expand_grid(
  effect=c("comparative", "incremental"),
  model=c("km", "cox_unadj", "cox_adj"),
  subgroup=subgroups,
  outcome=outcomes,
)

