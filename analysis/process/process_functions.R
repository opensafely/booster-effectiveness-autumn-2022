################################################################################
# functions for processing each of the variable groups in analysis/process_data.R


process_input <- function(.data) {
  
  .data %>%
    # because date types are not returned consistently by cohort extractor
    mutate(across(ends_with("_date"), ~ as.Date(.))) %>%
    # for consitency with dummy data
    mutate(across(where(is.factor), as.character))
  
}

################################################################################
flow_stats_rounded <- function(.data, to) {
  .data %>%
    mutate(
      n = roundmid_any(n, to = to),
      n_exclude = lag(n) - n,
      pct_exclude = n_exclude/lag(n),
      pct_all = n / first(n),
      pct_step = n / lag(n),
    )
}

################################################################################
# process_covs <- function(.data) {
#   
#   .data %>%
#     mutate(
#       
#       bmi = factor(bmi, levels = c("Not obese", "Obese I (30-34.9)", "Obese II (35-39.9)", "Obese III (40+)")),
#       
#       prior_test_cat = cut(
#         prior_test_frequency, 
#         breaks=c(0, 1, 2, 3, Inf), 
#         labels=c("0", "1", "2", "3+"), 
#         right=FALSE
#         )
#       
#     )  
#   
# }

################################################################################

# process_outcome <- function(.data) {
#   
#   .data %>%
#     mutate(
#       
#       # earliest covid event after study start
#       anycovid_date = pmin(postest_date, covidemergency_date, covidadmitted_date, covidcritcare_date, coviddeath_date, na.rm=TRUE),
#       
#       noncoviddeath_date = if_else(!is.na(death_date) & is.na(coviddeath_date), death_date, as.Date(NA_character_)),
#       # cvd or cancer deaths must be non-covid
#       # cvddeath_date = if_else(!is.na(cvddeath_date) & is.na(coviddeath_date), death_date, as.Date(NA_character_)),
#       # cancerdeath_date = if_else(!is.na(cancerdeath_date) & is.na(coviddeath_date), death_date, as.Date(NA_character_)),
#       
#       covidcritcareordeath_date = pmin(covidcritcare_date, coviddeath_date, na.rm=TRUE),
#       
#       fracture_date = pmin(fractureemergency_date, fractureadmitted_date, fracturedeath_date, na.rm=TRUE)
#       
#     )
# }
