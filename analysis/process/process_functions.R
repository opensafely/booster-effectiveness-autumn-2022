################################################################################
# functions for processing each of the variable groups in analysis/process_data.R


process_input <- function(.data) {
  
  .data %>%
    # because date types are not returned consistently by cohort extractor
    mutate(across(ends_with("_date"), ~ as.Date(.))) %>%
    # for consitency with dummy data
    mutate(across(where(is.factor), as.character))
  
}


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
process_jcvi <- function(.data) {
  
  .data %>%
    mutate(
      
      multimorb =
        (sev_obesity) +
        (chronic_heart_disease) +
        (chronic_kidney_disease) +
        (diabetes) +
        (chronic_liver_disease) +
        (chronic_resp_disease | asthma) +
        (chronic_neuro_disease),
      multimorb = cut(multimorb, breaks = c(0, 1, 2, Inf), labels=c("0", "1", "2+"), right=FALSE),
      immunosuppressed = immunosuppressed | asplenia,
      
      # clinically at-risk group
      cv = immunosuppressed | chronic_kidney_disease | chronic_resp_disease | diabetes | chronic_liver_disease |
        chronic_neuro_disease | chronic_heart_disease | asplenia | learndis | sev_mental,
      
      # original priority groups https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1007737/Greenbook_chapter_14a_30July2021.pdf#page=15
      # new priority groups https://www.england.nhs.uk/coronavirus/wp-content/uploads/sites/52/2021/07/C1327-covid-19-vaccination-autumn-winter-phase-3-planning.pdf
      # group 10 split into 16-39 and 40-49 because of earlier roll-out in 40+ from 15 Nov https://www.gov.uk/government/news/jcvi-issues-advice-on-covid-19-booster-vaccines-for-those-aged-40-to-49-and-second-doses-for-16-to-17-year-olds
      
      # jcvi_ageband = cut(
      #   age_aug2021,
      #   breaks=c(50, 55, 60, 65, 70, 75, 80, Inf),
      #   labels=c("50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"),
      #   right=FALSE
      # ),
      
    ) 
}

################################################################################
process_covs <- function(.data) {
  
  .data %>%
    mutate(
      
      bmi = factor(bmi, levels = c("Not obese", "Obese I (30-34.9)", "Obese II (35-39.9)", "Obese III (40+)")),
      
      prior_test_cat = cut(
        prior_test_frequency, 
        breaks=c(0, 1, 2, 3, Inf), 
        labels=c("0", "1", "2", "3+"), 
        right=FALSE
        )
      
    )  
  
}

################################################################################
process_demo <- function(.data) {
  
  .data %>%
    mutate(
      
      # age65plus=age>=65,
      # 
      # agegroup = cut(
      #   age, 
      #   breaks=c(-Inf, 18, 50, 65, 80, Inf),
      #   labels=c("under 18", "18-49", "50-64", "65-79", "80+"),
      #   right=FALSE
      # ),
      # 
      # ageband = cut(
      #   age,
      #   breaks=c(-Inf, 18, 40, 50, 60, 70, 80, 90, Inf),
      #   labels=c("under 18", "18-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
      #   right=FALSE
      # ),
      
      sex = fct_case_when(
        sex == "F" ~ "Female",
        sex == "M" ~ "Male",
        #sex == "I" ~ "Inter-sex",
        #sex == "U" ~ "Unknown",
        TRUE ~ NA_character_
      ),
      
      ethnicity = factor(
        ethnicity,
        levels = c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other")
      ),
      
      region = fct_collapse(
        region,
        `East of England` = "East",
        `London` = "London",
        `Midlands` = c("West Midlands", "East Midlands"),
        `North East and Yorkshire` = c("Yorkshire and The Humber", "North East"),
        `North West` = "North West",
        `South East` = "South East",
        `South West` = "South West"
      ),
      
      imd_Q5 = factor(
        imd_Q5,
        levels = c("1 (most deprived)", "2", "3", "4", "5 (least deprived)")
        )
      
    ) 
}

################################################################################
process_pre <- function(.data) {
  
  .data
  
}

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
