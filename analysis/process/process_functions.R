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
    ) %>%
    mutate(across(starts_with("pct_"), ~round(.x, 3)))
}

################################################################################
process_extra_vars <- function(.data, extra_vars) {
  
  if ("region" %in% extra_vars) {
    
    .data <- .data %>%
      mutate(
        region = fct_collapse(
          region,
          `East of England` = "East",
          `London` = "London",
          `Midlands` = c("West Midlands", "East Midlands"),
          `North East and Yorkshire` = c("Yorkshire and The Humber", "North East"),
          `North West` = "North West",
          `South East` = "South East",
          `South West` = "South West"
        )
      )
    
  }
  
  if ("imd_Q5" %in% extra_vars) {
    
    imd_levs <- c("Unknown", "1 (most deprived)", "2", "3", "4", "5 (least deprived)")
    
    .data <- .data %>%
      mutate(
        # define imd quintiles
        imd_Q5 = cut(
          x = imd,
          breaks = seq(0,1,0.2)*32800,
          labels = imd_levs[-1]
        ),
        # add labels (done here instead of in cut() so can define labels for NAs)
        imd_Q5 = factor(
          replace_na(as.character(imd_Q5), imd_levs[1]),
          levels = imd_levs
        )
        
      )
    
  }
  
  if ("timesince_discharged" %in% extra_vars) {
    
    .data <- .data %>%
      mutate(
        # time since discharged from any unplanned admission
        timesince_discharged = as.integer(study_dates$riskscore_i$start - unplanneddischarged_0_date)/365.25,
        timesince_discharged = fct_case_when(
          is.na(timesince_discharged) | (timesince_discharged > 5) ~ "Never or >5 years",
          timesince_discharged > 2 ~ "2-5 years",
          timesince_discharged > 1 ~ "1-2 years",
          TRUE ~ "Past year"
        )
      )
  }
  
  return(.data)

}

################################################################################

process_outcomes <- function(.data) {

  .data %>%
    mutate(

      noncoviddeath_date = if_else(!is.na(death_date) & is.na(coviddeath_date), death_date, as.Date(NA_character_)),
      # cvd or cancer deaths must be non-covid
      # cvddeath_date = if_else(!is.na(cvddeath_date) & is.na(coviddeath_date), death_date, as.Date(NA_character_)),
      # cancerdeath_date = if_else(!is.na(cancerdeath_date) & is.na(coviddeath_date), death_date, as.Date(NA_character_)),

      covidcritcareordeath_date = pmin(covidcritcare_date, coviddeath_date, na.rm=TRUE),

      fracture_date = pmin(fractureemergency_date, fractureadmitted_date, fracturedeath_date, na.rm=TRUE)
      
      # TODO also derive censoring events here

    )
}

################################################################################

add_descr <- function(
    .data, vars, effect = NULL, subgroup = NULL, 
    remove = FALSE # if TRUE remove the original vars so that only the vars_descr remain
    ) {
  
  stopifnot(
    "`effect` must be specified when \"treated\" in `vars`" = 
      !("treated" %in% vars) | !is.null(effect)
  )
  
  stopifnot(
    "`subgroup` must be specified when \"subgroup_level\" in `vars`" = 
      !("subgroup" %in% vars) | !is.null(subgroup)
  )
  
  add_desc_var <- function(.data, var) {
    
    print_message <- str_c("There is no column called \"", var, "\" in .data")
    stopifnot(print_message = var %in% names(.data))
    
    if (var == "treated") {
      lookup <- recoder[[effect]]
    }
    if (var == "subgroup") {
      lookup <- recoder[["subgroups"]]
    }
    if (var == "subgroup_level") {
      lookup <- recoder[[subgroup]]
    }
    if (var == "status") {
      lookup <- recoder[["status"]]
    } 
    if (var == "outcome") {
      lookup <- recoder[["outcome"]]
      lookup <- lookup[lookup %in% outcomes]
    }
    
   var_descr <- fct_recoderelevel(.data[[var]], lookup)
    
   return(var_descr)
    
  }
  
  names(vars) <- str_c(vars, "_descr")
  
  .data <- .data %>% bind_cols(map_dfc(vars, ~add_desc_var(.data, .x)))
  
  if (remove) {
    .data <- .data %>% select(-all_of(vars))
  }
  
  return(.data)
  
}
