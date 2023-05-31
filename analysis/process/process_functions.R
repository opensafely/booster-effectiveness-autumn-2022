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
    mutate(across(starts_with("pct_"), round, 3))
}

################################################################################
add_vars <- function(.data, vars, group) {
  
  stopifnot("`vars` must be \"covs\" or \"outcomes\"" = (vars == "covs" | vars == "outcomes"))
  
  if (all(c("treated", "control") %in% group)) {
    
    by_vars <- c("patient_id", "trial_date")
    remove_vars <- NULL
    
  } else if (all(group == "treated")) {
    
    # omit trial_id here as not needed and will slow down
    by_vars <- "patient_id"
    remove_vars <- "trial_date"
    
  } else {
    
    stop("`group` must be either c(\"treated\", \"control\") or \"treated\"")
    
  }
  
  # source(here::here("analysis", "process", "process_functions.R"))
  
  group[group=="treated"] <- "alltreated"
  group[group=="control"] <- "matchcontrol"
  
  data_vars <- map_dfr(
    group,
    ~arrow::read_feather(here("output", "postmatch", vars, glue("input_{vars}_", .x, ".feather")))
  ) %>%
    process_input() 
  
  if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")) {
    
    # if dummy data remove all dates before trial_date
    data_vars <- data_vars %>% 
      mutate(across(
        ends_with("_date"), 
        ~if_else(.x < trial_date, as.Date(NA_character_), .x)
        ))
    
  }
  
  .data %>%
    # the next line is to move trial_date when arms=="treated"
    select(-all_of(remove_vars)) %>%
    left_join(data_vars, by = by_vars)
  
}

################################################################################
process_covs <- function(.data) {

  .data %>%
    mutate(

      bmi = factor(bmi, levels = c("Not obese", "Obese I (30-34.9)", "Obese II (35-39.9)", "Obese III (40+)")),

    )

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
