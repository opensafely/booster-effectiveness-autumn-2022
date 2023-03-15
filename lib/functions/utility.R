
# combine here() and glue() functionality
ghere <- function(...){
  here::here(glue::glue(..., .sep=.Platform$file.sep))
}

ceiling_any <- function(x, to=1){
  # round to nearest 100 millionth to avoid floating point errors
  ceiling(plyr::round_any(x/to, 1/100000000))*to
}

roundmid_any <- function(x, to=1){
  # like ceiling_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}


fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

# for relabelling variables
# use like this:
# fct_recoderelevel(variable_coded,  c(`code1`="full name 1", `code2` = "full name 2"))
fct_recoderelevel <- function(x, lookup){
  stopifnot(!is.na(names(lookup)))
  factor(x, levels=lookup, labels=names(lookup))
}

# function for printing summaries of datasets to txt files
my_skim <- function(
  .data, # dataset to be summarised
  path,
  id_suffix = "_id" # (set to NULL if no id columns)
) {
  
  # don't run when in an interactive session as for some reason is causes R to crash
  if(Sys.getenv("OPENSAFELY_BACKEND") != "") {
    
    # specify summary function for each class
    my_skimmers <- list(
      logical = skimr::sfl(
      ),
      # numeric applied to numeric and integer
      numeric = skimr::sfl(
        mean = ~ mean(.x, na.rm=TRUE),
        sd = ~ sd(.x, na.rm=TRUE),
        min = ~ min(.x, na.rm=TRUE),
        p10 = ~ quantile(.x, p=0.1, na.rm=TRUE, type=1),
        p25 = ~ quantile(.x, p=0.25, na.rm=TRUE, type=1),
        p50 = ~ quantile(.x, p=0.5, na.rm=TRUE, type=1),
        p75 = ~ quantile(.x, p=0.75, na.rm=TRUE, type=1),
        p90 = ~ quantile(.x, p=0.9, na.rm=TRUE, type=1),
        max = ~ max(.x, na.rm=TRUE)
      ),
      character = skimr::sfl(),
      factor = skimr::sfl(),
      Date = skimr::sfl(
        # wrap in as.Date to avoid errors when all missing
        min = ~ as.Date(min(.x, na.rm=TRUE)),
        p50 = ~ as.Date(quantile(.x, p=0.5, na.rm=TRUE, type=1)),
        max = ~ as.Date(max(.x, na.rm=TRUE))
      ),
      POSIXct = skimr::sfl(
        # wrap in as.POSIXct to avoid errors when all missing
        min = ~ as.POSIXct(min(.x, na.rm=TRUE)),
        p50 = ~ as.POSIXct(quantile(.x, p=0.5, na.rm=TRUE, type=1)),
        max = ~ as.POSIXct(max(.x, na.rm=TRUE))
      )
    )
    
    my_skim_fun <- skimr::skim_with(
      !!!my_skimmers,
      append = FALSE
    )
    
    # summarise factors as the printing is not very nice or flexible in skim
    summarise_factor <- function(var) {
      
      out <- .data %>%
        group_by(across(all_of(var))) %>%
        count() %>%
        ungroup() %>%
        mutate(across(n, ~roundmid_any(.x, to = 7))) %>%
        mutate(percent = round(100*n/sum(n),2)) %>%
        arrange(!! sym(var)) 
      
      total <- nrow(out)
      
      out %>%
        slice(1:min(total, 10)) %>% 
        knitr::kable(
          format = "pipe",
          caption = glue::glue("{min(total, 10)} of {total} factor levels printed")
        ) %>% 
        print()
      
    }
    
    vars <- .data %>% 
      select(-ends_with(id_suffix)) %>% 
      select(where(~ is.factor(.x) | is.character(.x))) %>%
      names()
    
    options(width = 120)
    capture.output(
      {
        cat("The following id variables are removed from this summary:\n")
        print(.data %>% select(ends_with(id_suffix)) %>% names())
        cat("\n")
        print(my_skim_fun(.data, -ends_with(id_suffix)))
        cat("\n")
        cat("--- counts for factor and character variables ---")
        for (v in vars) {
          summarise_factor(v)
        }
      },
      file = path,
      append = FALSE
    )
    
  }
}

# define function to add descriptive variables ----
add_descr <- function(.data, treated=TRUE, subgroup=TRUE, outcome=TRUE, variant=TRUE) {
  
  # brand_descr <- brand_lookup %>% filter(brand %in% model_brands) %>% pull(brand_descr)
  
  if (treated & ("treated" %in% names(.data))) {
    
    treated_levs <- c(0,1)
    treated_descr <- c("Unboosted", "Boosted")
    
    .data <- .data %>%
      mutate( 
        treated_descr = factor(treated, levels = treated_levs, labels = treated_descr)
      ) %>%
      mutate(across(treated, factor, levels = treated_levs)) 
  }
  
  if (subgroup) {
    
    subgroup_descr <- names(recoder$subgroups[recoder$subgroups %in% subgroups])
    
    .data <- .data %>%
      mutate( 
        subgroup_descr = factor(subgroup, levels = subgroups, labels = subgroup_descr),
      ) %>%
      mutate(across(subgroup, factor, levels = subgroups)) 
    
  }
  
  if (outcome) {
    
    outcome_descr <- events_lookup %>% filter(event %in% outcomes) %>% pull(event_descr)
    
    .data <- .data %>%
      mutate( 
        outcome_descr = factor(outcome, levels = outcomes, labels = outcome_descr),
      ) %>%
      mutate(across(outcome, factor, levels = outcomes)) 
    
  }
  
  if (variant & ("variant" %in% names(.data))) {
    
    variant_levs <- c(
      "all variants" = "ignore", 
      "delta variant" = "delta", 
      "delta-omicron transition" = "transition",
      "omicron variant" = "omicron"
      )
    
    .data <- .data %>%
      mutate( 
        variant_descr = factor(variant, levels = unname(variant_levs), labels = names(variant_levs))
      ) %>%
      mutate(across(variant, factor, levels = unname(variant_levs)))
    
  }
  
  return(.data)
  
}

