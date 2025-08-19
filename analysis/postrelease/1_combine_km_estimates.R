# # # # # # # # # # # # # # # # # # # # #
# Purpose: Combine km estimates from different outcomes from post-release info
# Note: This script was neccesary as the km-estimates for different outcomes had to be
#       output from L4 separately due to the number of rows in the output files.  
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

# import libraries
library(tidyverse)
library(glue)
library(here)

# load functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))

# create output directories 
postrelease_dir <- here("output", "postrelease")
fs::dir_create(postrelease_dir)

# create function to combine and save outputs ----
combine_and_save_contrasts <- function(
    filenames,
    new_filename = NULL,
    metaparams = model_args
) {
  
  model_type <- unique(str_extract(filenames, "km|cox"))
  
  metaparams <- metaparams %>%
    filter(str_detect(model, model_type)) %>%
    uncount(length(filenames), .id="filename") %>% 
    mutate(across(filename, ~filenames[.x])) %>%
    filter(str_detect(filename, model))
  
  rounding_info <- unique(str_extract(filenames, "midpoint\\d+"))
  stopifnot(
    "Do not combine files with different rounding" = 
      length(rounding_info) <= 1
  )
  
  if (length(filenames) > 1) {
    stopifnot(
      "Must specify new_filename when length(filenames) > 1" =
        !is.null(new_filename)
    )
    if (!is.na(rounding_info)) {
      new_filename <- str_c(new_filename, rounding_info, sep = "_")
    }
  } else {
    new_filename <- filenames
  }
  
  metaparams %>%
    mutate(
      data = pmap(
        list(effect, model, subgroup, outcome, filename), 
        function(effect, model, subgroup, outcome, filename)  {
          dat <- try(
            read_csv(
              here("output", glue(effect, "_", match_strategy), "model", model, subgroup, outcome, glue(filename, ".csv"))
            )
          )
          if (inherits(dat, "try-error")) {
            dat <- tibble()
          } else {
            dat <- dat %>%
              add_column(
                subgroup_level = as.character(.[[subgroup]]),
                .before=1
              ) 
          }
          return(dat)
        }
        
      )
    ) %>%
    unnest(data) %>%
    select(-any_of(subgroups)) %>%
    mutate(across(
      starts_with(c("surv", "risk", "inc", "cml.rate", "irr", "cmlirr", "sr", "rd", "rr", "cox")),
      ~round(.x, digits = 5)
    )) %>%
    mutate(across( # this bit is a bit of a hack to deal with need to round counts of patients in cox models without rerunning them all - to update cox.R if running again 
      starts_with(c("npat", "nswitch")),
      ~ roundmid_any(.x, to = 6)
    )) %>%
    write_csv(fs::path(output_dir, glue("{new_filename}.csv")))
  
}


# set effect and match strategy
for(effect in c("comparative", "incremental")) {
  for(match_strategy in c("a", "b")) {

    # define output directory
    output_dir <- ghere("output", "postrelease", "{effect}_{match_strategy}", "model")
    fs::dir_create(output_dir)

    # metaparams for all models that have been run for the given effect
    model_args <- model_args[model_args$effect == effect,]

    # get all subgroups
    subgroups <- model_args %>% distinct(subgroup) %>% pull()

    # km outputs
    combine_and_save_contrasts(
      filenames = glue("km_estimates_midpoint{threshold}")
    ) 
  }
}    
