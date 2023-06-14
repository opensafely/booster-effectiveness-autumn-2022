# # # # # # # # # # # # # # # # # # # # #
# Purpose: match pfizer recipients to moderna recipients
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

## Import libraries ----
library(tidyverse)
library(here)
library(glue)
library(survival)
library(MatchIt)
library(doParallel)

## Import custom user functions from lib
source(here("lib", "functions", "utility.R"))

## Import design elements
source(here("analysis", "design.R"))

# import command-line arguments 
args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  # use for interactive testing
  match_strategy <- "A"
} else {
  match_strategy <- args[[1]]
}

# create output directories ----
output_dir <- here("output", glue("comparative_{match_strategy}"), "match")
fs::dir_create(output_dir)

# Prepare data ----

## one pow per patient ----
data_treated <- read_rds(here("output", "treated", "eligible", "data_treated.rds"))

print(
  cat(
    glue("data_treated", " data size = ", nrow(data_treated)),
    glue("data_treated", " memory usage = ", format(object.size(data_treated), units="GB", standard="SI", digits=3L)),
    sep = "\n  "
  )
)

## select all match candidates and variables necessary for match
# encode all exact variables that are characters as factors
char_to_factor <- data_treated %>%
  select(all_of(exact_variables_comparative)) %>%
  select(where(is.character)) %>%
  names()

data_matchcandidates <- data_treated %>%
  mutate(
    # create variable to parallelise on (just pick an exact match variable??)
    thread_variable = agegroup_match,
    thread_id = dense_rank(thread_variable),
    # matchit needs a binary variables for match; pfizerbivalent=0, modernabivalent=1
    treated = as.integer(vax_boostautumn_brand == (comparison_definition %>% filter(comparison=="comparative") %>% pull(level1))),
  ) %>%
  select(
    thread_id,
    thread_variable,
    patient_id,
    vax_boostautumn_date,
    treated,
    all_of(match_variables_comparative),
  ) %>%
  mutate(across(all_of(char_to_factor), as.factor)) %>%
  arrange(patient_id)

# tidy up 
rm(data_treated)

# check for any missing data in data_matchcandidates
check_missing <- data_matchcandidates %>%
  map_int(~sum(is.na(.x)))

if (any(check_missing > 0)) {
  cat("Number of missing entries in each column:\n")
  print(check_missing)
  stop("match will fail if there are missing values.")
}

# create function that catches errors in case no matches are found within a thread
safely_matchit <- purrr::safely(matchit)

## parallelisation preliminaries ----

parallel::detectCores() # how many cores available?
n_threads <- 8

cluster <- parallel::makeCluster(
  n_threads,
  type = "PSOCK" # this should work across multi-core windows or linux machines
)
print(cluster)
#register it to be used by %dopar%
registerDoParallel(cl = cluster)

# create parallel match streams
matchthreads <- unique(as.character(data_matchcandidates$thread_variable))

table(data_matchcandidates$thread_variable, useNA="ifany")

## match in parallel ----
data_matchstatus <-
  foreach(
    matchthread = matchthreads,
    .combine = 'bind_rows',
    .packages = c("dplyr", "MatchIt", "tibble", "lubridate")
  ) %dopar% {
  #for(matchthread in matchthreads){
    
    data_thread <- data_matchcandidates %>%
      filter(thread_variable==matchthread)
    
    # run match algorithm
    # (this will be updated to allow for different matching strategies)
    obj_matchit <-
      safely_matchit(
        formula = treated ~ 1,
        data = data_thread,
        method = "nearest", distance = "glm", # these two options don't really do anything because we only want exact + caliper match
        replace = FALSE,
        estimand = "ATT",
        exact = exact_variables_comparative,
        caliper = caliper_variables, std.caliper=FALSE,
        m.order = "data", # data is sorted on (effectively random) patient ID
        #verbose = TRUE,
        ratio = 1L # could also consider exact match only, with n:m ratio, determined by availability
      )[[1]]
    
    ## process matchit object to give one row per candidate, matched status (0/1) and match id
    
    data_matchstatus <-
      if(is.null(obj_matchit)){
        tibble(
          patient_id = data_thread$patient_id,
          matched = FALSE,
          thread_id = data_thread$thread_id,
          threadmatch_id = NA_integer_,
          treated = data_thread$treated,
          weight = 0,
          trial_date = data_thread$vax_boostautumn_date
        )
      } else {
        as.data.frame(obj_matchit$X) %>%
          select(trial_date = vax_boostautumn_date) %>%
          add_column(
            patient_id = data_thread$patient_id,
            matched = !is.na(obj_matchit$subclass),
            thread_id = data_thread$thread_id,
            threadmatch_id = as.integer(as.character(obj_matchit$subclass)),
            treated = obj_matchit$treat,
            weight = obj_matchit$weights,
            .before = 1
          ) %>% as_tibble()
      }
    
    data_matchstatus
  }

stopCluster(cl = cluster)

data_matchstatus <- data_matchstatus %>%
  arrange(thread_id, threadmatch_id) %>%
  mutate(
    match_id = dense_rank(threadmatch_id * max(thread_id) + thread_id) # create unique match id across all threads
  )

write_rds(data_matchstatus, fs::path(output_dir, "data_matchstatus.rds"), compress="gz")

# summarise match status for each trial date
data_matchstatus %>%
  group_by(trial_date, treated, matched) %>%
  summarise(
    n=n()
  ) %>%
  print(n=1000)
