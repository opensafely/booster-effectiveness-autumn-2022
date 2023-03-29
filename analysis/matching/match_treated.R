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

# create output directories ----

output_dir <- here("output", "treated", "matching")
fs::dir_create(output_dir)

# Prepare data ----

## one pow per patient ----
data_treated <- read_rds(here("output", "treated", "eligible", "data_treated.rds"))

# autumn booster brand info
data_eligible_treated <- read_csv(
  here("output", "initial", "eligible", "data_eligible_treated.csv.gz")
  ) %>%
  mutate(across(patient_id, as.integer))

print(
  cat(
    glue("data_treated", " data size = ", nrow(data_treated)),
    glue("data_treated", " memory usage = ", format(object.size(data_treated), units="GB", standard="SI", digits=3L)),
    sep = "\n  "
  )
)

## select all matching candidates and variables necessary for matching
# encode all exact variables that are characters as factors
char_to_factor <- data_treated %>%
  select(all_of(exact_variables_treated)) %>%
  select(where(is.character)) %>%
  names()

data_matchingcandidates <- data_treated %>%
  mutate(
    # create variable to parallelise on (just pick an exact match variable??)
    thread_variable = agegroup_match,
    thread_id = dense_rank(thread_variable)
  ) %>%
  select(
    thread_id,
    thread_variable,
    patient_id,
    index_date,
    vax_boostautumn_brand,
    all_of(matching_variables_treated),
  ) %>%
  mutate(
    # matchit needs a binary variables for matching; pfizerbivalent=1, modernabivalent=0
    treatment = vax_boostautumn_brand == "pfizerbivalent"
  ) %>%
  mutate(across(all_of(char_to_factor), as.factor)) %>%
  arrange(patient_id)

# tidy up 
rm(data_treated, data_eligible_treated)

# check for any missing data in data_matchingcandidates
check_missing <- data_matchingcandidates %>%
  map_int(~sum(is.na(.x)))

if (any(check_missing > 0)) {
  cat("Number of missing entries in each column:\n")
  print(check_missing)
  stop("Matching will fail if there are missing values.")
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

# create parallel matching streams
matchthreads <- unique(as.character(data_matchingcandidates$thread_variable))

table(data_matchingcandidates$thread_variable, useNA="ifany")

## match in parallel ----
data_matchstatus <-
  foreach(
    matchthread = matchthreads,
    .combine = 'bind_rows',
    .packages = c("dplyr", "MatchIt", "tibble", "lubridate")
  ) %dopar% {
    
    #for(matchthread in matchthreads){
    
    data_thread <- data_matchingcandidates %>%
      filter(thread_variable==matchthread)
    
    # run matching algorithm
    obj_matchit <-
      safely_matchit(
        formula = treatment ~ 1,
        data = data_thread,
        method = "nearest", distance = "glm", # these two options don't really do anything because we only want exact + caliper matching
        replace = FALSE,
        estimand = "ATT",
        exact = exact_variables_treated,
        caliper = caliper_variables, std.caliper=FALSE,
        m.order = "data", # data is sorted on (effectively random) patient ID
        #verbose = TRUE,
        ratio = 1L # could also consider exact matching only, with n:m ratio, determined by availability
      )[[1]]
    
    ## process matchit object to give one row per candidate, matched status (0/1) and match id
    
    data_matchstatus <-
      if(is.null(obj_matchit)){
        tibble(
          patient_id = data_thread$patient_id,
          matched = FALSE,
          thread_id = data_thread$thread_id,
          threadmatch_id = NA_integer_,
          treatment = data_thread$treatment,
          weight = 0,
          index_date = data_thread$index_date
        )
      } else {
        as.data.frame(obj_matchit$X) %>%
          select(index_date) %>%
          add_column(
            patient_id = data_thread$patient_id,
            matched = !is.na(obj_matchit$subclass),
            thread_id = data_thread$thread_id,
            threadmatch_id = as.integer(as.character(obj_matchit$subclass)),
            treatment = obj_matchit$treat,
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

data_matchstatus %>%
  group_by(index_date, treatment, matched) %>%
  summarise(
    n=n()
  ) %>%
  print(n=1000)



## bootstrap sampling ----

## bootstrap sample matched pairs and use this sampling throughout the analysis
## doing it here avoids repeating the sampling process in each individual outcome script
## and provides consistency across different analyses
## but the leg work is still done by the analysis scripts

boot_n <- 500 # more than necessary, can select fewer in the analysis scripts

boot_id <- seq_len(boot_n)

match_ids <- unique(data_matchstatus$match_id[!is.na(data_matchstatus$match_id)])

set.seed(20220506)

boot_samples <-
  tibble(boot_id) %>%
  mutate(
    match_id = map(boot_id, ~sample(match_ids, size=length(match_ids), replace=TRUE))
  ) %>%
  unnest(match_id)

write_rds(boot_samples, fs::path(output_dir, "boot_samples.rds"), compress="gz")

