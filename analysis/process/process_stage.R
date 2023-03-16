######################################

# This script:

######################################

# Preliminaries ----

## Import libraries ----
library('tidyverse')
library('lubridate')
library('arrow')
library('here')
library('glue')

## import local functions and parameters ---

source(here("analysis", "design.R"))

source(here("lib", "functions", "utility.R"))

source(here("analysis", "process", "process_functions.R"))

## import command-line arguments ----

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  # use for interactive testing
  stage <- "treated"
  # stage <- "potential"
  # stage <- "actual"
  # stage <- "final"
  # matching_round <- as.integer("1")
} else {
  stage <- args[[1]]
  if (stage %in% c("treated", "final")) {
    if (length(args) > 1) 
      stop("No additional args to be specified when `stage=\"treated\" or stage=\"final\"")
  } else if (stage %in% c("potential", "actual")) {
    if (length(args) == 1) {
      stop("`matching_round` must be specified when `stage=\"potential\"` or \"actual\"")
    }
    matching_round <- as.integer(args[[2]]) # NULL if treated    
  } 
} 

## create output directories and define parameters ----
if (stage == "treated") {
  fs::dir_create(here("output", "pfizer", "treated"))
  fs::dir_create(here("output", "moderna", "treated"))
  fs::dir_create(here("output", "treated", "eligible"))
  fs::dir_create(here("output", "treated", "process"))
  studydef_path <- here("output", "treated", "extract", "input_treated.feather")
  custom_path <- here("lib", "dummydata", "dummy_treated.feather")
} else if (stage %in% c("potential", "actual")) {
  fs::dir_create(ghere("output", "matchround{matching_round}", "process"))
  fs::dir_create(ghere("output", "matchround{matching_round}", "extract", stage))
  fs::dir_create(ghere("output", "matchround{matching_round}", stage))
  studydef_path <- ghere("output", "matchround{matching_round}", "extract", "input_control{stage}.feather")
  custom_path <- here("lib", "dummydata", "dummy_control_potential1.feather")
  matching_round_date <- study_dates$control_extract_dates[matching_round]
} else if (stage == "final") {
  fs::dir_create(ghere("output", "match"))
  studydef_path <- ghere("output", "extract", "input_controlfinal.feather")
  custom_path <- ghere("output", "dummydata", "dummy_control_final.feather")
}

# import data ----

if (stage == "actual") {
  ## trial info for potential matches in round X
  data_potential_matchstatus <- 
    read_rds(ghere("output", "matchround{matching_round}", "potential", "data_potential_matchstatus.rds")) %>% 
    filter(matched==1L)
}

# use externally created dummy data if not running in the server
# check variables are as they should be
if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){
  
  # set seed so that dummy data results are reproducible
  set.seed(10)
  
  data_studydef_dummy <- read_feather(studydef_path) %>%
    # because date types are not returned consistently by cohort extractor
    mutate(across(ends_with("_date"), ~ as.Date(.))) 
  
  data_custom_dummy <- read_feather(custom_path) 
  
  if (stage != "final") {
    data_custom_dummy <- data_custom_dummy %>%
      mutate(
        msoa = sample(factor(c("1", "2")), size=n(), replace=TRUE) # override msoa so matching success more likely
      )
  }
  
  if (stage == "actual") {
    # reuse previous extraction for dummy run, dummy_control_potential1.feather
    data_custom_dummy <- data_custom_dummy %>%
      filter(patient_id %in% data_potential_matchstatus[(data_potential_matchstatus$treated==0L),]$patient_id) %>%
      # remove vaccine variables
      select(-starts_with("covid_vax_")) %>%
      # trial_date and match_id are not included in the dummy data so join them on here
      # they're joined in the study def using `with_values_from_file`
      left_join(
        data_potential_matchstatus %>% 
          filter(treated==0L) %>%
          select(patient_id, trial_date, match_id),
        by="patient_id"
      ) %>%
      # change a few variables to simulate new index dates
      mutate(
        region = if_else(runif(n())<0.05, sample(x=unique(region), size=n(), replace=TRUE), region)
      ) 
  }
  
  not_in_studydef <- names(data_custom_dummy)[!( names(data_custom_dummy) %in% names(data_studydef_dummy) )]
  not_in_custom  <- names(data_studydef_dummy)[!( names(data_studydef_dummy) %in% names(data_custom_dummy) )]
  
  
  if(length(not_in_custom)!=0) stop(
    paste(
      "These variables are in studydef but not in custom: ",
      paste(not_in_custom, collapse=", ")
    )
  )
  
  if(length(not_in_studydef)!=0) stop(
    paste(
      "These variables are in custom but not in studydef: ",
      paste(not_in_studydef, collapse=", ")
    )
  )
  
  # reorder columns
  data_studydef_dummy <- data_studydef_dummy[,names(data_custom_dummy)]
  
  unmatched_types <- cbind(
    map_chr(data_studydef_dummy, ~paste(class(.), collapse=", ")),
    map_chr(data_custom_dummy, ~paste(class(.), collapse=", "))
  )[ (map_chr(data_studydef_dummy, ~paste(class(.), collapse=", ")) != map_chr(data_custom_dummy, ~paste(class(.), collapse=", ")) ), ] %>%
    as.data.frame() %>% rownames_to_column()
  
  
  if(nrow(unmatched_types)>0) stop(
    #unmatched_types
    "inconsistent typing in studydef : dummy dataset\n",
    apply(unmatched_types, 1, function(row) paste(paste(row, collapse=" : "), "\n"))
  )
  
  data_extract <- data_custom_dummy 
  
} else {
  
  if (stage == "treated") {
    
    data_extract <- read_feather(studydef_path) %>%
      #because date types are not returned consistently by cohort extractor
      mutate(across(ends_with("_date"),  as.Date))
    
  } else if (stage == "potential") {
    
    data_extract <- read_feather(studydef_path) %>%
      # because date types are not returned consistently by cohort extractor
      mutate(across(ends_with("_date"), as.Date))
    
  } else if (stage == "actual") {
    
    data_extract <- read_feather(studydef_path) %>%
      #because date types are not returned consistently by cohort extractor
      mutate(across(ends_with("_date"),  as.Date)) 
    
  } else if (stage == "final") {
    
    data_extract <- read_feather(studydef_path) %>%
      #because date types are not returned consistently by cohort extractor
      mutate(across(ends_with("_date"),  as.Date))
    
  }
  
}

# add certain matching variables when stage=actual
if (stage == "actual") {
  # add: treated 
  data_extract <- data_extract %>%
    mutate(treated=0L) %>%
    # add: trial_time, matched, control, controlistreated_date to data_extract
    left_join(
      data_potential_matchstatus %>%
        filter(treated==0L),
      by=c("patient_id", "treated", "trial_date", "match_id")
      )
  
}

# summarise extracted data 
if (stage == "treated") {
  my_skim(data_extract, path = here("output", "treated", "extract", "input_treated_skim.txt"))
} else if (stage %in% c("potential", "actual")) {
  my_skim(data_extract, path = ghere("output", cohort, "matchround{matching_round}", "extract", stage, "input_control{stage}_skim.txt"))
} else if (stage == "final") {
  my_skim(data_extract, path = ghere("output", cohort, "extract", "input_control{stage}_skim.txt"))
}

# process the final dataset ----
if (stage == "final") {
  
  data_matchstatus <- read_rds(ghere("output", cohort, "matchround{n_matching_rounds_list[[cohort]]}", "actual", "data_matchstatus_allrounds.rds"))
  
  # import data for treated group and select those who were successfully matched
  if (cohort=="mrna") {
    data_treatedeligible <- bind_rows(
      read_rds(ghere("output", "pfizer", "treated", "data_treatedeligible.rds")),
      read_rds(ghere("output", "moderna", "treated", "data_treatedeligible.rds"))
    )
  } else {
    data_treatedeligible <- read_rds(ghere("output", cohort, "treated", "data_treatedeligible.rds"))
  }
  
  data_treated <- 
    left_join(
      data_matchstatus %>% filter(treated==1L),
      data_treatedeligible,
      by="patient_id"
    ) 
  
  # import extracted data from controls
  
  
  # import final dataset of matched controls, including matching variables
  # alternative to this is re-extracting everything in the study definition
  data_control <- 
    data_matchstatus %>% filter(treated==0L) %>%
    left_join(
      map_dfr(
        seq_len(n_matching_rounds_list[[cohort]]), 
        ~{read_rds(ghere("output", cohort, glue("matchround", .x), "actual", "data_successful_matchedcontrols.rds"))}
      ) %>% select(-match_id, -trial_date, -treated, -controlistreated_date), # remove to avoid clash with already-stored variables
      by=c("patient_id", "matching_round")
    ) %>%
    # merge with outcomes data
    left_join(
      data_extract,
      by=c("patient_id", "match_id", "trial_date")
    ) %>%
    mutate(
      treated=0L
    )
  
  # check final data agrees with matching status
  
  all(data_control$patient_id %in% (data_matchstatus %>% filter(treated==0L) %>% pull(patient_id)))
  all((data_matchstatus %>% filter(treated==0L) %>% pull(patient_id)) %in% data_control$patient_id)
  
  # merge treated and control groups
  data_matched <-
    bind_rows(
      data_treated,
      data_control %>% 
        # process the covariates and post-baseline variables (done previously for data_treated)
        process_covs() %>%
        process_outcome() 
    ) %>%
    select(
      ends_with("_id"),
      starts_with(other_variables),
      any_of(c(matching_variables, covariates, events_lookup$event_var, subgroups))
    )
  
  # for reading into analysis scripts
  data_matched %>%
    write_rds(here("output", cohort, "match", "data_matched.rds"), compress="gz")
  
  # for reading into tests project yaml
  # treated
  data_matched %>%
    filter(treated==1) %>%
    select(patient_id, trial_date) %>%
    write_csv(here("output", cohort, "match", "data_matched_treated.csv.gz"))
  # control
  data_matched %>%
    filter(treated==0) %>%
    select(patient_id, trial_date) %>%
    write_csv(here("output", cohort, "match", "data_matched_control.csv.gz"))
  # unique patient ids for reading into noncoviddeathcause project yaml
  data_matched %>%
    distinct(patient_id) %>%
    write_csv(here("output", cohort, "match", "data_matched_unique.csv.gz"))
  
  # summarise matched data by treatment group
  data_matched %>% filter(treated==0) %>%
    my_skim(
      path = here("output", cohort, "match", "data_matched_control_skim.txt")
    )
  data_matched %>% filter(treated==1) %>%
    my_skim(
      path = here("output", cohort, "match", "data_matched_treated_skim.txt")
    )
  
  # matching status of all treated, eligible people ----
  
  data_treatedeligible_matchstatus <- 
    left_join(
      data_treatedeligible %>% select(patient_id, vax3_date, vax3_type),
      data_matchstatus %>% filter(treated==1L),
      by="patient_id"
    ) %>%
    mutate(
      matched = if_else(is.na(match_id), 0L, 1L),
      treated = if_else(is.na(match_id), 1L, treated),
    )
  
  print(
    glue(
      "all trial dates match vaccination dates for matched, treated people: ",
      data_treatedeligible_matchstatus %>% 
        filter(matched==1L) %>%
        mutate(
          agree = trial_date==vax3_date
        ) %>% pull(agree) %>% all()
    )
  )
  
  write_rds(data_treatedeligible_matchstatus, here("output", cohort, "match", "data_treatedeligible_matchstatus.rds"), compress="gz")
  
} 

# script stops here when stage = "final"

# process data -----

## patient-level info ----

# process variables
if (stage %in% c("treated", "potential", "actual")) {
  
  # define index_date depending on stage
  stage_index_date <- case_when(
    stage=="treated" ~ "covid_vax_disease_3_date",
    stage=="potential" ~ "matching_round_date",
    stage=="actual" ~ "trial_date"
  )
  
  data_processed <- data_extract %>%
    mutate(index_date = !! sym(stage_index_date)) %>%
    process_jcvi() %>%
    process_demo() %>%
    process_pre() 
  
}

# process outcomes
if (stage == "treated") {
  
  data_processed <- data_processed %>%
    process_covs() %>%
    process_outcome() 
    
  
}

## process vaccination data ----

if (stage %in% c("treated", "potential")) {
  
  data_processed <- data_processed %>%
    process_vax(stage)
  
} else if (stage == "actual") {
  
  ### join to vax data 
  data_vax_wide <- 
    read_rds(ghere("output", cohort, "matchround{matching_round}", "process", "data_controlpotential.rds")) %>%
    select(patient_id, matches("^vax\\d"))
  
  data_processed <- data_processed %>%
    left_join(data_vax_wide, by = "patient_id") %>%
    # the following line is needed for applying the eligibility criteria: covid_vax_disease_\d_date_matches_vax\d_date
    # it has already been checked that this is true in the process_potential stage, 
    # but `covid_vax_disease_\d_date` is added to avoid having to add extra logic statements for the case when stage="actual"
    mutate(
      covid_vax_disease_1_date = vax1_date,
      covid_vax_disease_2_date = vax2_date,
      covid_vax_disease_3_date = vax3_date
      )
  
}

# summarise processed data
if (stage %in% c("treated", "potential", "actual")) {
  
  if (stage == "treated") {
    skim_path <- here("output", "treated", "process", "data_processed_skim.txt")
  } else {
    skim_path <- ghere("output", cohort, "matchround{matching_round}", stage, "data_processed_skim.txt")
  }
  my_skim(data_processed, path = skim_path)
}

####################################################################################
# Define selection criteria ----

if (stage == "treated") {
  selection_stage <- rlang::quos(
    
    has_expectedvax3type = vax3_type %in% c("pfizer", "moderna"),
    
    vax3_notbeforestartdate = case_when(
      is.na(vax3_date) ~ FALSE,
      (vax3_type=="pfizer") & (vax3_date < study_dates$pfizer$start_date) ~ FALSE,
      (vax3_type=="moderna") & (vax3_date < study_dates$moderna$start_date) ~ FALSE,
      TRUE ~ TRUE
    ),
    vax3_beforeenddate = case_when(
      is.na(vax3_date) ~ FALSE,
      (vax3_type%in%c("pfizer", "moderna")) & (vax3_date <= study_dates$recruitmentend_date) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    reliable_vax3 = vax3_notbeforestartdate & vax3_beforeenddate & 
      has_expectedvax3type & has_vaxgap2index & 
      covid_vax_disease_3_date_matches_vax_3_date,
    
  )
  
} else if (stage %in% c("potential",  "actual")) {
  
  selection_stage <- rlang::quos(
    
    vax3_notbeforeindexdate = case_when(
      is.na(vax3_date) ~ TRUE,
      vax3_date > index_date ~ TRUE,
      TRUE ~ FALSE
    ),
    
    reliable_vax3 = vax3_notbeforeindexdate & 
      has_vaxgap2index &
      covid_vax_disease_3_date_matches_vax_3_date,
    
  )
  
} 

if (stage %in% c("treated", "potential", "actual")) {
  
data_criteria <- data_processed %>%
  left_join(
    data_extract %>% select(patient_id, matches("covid_vax_disease_\\d_date")),
    by = "patient_id"
  ) %>%
  transmute(
    
    patient_id,
    is_adult = age >= 18,
    has_age = !is.na(age),
    has_sex = !is.na(sex),
    has_imd = imd_Q5 != "Unknown",
    has_ethnicity = !is.na(ethnicity),
    has_region = !is.na(region),
    isnot_hscworker = !hscworker,
    isnot_carehomeresident = !care_home_combined,
    isnot_endoflife = !endoflife,
    isnot_housebound = !housebound,
    
    # make sure vaccine dates match for all doses
    covid_vax_disease_12_date_matches_vax_12_date = case_when(
      is.na(vax1_date) | is.na(vax2_date) ~ FALSE,
      (covid_vax_disease_1_date == vax1_date) & (covid_vax_disease_2_date == vax2_date) ~ TRUE,
      TRUE ~ FALSE
    ),
    covid_vax_disease_3_date_matches_vax_3_date = case_when(
      is.na(vax3_date) & is.na(covid_vax_disease_3_date) ~ TRUE,
      covid_vax_disease_3_date == vax3_date ~ TRUE,
      TRUE ~ FALSE
    ),
    
    vax1_afterfirstvaxdate = case_when(
      is.na(vax1_date) ~ FALSE,
      (vax1_type=="pfizer") & (vax1_date >= study_dates$firstpfizer_date) ~ TRUE,
      (vax1_type=="az") & (vax1_date >= study_dates$firstaz_date) ~ TRUE,
      (vax1_type=="moderna") & (vax1_date >= study_dates$firstmoderna_date) ~ TRUE,
      TRUE ~ FALSE
    ),
    vax2_beforelastvaxdate = case_when(
      is.na(vax2_date) ~ FALSE,
      vax2_date <= study_dates$lastvax2_date ~ TRUE,
      TRUE ~ FALSE
      ),
    
    has_knownvax1 = vax1_type %in% c("pfizer", "az"),
    has_knownvax2 = vax2_type %in% c("pfizer", "az"),
    
    vax12_homologous = case_when(
      is.na(vax1_type) | is.na(vax2_type) ~ FALSE,
      vax1_type==vax2_type ~ TRUE,
      TRUE ~ FALSE
      ),
    # at least 17 days between first two vaccinations
    has_vaxgap12 = case_when(
      is.na(vax1_date) | is.na(vax2_date) ~ FALSE,
      vax2_date >= (vax1_date+17) ~ TRUE, 
      TRUE ~ FALSE
    ),
    # at least 75 days between second vaccination and index date (index_date=vax3_date for treated)
    has_vaxgap2index = case_when(
      is.na(vax2_date) | is.na(index_date) ~ FALSE,
      index_date >= (vax2_date+75) ~ TRUE,
      TRUE ~ FALSE
    ), 
    
    # read in stage specific vars
    !!! selection_stage,
    
    no_recentinfection = time_since_infection!="1-30 days",
    
    isnot_inhospital = case_when(
      is.na(index_date) ~ FALSE,
      is.na(admitted_unplanned_0_date) ~ TRUE,
      !is.na(discharged_unplanned_0_date) & (discharged_unplanned_0_date < index_date) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    c0 = is_adult, # everyone read in from study definition (all adults, but add this condition for dummy data)
    c1 = c0 & reliable_vax3,
    c2 = c1 & vax1_afterfirstvaxdate & vax2_beforelastvaxdate & has_vaxgap12 & has_knownvax1 & has_knownvax2 & vax12_homologous & covid_vax_disease_12_date_matches_vax_12_date,
    c3 = c2 & isnot_hscworker,
    c4 = c3 & isnot_carehomeresident & isnot_endoflife & isnot_housebound,
    c5 = c4 & has_age & has_sex & has_imd & has_ethnicity & has_region,
    c6 = c5 & no_recentinfection,
    c7 = c6 & isnot_inhospital,
    
    include = c7,
    
  )

data_eligible <- data_criteria %>%
  filter(include) %>%
  select(patient_id) %>%
  left_join(data_processed %>%
              select(
                ends_with("_id"),
                starts_with(other_variables),
                any_of(c(matching_variables, covariates, events_lookup$event_var, subgroups))
                ), 
            by="patient_id") %>%
  droplevels()

}

# save cohort-specific datasets ----
if (stage == "treated") {
  
  data_eligible %>% filter(vax3_type == "pfizer") %>%
    my_skim(path = here("output", "treated", "eligible", "data_eligible_pfizer_skim.txt"))
  
  write_rds(data_eligible %>% filter(vax3_type == "pfizer"), 
            here("output", "pfizer", "treated", "data_treatedeligible.rds"),
            compress="gz")
  
  data_eligible %>% filter(vax3_type == "moderna") %>%
    my_skim(path = here("output", "treated", "eligible", "data_eligible_moderna_skim.txt"))
  
  write_rds(data_eligible %>% filter(vax3_type == "moderna"), 
            here("output", "moderna", "treated", "data_treatedeligible.rds"), 
            compress="gz")
  
} else if (stage == "potential") {
  
  my_skim(data_eligible, path = ghere("output", cohort, "matchround{matching_round}", "process", "data_controlpotential_skim.txt"))
  
  write_rds(data_eligible, 
            ghere("output", cohort, "matchround{matching_round}", "process", "data_controlpotential.rds"),
            compress = "gz")
  
}


# create flowchart (only when stage="treated") ----
if (stage == "treated") {
  
  data_flowchart <- data_criteria %>%
    summarise(
      across(matches("^c\\d"), .fns=sum)
    ) %>%
    pivot_longer(
      cols=everything(),
      names_to="criteria",
      values_to="n"
    ) %>%
    mutate(
      n_exclude = lag(n) - n,
      pct_exclude = n_exclude/lag(n),
      pct_all = n / first(n),
      pct_step = n / lag(n),
      crit = str_extract(criteria, "^c\\d+"),
      criteria = fct_case_when(
        crit == "c0" ~ "Aged 18+ with 3rd dose in vaccination record", 
        crit == "c1" ~ "  pfizer or moderna for 3rd dose and no unreliable vaccination data for 3rd dose",
        crit == "c2" ~ "  pfizer or az for primary course and no unreliable vaccination data for primary course",
        crit == "c3" ~ "  not a HSC worker",
        crit == "c4" ~ "  not a care/nursing home resident, end-of-life or housebound",
        crit == "c5" ~ "  no missing demographic information",
        crit == "c6" ~ "  no evidence of covid in 30 days before third dose",
        crit == "c7" ~ "  not in hospital (unplanned) during booster vaccination",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(across(criteria, factor, labels = sapply(levels(.$criteria), glue)))
  
  write_rds(data_flowchart, here("output", "treated", "eligible", "flowchart_treatedeligible.rds"))
  
  # save rounded flowchart for release
  data_flowchart %>%
    transmute(
      criteria, crit, 
      n = ceiling_any(n, to=7),
      n_exclude = lag(n) - n,
      pct_exclude = n_exclude/lag(n),
      pct_all = n / first(n),
      pct_step = n / lag(n),
    ) %>%
    write_csv(here("output", "treated", "eligible", "flowchart_treatedeligible_rounded.csv")) 
  
}

# check matching (only when stage="actual") ----
if (stage == "actual") { 
  
  data_control <- data_eligible
  
  if (cohort == "mrna") {
    data_alltreated <- bind_rows(
      read_rds(ghere("output", "pfizer", "treated", "data_treatedeligible.rds")), 
      read_rds(ghere("output", "moderna", "treated", "data_treatedeligible.rds"))
    ) 
  } else {
    data_alltreated <- read_rds(ghere("output", cohort, "treated", "data_treatedeligible.rds")) 
  }
  
  data_treated <- 
    left_join(
      data_potential_matchstatus %>% filter(treated==1L),
      data_alltreated %>% 
        # only keep variables that are in data_control (this gets rid of outcomes and vax4 dates)
        select(any_of(names(data_control))),
      by="patient_id"
    )
  
  matching_candidates <- 
    bind_rows(data_treated, data_control) %>%
    arrange(treated, match_id, trial_date)
  
  #print missing values
  matching_candidates_missing <- map(matching_candidates, ~any(is.na(.x)))
  sort(names(matching_candidates_missing[unlist(matching_candidates_missing)]))
  
  # rematch ----
  rematch <-
    # first join on exact variables + match_id + trial_date
    inner_join(
      x=data_treated %>% select(match_id, trial_date, all_of(c(names(caliper_variables), exact_variables))),
      y=data_control %>% select(match_id, trial_date, all_of(c(names(caliper_variables), exact_variables))),
      by = c("match_id", "trial_date", exact_variables)
    ) 
  
  
  if(length(caliper_variables) >0 ){
    # check caliper_variables are still within caliper
    rematch <- rematch %>%
      bind_cols(
        map_dfr(
          set_names(names(caliper_variables), names(caliper_variables)),
          ~ abs(rematch[[str_c(.x, ".x")]] - rematch[[str_c(.x, ".y")]]) <= caliper_variables[.x]
        )
      ) %>%
      # dplyr::if_all not in opensafely version of dplyr so use filter_at instead
      # filter(if_all(
      #   all_of(names(caliper_variables))
      # )) 
      filter_at(
        all_of(names(caliper_variables)),
        all_vars(.)
      )
    
    
  } 
  
  rematch <- rematch %>%
    select(match_id, trial_date) %>%
    mutate(matched=1)
  
  data_successful_match <-
    matching_candidates %>%
    inner_join(rematch, by=c("match_id", "trial_date", "matched")) %>%
    mutate(
      matching_round = matching_round
    ) %>%
    arrange(trial_date, match_id, treated)
  
  
  ###
  
  matchstatus_vars <- c("patient_id", "match_id", "trial_date", "matching_round", "treated", "controlistreated_date")
  
  data_successful_matchstatus <- 
    data_successful_match %>% 
    # keep all variables from the processed data as they are required for adjustments in the cox model
    select(all_of(matchstatus_vars), everything())
  
  ## size of dataset
  print("data_successful_match treated/untreated numbers")
  table(treated = data_successful_matchstatus$treated, useNA="ifany")
  
  
  ## how many matches are lost?
  
  print(glue("{sum(data_successful_matchstatus$treated)} matched-pairs kept out of {sum(data_potential_matchstatus$treated)} 
           ({round(100*(sum(data_successful_matchstatus$treated) / sum(data_potential_matchstatus$treated)),2)}%)
           "))
  
  
  ## pick up all previous successful matches ----
  
  if(matching_round>1){
    
    data_matchstatusprevious <- 
      read_rds(ghere("output", cohort, "matchround{matching_round-1}", "actual", "data_matchstatus_allrounds.rds"))
    
    data_matchstatus_allrounds <- 
      data_successful_matchstatus %>% 
      select(all_of(matchstatus_vars)) %>%
      bind_rows(data_matchstatusprevious) 
    
  } else{
    data_matchstatus_allrounds <- 
      data_successful_matchstatus %>%
      select(all_of(matchstatus_vars))
  }
  
  write_rds(data_matchstatus_allrounds, ghere("output", cohort, "matchround{matching_round}", "actual", "data_matchstatus_allrounds.rds"), compress="gz")
  
  
  # output all control patient ids for finalmatched study definition
  data_matchstatus_allrounds %>%
    mutate(
      trial_date=as.character(trial_date)
    ) %>%
    filter(treated==0L) %>% #only interested in controls as all
    write_csv(ghere("output", cohort, "matchround{matching_round}", "actual", "cumulative_matchedcontrols.csv.gz"))
  
  ## size of dataset
  print("data_matchstatus_allrounds treated/untreated numbers")
  table(treated = data_matchstatus_allrounds$treated, useNA="ifany")
  
  
  
  ## duplicate IDs
  data_matchstatus_allrounds %>% group_by(treated, patient_id) %>%
    summarise(n=n()) %>% group_by(treated) %>% summarise(ndups = sum(n>1)) %>%
    print()
  
  my_skim(data_eligible, path = ghere("output", cohort, "matchround{matching_round}", "actual", "data_successful_matchedcontrols_skim.txt"))
  write_rds(data_successful_matchstatus %>% filter(treated==0L), ghere("output", cohort, "matchround{matching_round}", "actual", "data_successful_matchedcontrols.rds"), compress="gz")
  
  ## size of dataset
  print("data_successful_match treated/untreated numbers")
  table(treated = data_successful_matchstatus$treated, useNA="ifany")
  
}

