library("tidyverse")
library("yaml")
library("here")
library("glue")
#library("rlang")
 
## import local functions and parameters ---
source(here("analysis", "design.R"))

# source action_* functions
action_functions <- list.files(here("lib", "functions"), pattern = "action_")
for (a in action_functions) {
  source(here("lib", "functions", a))
}

# create action functions ----

## create comment function ----
comment <- function(...) {
  list_comments <- list(...)
  comments <- map(list_comments, ~paste0("## ", ., " ##"))
  comments
}

## create a list of actions
lapply_actions <- function(X, FUN) {
  unlist(
    lapply(
      X,
      FUN
    ),
    recursive = FALSE
  )
}

## create function to convert comment "actions" in a yaml string into proper comments
convert_comment_actions <-function(yaml.txt) {
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1")  %>%
    #str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}

## generic action function ----
action <- function(
  name,
  run,
  arguments=NULL,
  needs=NULL,
  highly_sensitive=NULL,
  moderately_sensitive=NULL,
  ... # other arguments / options for special action types
) {

  outputs <- list(
    highly_sensitive = highly_sensitive,
    moderately_sensitive = moderately_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL

  action <- list(
    run = paste(c(run, arguments), collapse=" "),
    needs = needs,
    outputs = outputs,
    ... = ...
  )
  action[sapply(action, is.null)] <- NULL

  action_list <- list(name = action)
  names(action_list) <- name

  action_list
}

namelesslst <- function(...){
  unname(lst(...))
}

needs_model_riskscore <- function(match_strategy) {
  names_actions <- NULL
  if (match_strategy %in% c("none", "riskscore_i")) {
    names_actions <- c(
      names_actions,
      # across the 3 levels of agegroup_match
      sapply(1:3, function(x) glue("fit_model_riskscore_i_{x}"))
    )
  }
  return(names_actions)
}

# specify project ----

## defaults ----
defaults_list <- lst(
  version = "3.0",
  expectations= lst(population_size=1000L)
)

## actions ----
actions_list <- splice(

  comment("# # # # # # # # # # # # # # # # # # #",
          "DO NOT EDIT project.yaml DIRECTLY",
          "This file is created by create-project.R",
          "Edit and run create-project.R to update the project.yaml",
          "# # # # # # # # # # # # # # # # # # #",
           " "
          ),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Preliminaries", 
          "# # # # # # # # # # # # # # # # # # #"),
  action(
    name = "design",
    run = glue("r:latest analysis/design.R"),
    moderately_sensitive = lst(
      lib = glue("lib/design/*.json")
    ),
  ),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Extract and process initial data.", 
          "The initial extract comprises vacination data",
          "and static variables (i.e. not defined at a timepoint,",
          "e.g. sex, ethnicity etc).",
          "# # # # # # # # # # # # # # # # # # #"),
  
  action(
    name = "extract_initial",
    run = glue(
      "cohortextractor:latest generate_cohort", 
      " --study-definition study_definition_initial", 
      " --output-file output/initial/extract/input_initial.feather",
    ),
    needs = namelesslst(
      "design"
    ),
    highly_sensitive = lst(
      extract = "output/initial/extract/input_initial.feather"
    ),
  ),
  
  action(
    name = "dummydata_initial",
    run = "r:latest analysis/dummydata/dummydata_initial.R",
    needs = namelesslst(
      "extract_initial"
    ),
    highly_sensitive = lst(
      dummydata = "output/initial/dummydata/*.feather"
    ),
  ),
  
  action(
    name = "process_initial",
    run = "r:latest analysis/process/process_initial.R",
    needs = namelesslst(
      "extract_initial", "dummydata_initial"
    ),
    highly_sensitive = lst(
      data_vax = "output/initial/processed/*.rds",
      data_eligible_csv = "output/initial/eligible/*.csv.gz"
    ),
    moderately_sensitive = lst(
      flowchart = "output/initial/flowchart/*.csv",
    )
  ),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Extract and process riskscore_i data.", 
          "These data are extracted from a period prior",
          "to the start of the main study period, and are",
          "used to fit a linear predictor of death. This",
          "model is then used to predict the probability of",
          "death during follow-up (riskscore_i), and individuals",
          "are matched on these probabilities.",
          "# # # # # # # # # # # # # # # # # # #"),
  
  action(
    name = "extract_riskscore_i",
    run = glue(
      "cohortextractor:latest generate_cohort", 
      " --study-definition study_definition_riskscore_i", 
      " --output-file output/riskscore_i/extract/input_riskscore_i.feather",
    ),
    needs = namelesslst("design"),
    highly_sensitive = lst(
      extract = "output/riskscore_i/extract/input_riskscore_i.feather"
    ),
  ),
  
  action(
    name = "dummydata_stage",
    run = "r:latest analysis/dummydata/dummydata_stage.R",
    needs = namelesslst(
      "dummydata_initial",
      "process_initial"
    ),
    highly_sensitive = lst(
      dummydata_treated = "output/treated/dummydata/*.feather",
      dummydata_controlpotential = "output/incremental_none/matchround1/controlpotential/dummydata/*.feather"
    ),
  ),
  
  action(
    name = "process_riskscore_i",
    run = "r:latest analysis/process/process_stage.R",
    # set match_strategy="none" and match_round=0 to keep the code happy,
    # but these don't really mean anything here
    arguments = c("riskscore_i", "riskscore_i", 0),
    needs = namelesslst(
      "process_initial",
      "extract_riskscore_i",
      "dummydata_stage"
    ),
    highly_sensitive = lst(
      eligiblerds = "output/riskscore_i/eligible/*.rds",
    ),
    moderately_sensitive = lst(
      flowchart = "output/riskscore_i/flowchart/*.csv",
      extract_skim = "output/riskscore_i/extract/*.txt",
      data_processed_skim = "output/riskscore_i/processed/*.txt",
      data_eligible_skim = "output/riskscore_i/eligible/*.txt"
    )
  ),
  
  map(
    1:3, # across the 3 levels of agegroup_match
    ~action(
      name = glue("fit_model_riskscore_i_", .x),
      run = "r:latest analysis/riskscore/model_fit.R",
      arguments = .x,
      needs = namelesslst("process_riskscore_i"),
      highly_sensitive = lst(
        model = glue("output/riskscore_i/agegroup_", .x, "/model_agegroup_*.rds"),
        percentile_breaks = glue("output/riskscore_i/agegroup_", .x, "/percentile_breaks_*.rds")
      ),
      moderately_sensitive = lst(
        dist_predictions = glue("output/riskscore_i/agegroup_", .x, "/dist_predictions_*.png"),
        calibration = glue("output/riskscore_i/agegroup_", .x, "/calibration_*.png"),
        binned_residuals = glue("output/riskscore_i/agegroup_", .x, "/binned_residuals_*.png")
      )
    )
  ) %>% 
    flatten(),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Extract and process treated data.", 
          "# # # # # # # # # # # # # # # # # # #"),
  
  action(
    name = "extract_treated",
    run = glue(
      "cohortextractor:latest generate_cohort", 
      " --study-definition study_definition_treated", 
      " --output-file output/treated/extract/input_treated.feather",
    ),
    needs = namelesslst(
      "process_initial"
    ),
    highly_sensitive = lst(
      extract = "output/treated/extract/input_treated.feather"
    ),
  ),
  
  # all treated people
  action(
    name = "process_treated",
    run = "r:latest analysis/process/process_stage.R",
    # set match_strategy="none" and match_round=0 to keep the code happy,
    # but these don't really mean anything here
    arguments = c("treated", "none", 0),
    needs = c(
      "process_initial",
      "extract_treated",
      "dummydata_stage",
      needs_model_riskscore("none")
    ) %>% as.list(),
    highly_sensitive = lst(
      eligiblerds = "output/treated/eligible/*.rds",
      eligiblecsv = "output/treated/eligible/*.csv.gz"
    ),
    moderately_sensitive = lst(
      riskscore_i_plots = "output/treated/riskscore_i/plot_*.png",
      flowchart_treated = "output/treated/flowchart/*.csv",
      flowchart_combined = "output/report/flowchart_combined*.csv",
      extract_treated_skim = "output/treated/extract/*.txt",
      data_processed_skim = "output/treated/processed/*.txt",
      data_eligible_skim = "output/treated/eligible/*.txt"
    )
  ),
  
  # table1 for all treated (including unmatched), all variables
  action_table1(
    effect = "treated",
    match_strategy = "none"
  ),
  
  ####################################################
  # COMPARATIVE
  ####################################################
  
  action_match_strategy(
    effect = "comparative",
    match_strategy = "a",
    include_models = FALSE
    ),
  
  action_match_strategy(
    effect = "comparative",
    match_strategy = "riskscore_i",
    include_models = FALSE
  ),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Extract adjustment and outcome variables for all treated people",
          "# # # # # # # # # # # # # # # # # # #"),
  # extract adjustment variables for final matched controls
  action(
    name = glue("extract_final_treated"),
    run = glue(
      "cohortextractor:latest generate_cohort",
      glue(" --study-definition study_definition_final"),
      glue(" --output-file output/treated/extract/input_final_treated.feather"),
      " --param effect=comparative",
      " --param match_strategy=none"
    ),
    needs = namelesslst("design", "process_treated"),
    highly_sensitive = lst(
      cohort = glue("output/treated/extract/input_final_treated.feather")
    )
  ),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          glue("Fit models for comparative effectiveness match_strategy = a"),
          "# # # # # # # # # # # # # # # # # # #"),
  
  expand_grid(
    match_strategy = c("a", "riskscore_i"),
    subgroup = subgroups,
    outcome = outcomes,
  ) %>%
    pmap(
      function(match_strategy, subgroup, outcome) {
        action_model(
          effect = "comparative",
          match_strategy = match_strategy,
          subgroup = subgroup,
          outcome = outcome
        )
      }
    ) %>%
    unlist(recursive = FALSE),
  
  action_combine_model_outputs(
    effect = "comparative",
    match_strategy = "a"
  ),
  
  action_combine_model_outputs(
    effect = "comparative",
    match_strategy = "riskscore_i"
  ),
  
  ####################################################
  # INCREMENTAL
  ####################################################
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Extract and process controlpotential1 data.", 
          "This can be done once for all matching strategies.",
          "# # # # # # # # # # # # # # # # # # #"),
  
  action_controlpotential(
    match_strategy = "none",
    match_round = 1
    ),
  
  action_match_strategy(
    effect = "incremental",
    match_strategy = "a",
    include_models = TRUE
    ),
  
  action_match_strategy(
    effect = "incremental",
    match_strategy = "riskscore_i",
    include_models = TRUE
  ),
  
  comment("#### End ####")
)

project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)

## convert list to yaml, reformat comments and whitespace ----
thisproject <- as.yaml(project_list, indent=2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1")


# if running via opensafely, check that the project on disk is the same as the project created here:
if (Sys.getenv("OPENSAFELY_BACKEND") %in% c("expectations", "tpp")){

  thisprojectsplit <- str_split(thisproject, "\n")
  currentproject <- readLines(here("project.yaml"))

  stopifnot("project.yaml is not up-to-date with create-project.R.  Run create-project.R before running further actions." = identical(thisprojectsplit, currentproject))

# if running manually, output new project as normal
} else if (Sys.getenv("OPENSAFELY_BACKEND") %in% c("")){

  ## output to file ----
  writeLines(thisproject, here("project.yaml"))
  #yaml::write_yaml(project_list, file =here("project.yaml"))
  
  ## grab all action names and send to a txt file
  
  names(actions_list) %>% tibble(action=.) %>%
    mutate(
      model = action==""  & lag(action!="", 1, TRUE),
      model_number = cumsum(model),
    ) %>%
    group_by(model_number) %>%
    summarise(
      sets = str_trim(paste(action, collapse=" "))
    ) %>% pull(sets) %>%
    paste(collapse="\n") %>%
    writeLines(here("actions.txt"))

# fail if backend not recognised
} else {
  stop("Backend not recognised by create.project.R script")
}

