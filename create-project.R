library("tidyverse")
library("yaml")
library("here")
library("glue")
#library("rlang")
 
## import local functions and parameters ---
source(here("analysis", "design.R"))

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

## actions for a single match round ----
action_1matchround <- function(match_round){

  control_extract_date <- study_dates[["control_extract"]][match_round]
  
  match_actions <- splice(
    action(
      name = glue("extract_controlpotential_{match_round}"),
      run = glue(
        "cohortextractor:latest generate_cohort",
        " --study-definition study_definition_controlpotential",
        " --output-file output/matchround{match_round}/controlpotential/extract/input_controlpotential.feather",
        " --param match_round={match_round}",
        " --param index_date={control_extract_date}"
      ),
      needs = c(
        "design",
        "process_initial",
        if(match_round>1) {glue("process_controlactual_{match_round-1}")} else {NULL}
      ) %>% as.list,
      highly_sensitive = lst(
        cohort = glue("output/matchround{match_round}/controlpotential/extract/input_controlpotential.feather")
      )
    ),

    action(
      name = glue("process_controlpotential_{match_round}"),
      run = glue("r:latest analysis/process/process_stage.R"),
      arguments = c("controlpotential", match_round),
      needs = namelesslst(
        "dummydata_stage",
        "process_initial",
        glue("extract_controlpotential_{match_round}"),
      ),
      highly_sensitive = lst(
        eligible_rds = glue("output/matchround{match_round}/controlpotential/eligible/*.rds")
      ),
      moderately_sensitive = lst(
        data_extract_skim = glue("output/matchround{match_round}/controlpotential/extract/*.txt"),
        data_processed_skim = glue("output/matchround{match_round}/controlpotential/process/*.txt"),
        data_controlpotential_skim = glue("output/matchround{match_round}/controlpotential/eligible/*.txt")
      )
    ),

    action(
      name = glue("match_controlpotential_{match_round}"),
      run = glue("r:latest analysis/match/match_controlpotential.R"),
      arguments = match_round,
      needs = c(
        "process_treated",
        glue("process_controlpotential_{match_round}"),
        if(match_round>1) {glue("process_controlactual_{match_round-1}")} else {NULL}
      ) %>% as.list,
      highly_sensitive = lst(
        rds = glue("output/matchround{match_round}/controlpotential/match/*.rds"),
        csv = glue("output/matchround{match_round}/controlpotential/match/*.csv.gz"),
      )
    ),

    action(
      name = glue("extract_controlactual_{match_round}"),
      run = glue(
        "cohortextractor:latest generate_cohort",
        " --study-definition study_definition_controlactual",
        " --output-file output/matchround{match_round}/controlactual/extract/input_controlactual.feather",
        " --param match_round={match_round}",
      ),
      needs = namelesslst(
        "design",
        glue("match_controlpotential_{match_round}"),
      ),
      highly_sensitive = lst(
        cohort = glue("output/matchround{match_round}/controlactual/extract/input_controlactual.feather")
      )
    ),


    action(
      name = glue("process_controlactual_{match_round}"),
      run = glue("r:latest analysis/process/process_stage.R"),
      arguments = c("controlactual", match_round),
      needs = c(
        "process_initial",
        "dummydata_stage",
        "process_treated",
        glue("match_controlpotential_{match_round}"),
        glue("extract_controlpotential_{match_round}"),  # this is only necessary for the dummy data
        glue("process_controlpotential_{match_round}"), # this is necessary for the vaccine data
        glue("extract_controlactual_{match_round}"),
        if(match_round>1){glue("process_controlactual_{match_round-1}")} else {NULL}
      ) %>% as.list,
      highly_sensitive = as.list(c(
          rds = glue("output/matchround{match_round}/controlactual/match/*.rds"),
          test = if(match_round==n_match_rounds) {"output/incremental/match/*.csv.gz"} else NULL
        )),
      moderately_sensitive = lst(
        input_controlactual_skim = glue("output/matchround{match_round}/controlactual/extract/*.txt"),
        eligible_skim = glue("output/matchround{match_round}/controlactual/eligible/*.txt"),
        process_skim = glue("output/matchround{match_round}/controlactual/process/*.txt"),
      )
    )

  )

  match_actions <- match_actions[sapply(match_actions, function(x) !is.null(x))]

  return(match_actions)

}


extract_outcomes <- function(group) {
  
  action(
    name = glue("extract_outcomes_{group}"),
    run = glue(
      "cohortextractor:latest generate_cohort",
      glue(" --study-definition study_definition_outcomes"),
      glue(" --output-file output/outcomes/input_outcomes_{group}.feather"),
      " --param group={group}"
    ),
    needs = as.list(c(
      "design",
      if (group=="alltreated") {"process_treated"} else {NULL},
      if (group=="matchcontrol") {glue("process_controlactual_{n_match_rounds}")} else {NULL}
    )),
    highly_sensitive = lst(
      cohort = glue("output/outcomes/input_outcomes_{group}.feather")
    )
  )
  
}


actions_model <- function(effect, subgroup, outcome, match_strategy) {
  
  needs_list <- list()
  needs_list[["km"]] <- c("process_treated", "extract_outcomes_alltreated")
  
  if (effect == "comparative") {
    needs_list[["km"]] <- c(needs_list[["km"]], "match_comparative")
    needs_list[["cox_adj"]] <- c(needs_list[["km"]], "extract_covs_alltreated")
  }
  
  if (effect == "incremental") {
    needs_list[["km"]] <- c(
      needs_list[["km"]], 
      map_chr(1:n_match_rounds, ~glue("process_controlactual_{.x}")),
      "extract_outcomes_matchcontrol"
    )
    needs_list[["cox_adj"]] <- c(
      needs_list[["km"]], 
      "extract_covs_alltreated", 
      "extract_covs_matchcontrol")
  }
  
  needs_list[["cox_unadj"]] <- needs_list[["km"]]
  
  actions <- splice(

    comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # ",
            glue("subgroup={subgroup}; outcome={outcome};"),
            "# # # # # # # # # # # # # # # # # # # # # # # # # # # "),

    # km
    action(
      name = glue("km_{effect}_{subgroup}_{outcome}"),
      run = glue("r:latest analysis/model/km.R"),
      arguments = c(effect, subgroup, outcome, match_strategy),
      needs = needs_list[["km"]],
      moderately_sensitive= lst(
        rds = glue("output/{effect}/model/km/{subgroup}/{outcome}/*.csv"),
        png = glue("output/{effect}/model/km/{subgroup}/{outcome}/*.png")
      )
    )
    
  )
  
  # if (subgroup == "all") {
    
    actions <- splice(
      
      actions,
      
      # cox
      expand_grid(
        model=c("cox_unadj", "cox_adj"),
      ) %>%
        pmap(
          function(model) {
            
            action(
              name = glue("{model}_{effect}_{subgroup}_{outcome}"),
              run = "r:latest analysis/model/cox.R",
              arguments = c(effect, model, subgroup, outcome, match_strategy),
              needs = needs_list[[model]],
              moderately_sensitive= lst(
                csv = glue("output/{effect}/model/{model}/{subgroup}/{outcome}/*.csv")
              )
            )
            
          }
        ) %>%
        unlist(recursive = FALSE)
      
    )
      
  # }
  
  return(actions)
    
}


action_table1 <- function(effect, match_strategy = NULL) {
  
  needs_list <- "process_treated"
  
  if (effect == "comparative") needs_list <- c(needs_list, "match_comparative")
  
  if (effect == "incremental") {
    
    needs_list <- c(
      needs_list, 
      map_chr(1:n_match_rounds, ~glue("process_controlactual_{.x}"))
      )
    
  }
  
  action(
    name = glue("table1_{effect}"),
    run = "r:latest analysis/report/table1.R",
    arguments = c(effect, match_strategy),
    needs = as.list(needs_list),
    moderately_sensitive = lst(
      csv = glue("output/{effect}/table1/table1_{effect}_midpoint{threshold}.csv")
    )
  )
}

action_coverage <- function(effect, match_strategy){
  
  needs_list <- "process_treated"
  
  if (effect == "comparative") needs_list <- c(needs_list, "match_comparative")
  
  if (effect == "incremental") {
    
    needs_list <- c(needs_list, glue("process_controlactual_{n_match_rounds}"))
    
  }
  
  out <- action(
    name = glue("coverage_{effect}"),
    run = "r:latest analysis/match/coverage.R",
    arguments = c(effect, match_strategy),
    needs = as.list(needs_list),
    moderately_sensitive= lst(
      csv= glue("output/{effect}/coverage/*.csv"),
      png= glue("output/{effect}/coverage/*.png"),
    )
  )
  
  if (effect == "incremental") {
    
    needs_list <- c(needs_list, "process_initial")
    
  }
  
  out <- splice(
    out,
    action(
      name = glue("flowchart_{effect}"),
      run = "r:latest analysis/match/match_flowchart.R",
      arguments = c(effect, match_strategy),
      needs = as.list(needs_list),
      moderately_sensitive= lst(
        csv= glue("output/{effect}/flowchart/*.csv"),
      )
    )
  )
  
  return(out)
  
}


actions_match_strategy <- function(effect, match_strategy, include_models=FALSE) {
  
  if (effect == "comparative") {
    
    actions_match <- splice(
      
      action(
        name = "match_comparative",
        run = "r:latest analysis/match/match_comparative.R",
        arguments = match_strategy,
        needs = namelesslst(
          "process_initial",
          "process_treated"
        ),
        highly_sensitive = lst(
          rds = "output/comparative/match/*.rds"
        )
      ),
      
      # table1 for matched data for comparative effectiveness, match variables
      action_table1(
        effect = "comparative",
        match_strategy = match_strategy
      ),
      
      # match coverage
      action_coverage(
        effect = "comparative",
        match_strategy = match_strategy
      )
      
    )
    
    
  }
  if (effect == "incremental") {
    
    actions_match <- splice(
      comment("# # # # # # # # # # # # # # # # # # #", 
              "Extract, process and match control data", 
              "for incremental effectiveness analysis",
              "# # # # # # # # # # # # # # # # # # #"),
      
      map(seq_len(n_match_rounds), ~action_1matchround(.x)) %>% flatten(),
      
      # table1 for matched data for incremental effectiveness, match variables
      action_table1(
        effect = "incremental",
        match_strategy = match_strategy
      ),
      
      # match coverage
      action_coverage(
        effect = "incremental",
        match_strategy = match_strategy
        ),
      
      # extract outcome variables for matched controls
      extract_outcomes(group = "matchcontrol")
    )
    
  }
  
  actions <- splice(
    
    comment("# # # # # # # # # # # # # # # # # # #", 
            glue("Match treated for {effect} effectiveness"), 
            "# # # # # # # # # # # # # # # # # # #"),
    
    actions_match
    
  )
  
  
  if (include_models) {
    
    actions <- splice(
      
      actions,
      
      comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # ",
              glue("Fit models to estimate {effect} effectiveness"),
              "# # # # # # # # # # # # # # # # # # # # # # # # # # # "),
      expand_grid(
        subgroup=subgroups,
        outcome=outcomes,
      ) %>%
        pmap(
          function(subgroup, outcome) {
            actions_model(
              effect = effect,
              subgroup = subgroup,
              outcome = outcome,
              match_strategy = match_strategy
            )
          }
        ) %>%
        unlist(recursive = FALSE),
      
      comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # ",
              "Combine all model outputs",
              "# # # # # # # # # # # # # # # # # # # # # # # # # # # "),
      action(
        name = "combine_model",
        run = glue("r:latest analysis/report/combine_model.R"),
        arguments = c(effect, match_strategy),
        needs = splice(
          as.list(
            glue_data(
              .x=model_args,
              "{model}_{effect}_{subgroup}_{outcome}"
            )
          )
        ),
        moderately_sensitive = lst(
          # update paths for effect and matching strategy!!
          rds = glue("output/report/model/*.csv"),
          png = glue("output/report/model/*.png"),
        )
      )
      
    )
    
  }
    
 return(actions) 
  
}

# specify project ----

## defaults ----
defaults_list <- lst(
  version = "3.0",
  expectations= lst(population_size=100000L)
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
          "Extract and process initial data", 
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
      data_eligible = "output/initial/eligible/*.csv.gz",
      data_vax = "output/initial/eligible/*.rds",
    ),
    moderately_sensitive = lst(
      flowchart = "output/initial/flowchart/*.csv",
    )
  ),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Extract and process treated data", 
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
  
  action(
    name = "dummydata_stage",
    run = "r:latest analysis/dummydata/dummydata_stage.R",
    needs = namelesslst(
      "dummydata_initial",
      "process_initial",
      "extract_treated"
    ),
    highly_sensitive = lst(
      dummydata_treated = "output/treated/dummydata/*.feather",
      dummydata_controlpotential = "output/matchround1/controlpotential/dummydata/*.feather"
    ),
  ),
  
  # all treated people
  action(
    name = "process_treated",
    run = "r:latest analysis/process/process_stage.R",
    arguments = "treated",
    needs = namelesslst(
      "process_initial",
      "extract_treated",
      "dummydata_stage"
    ),
    highly_sensitive = lst(
      eligiblerds = "output/treated/eligible/*.rds",
      eligiblecsv = "output/treated/eligible/*.csv.gz"
    ),
    moderately_sensitive = lst(
      flowchart_treated = "output/treated/flowchart/*.csv",
      flowchart_combined = "output/report/flowchart_combined*.csv",
      extract_treated_skim = "output/treated/extract/*.txt",
      data_processed_skim = "output/treated/process/*.txt",
      data_eligible_skim = "output/treated/eligible/*.txt"
    )
  ),
  
  # table1 for all treated (including unmatched), all variables
  action_table1(
    effect = "treated"
  ),
  
  # extract outcome data for all treated people
  # (not incorporated into study_definition_treated so that it can be updated when more outcome data available)
  extract_outcomes(group = "alltreated"),
  
  ####################################################
  # all actions for comparative effectiveness analysis
  ####################################################
  actions_match_strategy(
    effect = "comparative", 
    match_strategy = "A",
    include_models = FALSE
    ),
  
  ####################################################
  # all actions for incremental effectiveness analysis
  ####################################################
  actions_match_strategy(
    effect = "incremental", 
    match_strategy = "A",
    include_models = FALSE
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

