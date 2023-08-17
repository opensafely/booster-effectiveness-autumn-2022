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

# action for controlpotential steps
# keep these outside action_1matchround(), as they only need to be run once for 
# round 1 as are independent of match_strategy
action_controlpotential <- function(match_strategy, match_round) {
  
  control_extract_date <- study_dates[["control_extract"]][match_round]
  
  controlpotential_path <- "output/incremental_{match_strategy}/matchround{match_round}/controlpotential/extract/input_controlpotential.feather"
  
  riskscore_i_plots <- 
    if(
      (match_strategy == "controlpotential" & match_round ==1) | 
      match_strategy == "riskscore_i"
    ) {
      glue("output/incremental_{match_strategy}/matchround{match_round}/controlpotential/riskscore_i/plot_*.png")
      } else NULL
    
  actions <- splice(
    
    action(
      name = glue("extract_controlpotential_{match_strategy}_{match_round}"),
      run = glue(
        "cohortextractor:latest generate_cohort",
        " --study-definition study_definition_controlpotential",
        " --output-file ", controlpotential_path,
        " --param match_strategy={match_strategy}",
        " --param match_round={match_round}",
        " --param index_date={control_extract_date}"
      ),
      needs = c(
        "design",
        if(match_round == 1) "process_initial",
        if(match_round > 1) glue("match_controlactual_{match_strategy}_{match_round-1}")
      ) %>% as.list,
      highly_sensitive = lst(
        cohort = glue(controlpotential_path)
      )
    ),
    
    action(
      name = glue("process_controlpotential_{match_strategy}_{match_round}"),
      run = "r:latest analysis/process/process_stage.R",
      arguments = c("controlpotential", match_strategy, match_round),
      needs = c(
        "dummydata_stage",
        "process_initial",
        glue("extract_controlpotential_{match_strategy}_{match_round}"),
        needs_model_riskscore(match_strategy)
      ) %>% as.list(),
      highly_sensitive = lst(
        eligible_rds = glue("output/incremental_{match_strategy}/matchround{match_round}/controlpotential/eligible/*.rds")
      ),
      moderately_sensitive = c(
        data_extract_skim = glue("output/incremental_{match_strategy}/matchround{match_round}/controlpotential/extract/*.txt"),
        data_processed_skim = glue("output/incremental_{match_strategy}/matchround{match_round}/controlpotential/processed/*.txt"),
        data_controlpotential_skim = glue("output/incremental_{match_strategy}/matchround{match_round}/controlpotential/eligible/*.txt"),
        riskscore_i_plots = riskscore_i_plots
      ) %>% as.list()
    )
  )
  
}

## actions for a single match round ----
action_1matchround <- function(match_strategy, match_round) {
  
  match_strategy_obj <- get(paste0("match_strategy_", match_strategy))
  n_match_rounds <- match_strategy_obj$n_match_rounds
  
  control_extract_date <- study_dates[["control_extract"]][match_round]
  
  controlactual_path <- "output/incremental_{match_strategy}/matchround{match_round}/controlactual/extract/input_controlactual.feather"
  
  riskscore_i_plots <- 
    if(match_strategy == "riskscore_i") {
      glue("output/incremental_{match_strategy}/matchround{match_round}/controlactual/riskscore_i/plot_*.png")
    } else NULL
  
  match_actions <- splice(
    
    comment(
      "Extract, process and match controlpotential data.",
      glue("Match_round = {match_round}")
      ),
    
    if (match_round > 1) {
      action_controlpotential(match_strategy, match_round)
    },

    action(
      name = glue("match_controlpotential_{match_strategy}_{match_round}"),
      run = glue("r:latest analysis/match/match_controlpotential.R"),
      arguments = c(match_strategy, match_round),
      needs = c(
        "process_treated",
        if (match_round==1) {
          glue("process_controlpotential_none_{match_round}")
        } else {
          c(glue("process_controlpotential_{match_strategy}_{match_round}"),
            glue("match_controlactual_{match_strategy}_{match_round-1}"))
        },
        needs_model_riskscore(match_strategy)
      ) %>% as.list,
      highly_sensitive = lst(
        rds = glue("output/incremental_{match_strategy}/matchround{match_round}/controlpotential/match/*.rds"),
        csv = glue("output/incremental_{match_strategy}/matchround{match_round}/controlpotential/match/*.csv.gz"),
      )
    ),
    
    comment(
      "Extract and process controlactual data",
      "(matches checked and rematched in the process action)."
      ),

    action(
      name = glue("extract_controlactual_{match_strategy}_{match_round}"),
      run = glue(
        "cohortextractor:latest generate_cohort",
        " --study-definition study_definition_controlactual",
        " --output-file ", controlactual_path,
        " --param match_strategy={match_strategy}",
        " --param match_round={match_round}",
      ),
      needs = namelesslst(
        "design",
        glue("match_controlpotential_{match_strategy}_{match_round}"),
      ),
      highly_sensitive = lst(
        cohort = glue(controlactual_path)
      )
    ),

    action(
      name = glue("process_controlactual_{match_strategy}_{match_round}"),
      run = glue("r:latest analysis/process/process_stage.R"),
      arguments = c("controlactual", match_strategy, match_round),
      needs = c(
        "process_initial",
        "dummydata_stage",
        glue("match_controlpotential_{match_strategy}_{match_round}"),
        glue("extract_controlactual_{match_strategy}_{match_round}"),
        needs_model_riskscore(match_strategy)
      ) %>% as.list,
      highly_sensitive = lst(
        data_eligible = glue("output/incremental_{match_strategy}/matchround{match_round}/controlactual/eligible/*.rds"),
      ),
      moderately_sensitive = c(
        input_skim = glue("output/incremental_{match_strategy}/matchround{match_round}/controlactual/extract/*.txt"),
        eligible_skim = glue("output/incremental_{match_strategy}/matchround{match_round}/controlactual/eligible/*.txt"),
        process_skim = glue("output/incremental_{match_strategy}/matchround{match_round}/controlactual/processed/*.txt"),
        riskscore_i_plots = riskscore_i_plots
      ) %>% as.list()
    ),
    
    action(
      name = glue("match_controlactual_{match_strategy}_{match_round}"),
      run = glue("r:latest analysis/match/match_controlactual.R"),
      arguments = c(match_strategy, match_round),
      needs = c(
        "process_initial",
        "process_treated",
        if (match_round > 1) map_chr(1:(match_round-1), ~glue("match_controlactual_{match_strategy}_", .x)),
        glue("match_controlpotential_{match_strategy}_{match_round}"),
        glue("process_controlactual_{match_strategy}_{match_round}")
      ) %>% as.list(),
      highly_sensitive = as.list(c(
        rds = glue("output/incremental_{match_strategy}/matchround{match_round}/controlactual/match/*.rds"),
        csv = if (match_round < n_match_rounds) {glue("output/incremental_{match_strategy}/matchround{match_round}/controlactual/match/*.csv.gz")} else NULL,
        final = if(match_round==n_match_rounds) {glue("output/incremental_{match_strategy}/match/*.csv.gz")} else NULL
      ))
    )

  )

  match_actions <- match_actions[sapply(match_actions, function(x) !is.null(x))]

  return(match_actions)

}


extract_outcomes <- function(match_strategy) {
  
  match_strategy_obj <- get(paste0("match_strategy_", match_strategy))
  n_match_rounds <- match_strategy_obj$n_match_rounds
  
  
    if (match_strategy=="alltreated") {
      needs <- "process_treated"
      outpath <- glue("output/treated/outcomes/input_outcomes.feather")
    } else {
      needs <- glue("process_controlactual_{match_strategy}_{n_match_rounds}")
      outpath <- glue("output/incremental_{match_strategy}/outcomes/input_outcomes.feather")
    }
  
  action(
    name = glue("extract_outcomes_{match_strategy}"),
    run = glue(
      "cohortextractor:latest generate_cohort",
      glue(" --study-definition study_definition_outcomes"),
      glue(" --output-file {outpath}"),
      " --param match_strategy={match_strategy}"
    ),
    needs = as.list(c("design", needs)),
    highly_sensitive = lst(
      cohort = outpath
    )
  )
  
}


actions_model <- function(effect, subgroup, outcome, match_strategy) {
  
  match_strategy_obj <- get(paste0("match_strategy_", match_strategy))
  n_match_rounds <- match_strategy_obj$n_match_rounds
  
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


action_table1 <- function(effect, match_strategy) {
  
  if (!is.null(match_strategy)) {
    match_strategy_obj <- get(paste0("match_strategy_", match_strategy))
    n_match_rounds <- match_strategy_obj$n_match_rounds
  }
  
  effect_match_strategy <- str_c(effect, match_strategy, sep = "_")
  
  needs_list <- "process_treated"
  
  if (effect == "comparative") needs_list <- c(needs_list, glue("match_{effect_match_strategy}"))
  
  if (effect == "incremental") {
    needs_list <- c(
      needs_list, 
      map_chr(1:n_match_rounds, ~glue("match_controlactual_{match_strategy}_{.x}"))
      )
  }
  
  action(
    name = glue("table1_{effect_match_strategy}"),
    run = "r:latest analysis/report/table1.R",
    arguments = c(effect, match_strategy),
    needs = as.list(needs_list),
    moderately_sensitive = lst(
      csv = glue("output/{effect_match_strategy}/table1/*.csv")
    )
  )
}

actions_postmatch <- function(effect, match_strategy) {
  
  match_strategy_obj <- get(paste0("match_strategy_", match_strategy))
  n_match_rounds <- match_strategy_obj$n_match_rounds
  
  effect_match_strategy <- str_c(effect, match_strategy, sep = "_")
  
  needs_list <- "process_treated"
  
  if (effect == "comparative") needs_list <- c(needs_list, glue("match_{effect_match_strategy}"))
  
  if (effect == "incremental") {
    needs_list <- c(
      needs_list, 
      map_chr(1:n_match_rounds, ~glue("match_controlactual_{match_strategy}_{.x}"))
      )
  }
  
  out <- action(
    name = glue("coverage_{effect_match_strategy}"),
    run = "r:latest analysis/match/match_coverage.R",
    arguments = c(effect, match_strategy),
    needs = as.list(needs_list),
    moderately_sensitive= lst(
      csv= glue("output/{effect_match_strategy}/coverage/*.csv"),
      png= glue("output/{effect_match_strategy}/coverage/*.png"),
    )
  )
  
  if (effect == "incremental") {
    
    needs_list <- c(needs_list, "process_initial")
    
  }
  
  out <- splice(
    out,
    action(
      name = glue("flowchart_{effect_match_strategy}"),
      run = "r:latest analysis/match/match_flowchart.R",
      arguments = c(effect, match_strategy),
      needs = as.list(needs_list),
      moderately_sensitive= lst(
        csv= glue("output/{effect_match_strategy}/flowchart/*.csv"),
      )
    )
  )
  
  return(out)
  
}


actions_match_strategy <- function(effect, match_strategy, include_models=FALSE) {
  
  match_strategy_obj <- get(paste0("match_strategy_", match_strategy))
  n_match_rounds <- match_strategy_obj$n_match_rounds
  
  effect_match_strategy <- str_c(effect, match_strategy, sep = "_")
  
  if (effect == "comparative") {
    
    actions_match <- splice(
      
      action(
        name = glue("match_{effect_match_strategy}"),
        run = "r:latest analysis/match/match_comparative.R",
        arguments = match_strategy,
        needs = c(
          # "process_initial",
          "process_treated",
          needs_model_riskscore(match_strategy)
        ) %>% as.list(),
        highly_sensitive = lst(
          rds = glue("output/comparative_{match_strategy}/match/*.rds")
        )
      ),
      
      # table1 for matched data for comparative effectiveness, match variables
      action_table1(
        effect = "comparative",
        match_strategy = match_strategy
      ),
      
      # match coverage
      actions_postmatch(
        effect = "comparative",
        match_strategy = match_strategy
      )
      
    )
    
    
  }
  if (effect == "incremental") {
    
    actions_match <- splice(
      
      map(
        seq_len(n_match_rounds), 
        ~action_1matchround(match_round = .x, match_strategy = match_strategy)
        ) %>% 
        flatten(),
      
      # table1 for matched data for incremental effectiveness, match variables
      action_table1(
        effect = "incremental",
        match_strategy = match_strategy
      ),
      
      # match coverage
      actions_postmatch(
        effect = "incremental",
        match_strategy = match_strategy
        ),
      
      comment("# # # # # # # # # # # # # # # # # # #", 
              "Extract adjustment variables for final matched controls",
              "# # # # # # # # # # # # # # # # # # #"),
      # extract adjustment variables for final matched controls
      action(
        name = glue("extract_controlfinal_{match_strategy}"),
        run = glue(
          "cohortextractor:latest generate_cohort",
          glue(" --study-definition study_definition_controlfinal"),
          glue(" --output-file output/incremental_{match_strategy}/match/input_controlfinal_{match_strategy}.feather"),
          " --param match_strategy={match_strategy}"
        ),
        needs = as.list(c(
          "design",
          map_chr(1:n_match_rounds, ~glue("match_controlactual_{match_strategy}_", .x))
        )),
        highly_sensitive = lst(
          cohort = glue("output/incremental_{match_strategy}/match/input_controlfinal_{match_strategy}.feather")
        )
      )#,
      
      # extract outcome variables for matched controls
      # extract_outcomes(match_strategy)
    )
    
  }
  
  actions <- splice(
    
    comment("# # # # # # # # # # # # # # # # # # #", 
            glue("Match treated for {effect} effectiveness"), 
            glue("match_strategy = {match_strategy}"),
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
  
  # extract outcome data for all treated people
  # (not incorporated into study_definition_treated so that it can be updated when more outcome data available)
  # extract_outcomes("alltreated"),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Extract and process controlpotential1 data.", 
          "This can be done once for all matching strategies.",
          "# # # # # # # # # # # # # # # # # # #"),
  action_controlpotential(match_strategy = "none", match_round = 1),
  
  ####################################################
  # all actions for comparative effectiveness analysis
  ####################################################
  actions_match_strategy(
    effect = "comparative",
    match_strategy = "a",
    include_models = FALSE
    ),
  
  actions_match_strategy(
    effect = "comparative",
    match_strategy = "riskscore_i",
    include_models = FALSE
  ),
  
  ####################################################
  # all actions for incremental effectiveness analysis
  ####################################################
  
  actions_match_strategy(
    effect = "incremental",
    match_strategy = "a",
    include_models = FALSE
    ),
  
  actions_match_strategy(
    effect = "incremental",
    match_strategy = "riskscore_i",
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

