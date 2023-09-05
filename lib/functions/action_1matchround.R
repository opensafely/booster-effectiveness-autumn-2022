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
          c(
            glue("process_controlpotential_{match_strategy}_{match_round}"),
            map_chr(1:(match_round-1), ~glue("match_controlactual_{match_strategy}_{.x}"))
          )
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
        flowchart = glue("output/incremental_{match_strategy}/matchround{match_round}/controlactual/flowchart/*.csv"),
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
      moderately_sensitive = list(
        csv = glue("output/incremental_{match_strategy}/matchround{match_round}/controlactual/match/*.csv")
      ),
      highly_sensitive = as.list(c(
        rds = glue("output/incremental_{match_strategy}/matchround{match_round}/controlactual/match/*.rds"),
        csvgz = if (match_round < n_match_rounds) {glue("output/incremental_{match_strategy}/matchround{match_round}/controlactual/match/*.csv.gz")} else NULL,
        final = if(match_round==n_match_rounds) {glue("output/incremental_{match_strategy}/match/*.csv.gz")} else NULL
      ))
    )
    
  )
  
  match_actions <- match_actions[sapply(match_actions, function(x) !is.null(x))]
  
  return(match_actions)
  
}
