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
        flowchart = glue("output/incremental_{match_strategy}/matchround{match_round}/controlpotential/flowchart/*.csv"),
        riskscore_i_plots = riskscore_i_plots
      ) %>% as.list()
    )
  )
  
}
