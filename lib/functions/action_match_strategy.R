action_match_strategy <- function(effect, match_strategy, include_models=TRUE) {
  
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
      action_postmatch(
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
      
      # match coverage
      action_postmatch(
        effect = "incremental",
        match_strategy = match_strategy
      ),
      
      comment("# # # # # # # # # # # # # # # # # # #", 
              "Extract adjustment and outcome variables for final matched controls",
              "# # # # # # # # # # # # # # # # # # #"),
      # extract adjustment variables for final matched controls
      action(
        name = glue("extract_final_incremental_{match_strategy}"),
        run = glue(
          "cohortextractor:latest generate_cohort",
          glue(" --study-definition study_definition_final"),
          glue(" --output-file output/incremental_{match_strategy}/match/input_final_{match_strategy}.feather"),
          " --param effect=incremental",
          " --param match_strategy={match_strategy}"
        ),
        needs = as.list(c(
          "design",
          map_chr(1:n_match_rounds, ~glue("match_controlactual_{match_strategy}_", .x))
        )),
        highly_sensitive = lst(
          cohort = glue("output/incremental_{match_strategy}/match/input_final_{match_strategy}.feather")
        )
      ),
      
      # table1 for matched data for incremental effectiveness, match variables
      action_table1(
        effect = "incremental",
        match_strategy = match_strategy
      )
      
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
            action_model(
              effect = effect,
              match_strategy = match_strategy,
              subgroup = subgroup,
              outcome = outcome
            )
          }
        ) %>%
        unlist(recursive = FALSE),
      
      action_combine_model_outputs(
        effect = "incremental",
        match_strategy = match_strategy
      )
      
    )
    
  }
  
  return(actions) 
  
}
