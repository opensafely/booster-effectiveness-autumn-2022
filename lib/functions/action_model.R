action_model <- function(effect, subgroup, outcome, match_strategy) {
  
  match_strategy_obj <- get(paste0("match_strategy_", match_strategy))
  n_match_rounds <- match_strategy_obj$n_match_rounds
  
  needs_list <- c("process_treated", "extract_final_treated")
  
  if (effect == "comparative") {
    needs_list <- c(
      needs_list, 
      glue("match_comparative_{match_strategy}")
    )
  }
  
  if (effect == "incremental") {
    needs_list <- c(
      needs_list, 
      map_chr(1:n_match_rounds, ~glue("match_controlactual_{match_strategy}_{.x}")),
      glue("extract_final_incremental_{match_strategy}")
    )
  }
  
  actions <- splice(
    
    comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # ",
            glue("subgroup={subgroup}; outcome={outcome};"),
            "# # # # # # # # # # # # # # # # # # # # # # # # # # # "),
    
    # km
    action(
      name = glue("km_{effect}_{match_strategy}_{subgroup}_{outcome}"),
      run = glue("r:latest analysis/model/km.R"),
      arguments = c(effect, match_strategy, subgroup, outcome),
      needs = needs_list,
      moderately_sensitive= lst(
        rds = glue("output/{effect}_{match_strategy}/model/km/{subgroup}/{outcome}/*.csv"),
        png = glue("output/{effect}_{match_strategy}/model/km/{subgroup}/{outcome}/*.png")
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
            name = glue("{model}_{effect}_{match_strategy}_{subgroup}_{outcome}"),
            run = "r:latest analysis/model/cox.R",
            arguments = c(effect, match_strategy, model, subgroup, outcome),
            needs = needs_list,
            moderately_sensitive= lst(
              csv = glue("output/{effect}_{match_strategy}/model/{model}/{subgroup}/{outcome}/*.csv")
            )
          )
          
        }
      ) %>%
      unlist(recursive = FALSE)
    
  )
  
  # }
  
  return(actions)
  
}