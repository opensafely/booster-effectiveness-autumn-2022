action_postmatch <- function(effect, match_strategy) {
  
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