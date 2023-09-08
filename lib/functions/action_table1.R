action_table1 <- function(effect, match_strategy) {
  
  if (!is.null(match_strategy)) {
    match_strategy_obj <- get(paste0("match_strategy_", match_strategy))
    n_match_rounds <- match_strategy_obj$n_match_rounds
  }
  
  if (effect == "treated") {
    effect_match_strategy <- "treated"
  } else {
    effect_match_strategy <- str_c(effect, match_strategy, sep = "_")
  }
  
  
  needs_list <- c(
    "process_treated",
    "extract_final_treated"
    )
  
  if (effect == "comparative") needs_list <- c(needs_list, glue("match_{effect_match_strategy}"))
  
  if (effect == "incremental") {
    needs_list <- c(
      needs_list, 
      map_chr(1:n_match_rounds, ~glue("match_controlactual_{match_strategy}_{.x}")),
      glue("extract_final_incremental_{match_strategy}")
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
