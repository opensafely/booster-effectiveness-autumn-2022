action_combine_model_outputs <- function(effect, match_strategy) {
  
  effect_select <- effect
  
  splice(
    comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # ",
            glue("Combine {effect} model outputs for match_strategy = {match_strategy}"),
            "# # # # # # # # # # # # # # # # # # # # # # # # # # # "),
    action(
      name = glue("combine_model_{effect}_{match_strategy}"),
      run = glue("r:latest analysis/model/combine_model.R"),
      arguments = c(effect, match_strategy),
      needs = splice(
        as.list(
          glue_data(
            .x=model_args %>% filter(effect == effect_select),
            "{model}_{effect}_{match_strategy}_{subgroup}_{outcome}"
          )
        )
      ),
      moderately_sensitive = lst(
        rds = glue("output/{effect}_{match_strategy}/model/*.csv"),
        png = glue("output/{effect}_{match_strategy}/model/*.png"),
      )
    )
  )  
  
}
