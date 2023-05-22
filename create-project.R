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
      highly_sensitive = lst(
        rds = glue("output/matchround{match_round}/controlactual/match/*.rds"),
      ),
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


extract_vars <- function(vars, group) {
  
  stopifnot("`vars` must be \"covs\" or \"outcomes\"" = vars %in% c("covs", "outcomes"))
  
  action(
    name = glue("extract_{vars}_{group}"),
    run = glue(
      "cohortextractor:latest generate_cohort",
      glue(" --study-definition study_definition_{vars}"),
      glue(" --output-file output/postmatch/{vars}/input_{vars}_{group}.feather"),
      " --param group={group}"
    ),
    needs = namelesslst(
      "design",
      "process_ids"
    ),
    highly_sensitive = lst(
      cohort = glue("output/postmatch/{vars}/input_{vars}_{group}.feather")
    )
  )
  
}


actions_model <- function(effect, subgroup, outcome) {
  
  needs_list <- list()
  needs_list[["km"]] <- c("process_treated", "extract_outcomes_alltreated")
  
  if (effect == "comparative") {
    needs_list[["km"]] <- c(needs_list[["km"]], "match_comparative")
    needs_list[["cox_adj"]] <- c(needs_list[["km"]], "extract_covs_alltreated")
  }
  
  
  
  if (effect == "relative") {
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
      arguments = c(effect, subgroup, outcome),
      needs = needs_list[["km"]],
      moderately_sensitive= lst(
        rds = glue("output/{effect}/model/km/{subgroup}/{outcome}/*.csv"),
        png = glue("output/{effect}/model/km/{subgroup}/{outcome}/*.png")
      )
    )
    
  )
  
  if (subgroup == "all") {
    
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
              arguments = c(effect, model, subgroup, outcome),
              needs = needs_list[[model]],
              moderately_sensitive= lst(
                csv = glue("output/{effect}/model/{model}/{subgroup}/{outcome}/*.csv")
              )
            )
            
          }
        ) %>%
        unlist(recursive = FALSE)
      
    )
      
  }
  
  return(actions)
    
}


# ## model action function ----
# action_combine <- function(
#     cohort
# ){
# 
#   action(
#     name = glue("combine"),
#     run = glue("r:latest analysis/model/combine.R"),
#     arguments = c(cohort),
#     needs = splice(
#       as.list(
#         glue_data(
#           .x=bind_rows(
#             km_args %>% filter(model=="km") %>% mutate(suffix=""),
#             km_args %>% filter(str_detect(model,"cox")) %>% mutate(suffix=""),
#             km_args %>% filter(str_detect(model,"cox")) %>% mutate(suffix="_overall")
#             ) %>%
#             filter(!(model == "cox_adj" & subgroup == "all" & outcome == "fracture" & suffix == "")),
#           "{model}_{subgroup}_{outcome}{suffix}"
#         )
#       )
#     ),
#     moderately_sensitive = lst(
#       rds = glue("output/models/combined/*.csv"),
#       png = glue("output/models/combined/*.png"),
#     )
#   )
# }
# 
action_table1 <- function(vars, effect){
  
  needs_list <- "process_treated"
  
  if (effect == "comparative") needs_list <- c(needs_list, "match_comparative")
  
  if (effect == "relative") {
    
    needs_list <- c(
      needs_list, 
      map_chr(1:n_match_rounds, ~glue("process_controlactual_{.x}"))
      )
    
  }
  
  if (vars %in% c("covs", "all")) {
    
    needs_list <- c(needs_list, "extract_covs_alltreated")
    
    if (effect == "relative") needs_list <- c(needs_list, "extract_covs_matchcontrol")  
    
  }
  
  action(
    name = glue("table1_{vars}_{effect}"),
    run = "r:latest analysis/report/table1.R",
    arguments = c(vars, effect),
    needs = as.list(needs_list),
    moderately_sensitive = lst(
      csv = glue("output/{effect}/table1/table1_{vars}_{effect}_rounded.csv"),
      html = glue("output/{effect}/table1/table1_{vars}_{effect}_rounded.html")
    )
  )
}

action_coverage <- function(effect){
  
  needs_list <- "process_treated"
  
  if (effect == "comparative") needs_list <- c(needs_list, "match_comparative")
  
  if (effect == "relative") {
    
    needs_list <- c(needs_list, glue("process_controlactual_{n_match_rounds}"))
    
  }
  
  out <- action(
    name = glue("coverage_{effect}"),
    run = "r:latest analysis/match/coverage.R",
    arguments = c(effect),
    needs = as.list(needs_list),
    moderately_sensitive= lst(
      csv= glue("output/{effect}/coverage/*.csv"),
      png= glue("output/{effect}/coverage/*.png"),
    )
  )
  
  if (effect == "relative") {
    
    needs_list <- c(needs_list, "process_initial")
    
  }
  
  out <- splice(
    out,
    action(
      name = glue("flowchart_{effect}"),
      run = "r:latest analysis/match/match_flowchart.R",
      arguments = c(effect),
      needs = as.list(needs_list),
      moderately_sensitive= lst(
        csv= glue("output/{effect}/flowchart/*.csv"),
      )
    )
  )
  
  return(out)
  
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
      flowchart = "output/treated/flowchart/*.csv",
      extract_treated_skim = "output/treated/extract/*.txt",
      data_processed_skim = "output/treated/process/*.txt",
      data_eligible_skim = "output/treated/eligible/*.txt"
    )
  ),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Match treated for comparative effectiveness", 
          "# # # # # # # # # # # # # # # # # # #"),
  
  action(
    name = "match_comparative",
    run = "r:latest analysis/match/match_comparative.R",
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
    vars = "match",
    effect = "comparative"
  ),
  
  # match coverage
  action_coverage("comparative"),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Extract, process and match control data", 
          "for relative effectiveness analysis",
          "# # # # # # # # # # # # # # # # # # #"),
  
  map(seq_len(n_match_rounds), ~action_1matchround(.x)) %>% flatten(),
  
  # table1 for matched data for relative effectiveness, match variables
  action_table1(
    vars = "match",
    effect = "relative"
  ),
  
  # match coverage
  action_coverage("relative"),
  
  comment("# # # # # # # # # # # # # # # # # # #", 
          "Extract and process covs and outcomes", 
          "for successfully matched data",
          "(relative and comparative effectiveness)",
          "# # # # # # # # # # # # # # # # # # #"),
  
  comment("",
          "process_ids creates datasets of all matched treated and control ids",
          "for whom to extract the covariates and outcomes"),
  action(
    name = "process_ids",
    run = "r:latest analysis/process/process_ids.R",
    needs = namelesslst(
      "process_treated",
      glue("process_controlactual_{n_match_rounds}")
    ),
    highly_sensitive = lst(
      ids = "output/postmatch/eligible/*.csv.gz"
    )
  ),
  
  extract_vars(vars = "covs", group = "alltreated"),
  extract_vars(vars = "covs", group = "matchcontrol"),
  extract_vars(vars = "outcomes", group = "alltreated"),
  extract_vars(vars = "outcomes", group = "matchcontrol"),
  
  # table1 for all treated (including unmatched), all variables
  action_table1(
    vars = "all",
    effect = "treated"
  ),
  
  # table1 for matched data for comparative effectiveness, covariates
  action_table1(
    vars = "covs",
    effect = "comparative"
  ),
  
  # table1 for matched data for relative effectiveness, covariates
  action_table1(
    vars = "covs",
    effect = "relative"
  ),
  
  comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # ",
          "Combine outputs for report",
          "# # # # # # # # # # # # # # # # # # # # # # # # # # # "),
  
  action(
    name = "combine_flowchart",
    run = "r:latest analysis/report/combine_flowchart.R",
    needs = namelesslst(
      "process_initial",
      "process_treated",
      "flowchart_comparative",
      "flowchart_relative"
    ),
    moderately_sensitive = lst(
      flowchart = "output/report/flowchart/*.csv"
    )
  ),
  
  action(
    name = "combine_table1",
    run = "r:latest analysis/report/combine_table1.R",
    needs = namelesslst(
      "table1_match_comparative",
      "table1_match_relative",
      "table1_all_treated",
      "table1_covs_comparative",
      "table1_covs_relative"
    ),
    moderately_sensitive = lst(
      flowchart = "output/report/table1/*.csv"
    )
  ),
  
  comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # ",
          "Fit models to estimate comparative effectiveness",
          "# # # # # # # # # # # # # # # # # # # # # # # # # # # "),
  
  expand_grid(
    subgroup=subgroups,
    outcome=outcomes,
  ) %>%
    pmap(
      function(subgroup, outcome) {
        actions_model(
          effect = "comparative",
          subgroup = subgroup,
          outcome = outcome
        )
      }
    ) %>%
    unlist(recursive = FALSE),
  
  comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # ",
          "Fit models to estimate relative effectiveness",
          "# # # # # # # # # # # # # # # # # # # # # # # # # # # "),
  
  expand_grid(
    subgroup=subgroups,
    outcome=outcomes,
  ) %>%
    pmap(
      function(subgroup, outcome) {
        actions_model(
          effect = "relative",
          subgroup = subgroup,
          outcome = outcome
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
    needs = splice(
      as.list(
        glue_data(
          .x=model_args,
          "{model}_{effect}_{subgroup}_{outcome}"
        )
      )
    ),
    moderately_sensitive = lst(
      rds = glue("output/report/model/*.csv"),
      png = glue("output/report/model/*.png"),
    )
  ),
  
  #####
  
  # action(
  #   name = glue("extract_controlfinal"),
  #   run = glue(
  #     "cohortextractor:latest generate_cohort",
  #     " --study-definition study_definition_controlfinal",
  #     " --output-file output/extract/input_controlfinal.feather",
  #     " --param n_match_rounds={n_match_rounds}",
  #   ),
  #   needs = namelesslst(
  #     "design",
  #     glue("process_controlactual_{n_match_rounds}")
  #   ),
  #   highly_sensitive = lst(
  #     extract = glue("output/extract/input_controlfinal.feather")
  #   ),
  # ),
  # 
  # action(
  #   name = glue("dummydata_final"),
  #   run = glue("r:latest analysis/dummy/dummydata_controlfinal.R"),
  #   needs =map(
  #     seq_len(n_match_rounds),
  #     ~glue("process_controlactual_",.x)
  #   ),
  #   highly_sensitive = lst(
  #     dummydata_controlfinal = glue("output/dummydata/dummy_control_final.feather")
  #   ),
  # ),
  # 
  # action(
  #   name = glue("process_final"),
  #   run = glue("r:latest analysis/process/process_data.R"),
  #   arguments = "final",
  #   needs = c(
  #     map(
  #       seq_len(n_match_rounds),
  #       ~glue("process_controlactual_",.x)
  #     ),
  #     glue("extract_controlfinal"),
  #     glue("process_treated"),
  #     glue("dummydata_controlfinal")
  #   ),
  #   highly_sensitive = lst(
  #     extract = glue("output/match/*.rds"),
  #     ids = glue("output/match/*.csv.gz")
  #   ),
  #   moderately_sensitive = lst(
  #     input_controlfinal_skim = glue("output/extract/*.txt"),
  #     data_matched_skim = glue("output/match/*.txt")
  #   )
  # ),
  
  # 
  # lapply_actions(
  #   "bivalent",
  #   function(x) {
  #     c(
  #       comment("# # # # # # # # # # # # # # # # # # #",
  #               glue("{x} cohort"),
  #               "# # # # # # # # # # # # # # # # # # #"),
  #       
  #       comment("# # # # # # # # # # # # # # # # # # #",
  #               "Extract and match"),
  #       
  #       action_extract_and_match(x),
  #       
  #       action_table1(x),
  #       
  #       action_coverage(x),
  #       
  #       action(
  #         name = "flowchart",
  #         run = glue("r:latest analysis/match/flowchart.R"),
  #         needs = namelesslst(
  #           "process_treated",
  #           "process_controlfinal_mrna"
  #           ),
  #         moderately_sensitive = lst(
  #           flowchart_final = "output/mrna/flowchart/flowchart_final_rounded.csv"
  #         )
  #       ),
  #       
  #       comment("# # # # # # # # # # # # # # # # # # #",
  #               "Causes of non-COVID death data",
  #               "# # # # # # # # # # # # # # # # # # #"),
  #       
  #       action(
  #         name = glue("extract_noncoviddeathcause_{x}"),
  #         run = glue(
  #           "cohortextractor:latest generate_cohort", 
  #           " --study-definition study_definition_noncoviddeathcause", 
  #           " --output-file output/{x}/noncoviddeathcause/extract/input_noncoviddeathcause.feather",
  #           " --param cohort={x}"
  #         ),
  #         needs = namelesslst(
  #           "design",
  #           glue("process_controlfinal_{x}")
  #         ),
  #         highly_sensitive = lst(
  #           extract = glue("output/{x}/noncoviddeathcause/extract/input_noncoviddeathcause.feather")
  #         )
  #       ),
  #       
  #       comment("# # # # # # # # # # # # # # # # # # #",
  #               "Covid tests data",
  #               "# # # # # # # # # # # # # # # # # # #"),
  #       
  #       lapply_actions(
  #         c("treated", "control"),
  #         function(y)
  #           # covidtests data for all matched people
  #           # need to do separately for treated and controls, one patient could have two different trial dates,
  #           # and study_definition can only handle one row per patient
  #           action(
  #             name = glue("extract_covidtests_{x}_{y}"),
  #             run = glue(
  #               "cohortextractor:latest generate_cohort", 
  #               " --study-definition study_definition_covidtests", 
  #               " --output-file output/{x}/covidtests/extract/input_covidtests_{y}.feather",
  #               " --param cohort={x}",
  #               " --param arm={y}"
  #             ),
  #             needs = namelesslst(
  #               "design",
  #               glue("process_controlfinal_{x}")
  #             ),
  #             highly_sensitive = lst(
  #               extract = glue("output/{x}/covidtests/extract/input_covidtests_{y}.feather")
  #             )
  #           )
  #       ),
  #       
  #       action(
  #         name = glue("process_covidtests_{x}"),
  #         run = "r:latest analysis/covidtests/process_covidtests.R",
  #         arguments = x,
  #         needs = namelesslst(
  #           "process_controlfinal_mrna",
  #           glue("extract_covidtests_{x}_treated"),
  #           glue("extract_covidtests_{x}_control")
  #         ),
  #         highly_sensitive = lst(
  #           extract = "output/mrna/covidtests/process/*.rds",
  #         ),
  #         moderately_sensitive = lst(
  #           skim = "output/mrna/covidtests/extract/*.txt",
  #           png = "output/mrna/covidtests/checks/*.png"
  #         )
  #       ),
  #       
  #       action(
  #         name = glue("summarise_covidtests_{x}"),
  #         run = "r:latest analysis/covidtests/summarise_covidtests.R",
  #         arguments = c("mrna", "all"), # may want to look in subgroups later, but for now just "all"
  #         needs = namelesslst(
  #           glue("process_covidtests_{x}")
  #         ),
  #         moderately_sensitive = lst(
  #           csv = "output/mrna/covidtests/summary/all/*.csv",
  #           png = "output/mrna/covidtests/summary/all/*.png"
  #         )
  #       ),
  #       
  #       comment("# # # # # # # # # # # # # # # # # # #",
  #               "Model",
  #               "# # # # # # # # # # # # # # # # # # #"),
  #       
  #       action_cinc_dose4(x),
  #       
  #       lapply_actions(
  #         subgroups,
  #         function(y) {
  #           lapply_actions(
  #             km_args %>% 
  #               filter(subgroup==y) %>%
  #               unlist() %>% unname(),
  #             function(v) {
  #                 lapply_actions(
  #                   outcomes,
  #                   function(z) actions_model(cohort=x, subgroup=y, outcome=z)
  #               )
  #             }
  #           )
  #         }
  #       ),
  #       
  #       comment("# # # # # # # # # # # # # # # # # # #",
  #               glue("combine all outputs for {x} cohort"),
  #               "# # # # # # # # # # # # # # # # # # #"),
  #       
  #       action_combine(x)
  #       
  #     )
  #   }
  # ),
  # 
  # comment("# # # # # # # # # # # # # # # # # # #", 
  #         "Move files for release", 
  #         "# # # # # # # # # # # # # # # # # # #"),
  # 
  # action(
  #   name = "release",
  #   run = glue("r:latest analysis/release_objects.R"),
  #   needs = namelesslst(
  #     glue("combine_km_pfizer"),
  #     glue("table1_pfizer"),
  #     glue("combine_km_under12"),
  #     glue("table1_under12"),
  #   ),
  #   highly_sensitive = lst(
  #     txt = glue("output/meta-release/*.txt"),
  #     csv = glue("output/release/*.csv"),
  #   ),
  # ),

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

