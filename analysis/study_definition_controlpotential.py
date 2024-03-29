# Import codelists from codelists.py
import codelists

# import json module
import json

from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
  params
)

# define params
match_strategy = params["match_strategy"]
match_round = int(params["match_round"])
matchroundindex_date = params["index_date"]

if match_round==1:
    # all individuals satisfying initial eligibility criteria
    file_path = "output/initial/eligible/data_eligible.csv.gz"
    match_vars = "everything"
else:
    # all individuals satisfying initial eligibility criteria and not previously matched as controls
    file_path = f"output/incremental_{match_strategy}/matchround{match_round-1}/controlactual/match/data_unsuccessful_matchedcontrols.csv.gz"
    # match_strategy_obj
    with open(f"lib/design/match-strategy-{match_strategy}.json") as f:
      match_strategy_obj = json.load(f)
      match_vars = match_strategy_obj["match_vars"]



############################################################
# inclusion variables
from variables_inclusion import generate_inclusion_variables 
inclusion_variables = generate_inclusion_variables(index_date="matchroundindex_date")
############################################################
# match variables
from variables_vars import generate_vars_variables 
match_variables = generate_vars_variables(
    index_date="matchroundindex_date", 
    vars = match_vars,
    )
############################################################

# Specify study definition
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": "2020-01-01", "latest": "today"},
    "rate": "uniform",
    "incidence": 0.2,
    "int": {"distribution": "normal", "mean": 1000, "stddev": 100},
    "float": {"distribution": "normal", "mean": 25, "stddev": 5},
  },
  
  # This line defines the study population
  # patients that satisfy the original eligibility criteria
  population=patients.which_exist_in_file(f_path = file_path),

  matchroundindex_date = patients.fixed_value(matchroundindex_date),
    
  ###############################################################################
  # inclusion variables
  ##############################################################################
  **inclusion_variables,   
  
  ###############################################################################
  # match variables
  ##############################################################################
  **match_variables

)
