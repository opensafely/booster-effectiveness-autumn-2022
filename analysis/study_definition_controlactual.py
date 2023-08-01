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
match_round = params["match_round"]

# match_strategy_ojb
with open("lib/design/match-strategy-A.json") as f:
   match_strategy_ojb = json.load(f)

############################################################
## inclusion variables
from variables_inclusion import generate_inclusion_variables 
inclusion_variables = generate_inclusion_variables(index_date="trial_date")
############################################################
# match variables
from variables_vars import generate_vars_variables 
match_variables = generate_vars_variables(
    index_date="trial_date", 
    vars = match_strategy_ojb["keep_vars"]
    )
############################################################

# Specify study defeinition
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
  population=patients.satisfying(
    "prematched",
    
    prematched = patients.which_exist_in_file(
        f_path=f"output/incremental_{match_strategy}/matchround{match_round}/controlpotential/match/potential_matchedcontrols.csv.gz"
        ), 

  ),

  trial_date = patients.with_value_from_file(
      f_path=f"output/incremental_{match_strategy}/matchround{match_round}/controlpotential/match/potential_matchedcontrols.csv.gz", 
      returning="trial_date", 
      returning_type="date", 
      date_format='YYYY-MM-DD'
      ),
  
  match_id = patients.with_value_from_file(
      f_path=f"output/incremental_{match_strategy}/matchround{match_round}/controlpotential/match/potential_matchedcontrols.csv.gz", 
      returning="match_id", 
      returning_type="int"
      ),
  
  ###############################################################################
  # inclusion variables
  ##############################################################################
  **inclusion_variables,   

  ###############################################################################
  # match variables
  ##############################################################################
  **match_variables, 

)
