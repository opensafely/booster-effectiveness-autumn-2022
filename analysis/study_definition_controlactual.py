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

############################################################
## inclusion variables
from variables_inclusion import generate_inclusion_variables 
inclusion_variables = generate_inclusion_variables(index_date="trial_date")
############################################################
## jcvi variables
from variables_jcvi import generate_jcvi_variables 
jcvi_variables = generate_jcvi_variables(index_date="trial_date")
############################################################
## demographic variables
from variables_demo import generate_demo_variables 
demo_variables = generate_demo_variables(index_date="trial_date")
############################################################
## pre variables
from variables_pre import generate_pre_variables 
pre_variables = generate_pre_variables(index_date="trial_date")



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
  # jcvi variables
  ##############################################################################
  **jcvi_variables, 
  
  ###############################################################################
  # demographic variables
  ##############################################################################
  **demo_variables,   

  ###############################################################################
  # pre variables
  ##############################################################################
  **pre_variables,    

)
