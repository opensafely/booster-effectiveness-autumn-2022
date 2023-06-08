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

############################################################
# inclusion variables
from variables_inclusion import generate_inclusion_variables 
inclusion_variables = generate_inclusion_variables(index_date="matchroundindex_date")
############################################################
## jcvi variables
from variables_jcvi import generate_jcvi_variables 
jcvi_variables = generate_jcvi_variables(index_date="matchroundindex_date")
############################################################
## demographic variables
from variables_demo import generate_demo_variables 
demo_variables = generate_demo_variables(index_date="matchroundindex_date")
############################################################
## pre variables
from variables_pre import generate_pre_variables 
pre_variables = generate_pre_variables(index_date="matchroundindex_date")
############################################################

if match_round==1:
    file_path = f"output/initial/eligible/data_eligible.csv.gz"
else:
    file_path = f"output/initial/eligible/data_eligible.csv.gz"

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
    "eligible_initial",   

    # patients that satisfy the original eligibility criteria
    eligible_initial = patients.which_exist_in_file(
    f_path = file_path
    ),

  ),

  matchroundindex_date = patients.fixed_value(matchroundindex_date),
    
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
