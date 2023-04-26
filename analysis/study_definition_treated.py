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

############################################################
# inclusion variables
from variables_inclusion import generate_inclusion_variables 
inclusion_variables = generate_inclusion_variables(index_date="vax_boostautumn_date")
############################################################
## jcvi variables
from variables_jcvi import generate_jcvi_variables 
jcvi_variables = generate_jcvi_variables(index_date="vax_boostautumn_date")
############################################################
## demographic variables
from variables_demo import generate_demo_variables 
demo_variables = generate_demo_variables(index_date="vax_boostautumn_date")
############################################################
## pre variables
from variables_pre import generate_pre_variables 
pre_variables = generate_pre_variables(index_date="vax_boostautumn_date")
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
  population=patients.satisfying(
    "eligible_initial", 

    # patients that satisfy the original eligibility criteria and have autumnbooster2022_date during recruitment period
    eligible_initial = patients.which_exist_in_file(
    f_path=f"output/initial/eligible/data_eligible_treated.csv.gz"
    ),

  ),

  vax_boostautumn_date = patients.with_value_from_file(
    f_path=f"output/initial/eligible/data_eligible_treated.csv.gz", 
    returning="vax_boostautumn_date", 
    returning_type="date", 
    date_format='YYYY-MM-DD'
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
