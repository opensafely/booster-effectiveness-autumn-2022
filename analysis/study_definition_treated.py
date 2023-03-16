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
inclusion_variables = generate_inclusion_variables(index_date="autumnbooster2022_date")
############################################################
## jcvi variables
from variables_jcvi import generate_jcvi_variables 
jcvi_variables = generate_jcvi_variables(index_date="autumnbooster2022_date")
############################################################
## demographic variables
from variables_demo import generate_demo_variables 
demo_variables = generate_demo_variables(index_date="autumnbooster2022_date")
############################################################
## covariates
from variables_covs import generate_covs_variables 
covs_variables = generate_covs_variables(index_date="autumnbooster2022_date")
############################################################
## pre variables
from variables_pre import generate_pre_variables 
pre_variables = generate_pre_variables(index_date="autumnbooster2022_date")
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
    """
    registered
    AND
    NOT has_died
    AND 
    eligible_initial
    """,
    
    **inclusion_variables,    

    # This line defines the study population
    eligible_initial = patients.which_exist_in_file(
    f_path=f"output/initial/eligible/data_eligible_treated.csv.gz"
    ),

  ),

  autumnbooster2022_date = patients.with_value_from_file(
    f_path=f"output/initial/eligible/data_eligible_treated.csv.gz", 
    returning="autumnbooster2022_date", 
    returning_type="date", 
    date_format='YYYY-MM-DD'
    ),
    
  ###############################################################################
  # jcvi variables
  ##############################################################################
  **jcvi_variables, 
  
  ###############################################################################
  # demographic variables
  ##############################################################################
  **demo_variables,   

  ###############################################################################
  # covariates
  ##############################################################################
  **covs_variables,   
  
  ###############################################################################
  # pre variables
  ##############################################################################
  **pre_variables,      
  
)
