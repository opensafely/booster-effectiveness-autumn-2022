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

# study_dates
with open("./lib/design/study-dates.json") as f:
  study_dates = json.load(f)

studystart_date = study_dates["studystart"]

risk_score_date = "2022-07-01"

############################################################
# inclusion variables
from variables_inclusion import generate_inclusion_variables 
inclusion_variables = generate_inclusion_variables(index_date=risk_score_date)
############################################################
## match variables
from variables_vars import generate_vars_variables 
vars_variables = generate_vars_variables(index_date=risk_score_date)
############################################################

# Specify study defeinition
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": studystart_date, "latest": "today"},
    "rate": "uniform",
    "incidence": 0.1,
    "int": {"distribution": "normal", "mean": 1000, "stddev": 100},
    "float": {"distribution": "normal", "mean": 25, "stddev": 5},
  },
  
    # This line defines the study population
  population = patients.which_exist_in_file(
    f_path="ouput/risk_score/data_eligible.csv.gz"
    ),

  ###############################################################################
  # inclusion variables
  ##############################################################################
  **inclusion_variables,   

  ###############################################################################
  # variables for matching and model adjustment
  ##############################################################################
  **vars_variables,  

  # outcome = death 
  death_date=patients.died_from_any_cause(
    returning="date_of_death",
    date_format="YYYY-MM-DD",
  ),

  # dereg?? don't need if linear predictor

)