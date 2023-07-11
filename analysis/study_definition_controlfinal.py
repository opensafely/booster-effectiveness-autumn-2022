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

# match_strategy_ojb
with open("lib/design/match-strategy-A.json") as f:
   match_strategy_ojb = json.load(f)

# define params
match_strategy = params["match_strategy"]

############################################################
# variables for adjustment
from variables_vars import generate_vars_variables 
adj_variables = generate_vars_variables(
    index_date="trial_date", 
    elig=False,
    vars=match_strategy_ojb["adj_vars"]
    )
############################################################

# specify file path for all matched controls
file_path = f"output/incremental_{match_strategy}/match/data_matchcontrol.csv.gz"

# Specify study defeinition
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": studystart_date, "latest": "today"},
    "rate": "uniform",
    "incidence": 0.2,
    "int": {"distribution": "normal", "mean": 1000, "stddev": 100},
    "float": {"distribution": "normal", "mean": 25, "stddev": 5},
  },
  
  # This line defines the study population
  population = patients.which_exist_in_file(
    f_path=file_path
    ),

  trial_date = patients.with_value_from_file(
    f_path=file_path, 
    returning="trial_date", 
    returning_type="date", 
    date_format="YYYY-MM-DD"
    ),

  ###############################################################################
  # adjustment variables
  ##############################################################################
  **adj_variables,
  
)
