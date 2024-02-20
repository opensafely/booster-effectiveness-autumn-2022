# Import codelists from codelists.py
import codelists

# import json module
import json

# study_dates
with open("./lib/design/study-dates.json") as f:
  study_dates = json.load(f)

recruitmentend_date = study_dates["recruitmentend"]

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
## functions
from variables_functions import *
############################################################
## vax variables
from variables_vax import generate_vax_variables 
vax_variables = generate_vax_variables(index_date="1900-01-01")
############################################################
# inclusion variables
from variables_inclusion import generate_inclusion_variables 
inclusion_variables = generate_inclusion_variables(index_date=study_dates["studystart"])
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
  population=patients.all(),
  
  **inclusion_variables,  
  
  age=patients.age_as_of( 
    days(recruitmentend_date, -1),
    ),
  
  #################################################################
  ## Covid vaccine dates
  #################################################################
  **vax_variables,
  
  #################################################################
  ## Static variables
  # i.e. are not defined on a certain date
  # names of these variables are stored as `initial_vars` in design.R
  #################################################################

  sex=patients.sex(
    return_expectations={
      "rate": "universal",
      "category": {"ratios": {"M": 0.49, "F": 0.51}},
      "incidence": 1,
    }
  ),

  # Ethnicity (6 categories)
  ethnicity = patients.categorised_as(
    {
    "Unknown": "DEFAULT",
    "White": "eth6='1'",
    "Mixed": "eth6='2'",
    "Asian or Asian British": "eth6='3'",
    "Black or Black British": "eth6='4'",
    "Other": "eth6='5'",
    },
    eth6 = patients.with_these_clinical_events(
      ethnicity_codes_6,
      returning = "category",
      find_last_match_in_period = True,
      include_date_of_match = False,
      return_expectations = {
        "incidence": 0.75,
        "category": {
          "ratios": { "1": 0.30, "2": 0.20, "3": 0.20, "4": 0.20, "5": 0.05, "6": 0.05, },
          },
        },
      ),
    return_expectations = {
      "rate": "universal",
      "category": {
        "ratios": {
          "White": 0.30,
          "Mixed": 0.20,
          "Asian or Asian British": 0.20,
          "Black or Black British": 0.20,
          "Other": 0.05,
          "Unknown": 0.05,
          },
        },
      },
  ),

  # health or social care worker  
    hscworker = patients.with_healthcare_worker_flag_on_covid_vaccine_record(
        returning="binary_flag"
    ),
  
)
