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

############################################################
# inclusion variables
from variables_inclusion import generate_inclusion_variables 
inclusion_variables = generate_inclusion_variables(index_date="riskscore_start_date")
############################################################
## match variables
from variables_vars import generate_vars_variables 
vars_variables = generate_vars_variables(index_date="riskscore_start_date")
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
    f_path="output/riskscore/data_eligible.csv.gz"
    ),

  riskscore_start_date = patients.fixed_value(study_dates["riskscore"]["start"]),

  ###############################################################################
  # inclusion variables
  ##############################################################################
  **inclusion_variables,   

  ###############################################################################
  # variables for matching and model adjustment
  ##############################################################################
  **vars_variables,  

  # date of last discharged from unplanned hospital admission
  # don't need to worry about people who were discharged after riskscore_start_date, 
  # as they'll be excluded anyway
  unplanneddischarged_0_date=patients.admitted_to_hospital(
                returning = "date_discharged",
                on_or_before = "riskscore_start_date - 1 day", # this is the admission date
                # see https://github.com/opensafely-core/cohort-extractor/pull/497 for codes
                # see https://docs.opensafely.org/study-def-variables/#sus for more info
                with_admission_method = ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
                with_patient_classification = ["1"], # ordinary overnight admissions only
                date_format = "YYYY-MM-DD",
                find_last_match_in_period = True,
            ), 

  # outcome = death 
  death_date = patients.died_from_any_cause(
    returning = "date_of_death",
    date_format = "YYYY-MM-DD",
  ),

  # deregistration date
    dereg_date=patients.date_deregistered_from_all_supported_practices(
      between=[study_dates["riskscore"]["start"], study_dates["riskscore"]["end"]],
      date_format="YYYY-MM-DD",
    ),

)