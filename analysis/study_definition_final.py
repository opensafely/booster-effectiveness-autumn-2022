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

# define params
effect = params["effect"]
match_strategy = params["match_strategy"]

# match_strategy_obj
with open(f"lib/design/match-strategy-{match_strategy}.json") as f:
   match_strategy_obj = json.load(f)

# specify path for patient_id and trial_date
if effect == "comparative":
  file_path = "output/treated/eligible/data_treated.csv.gz"
  return_var = "vax_boostautumn_date"
if effect == "incremental":
  file_path = f"output/incremental_{match_strategy}/match/data_matchcontrol.csv.gz"
  return_var = "trial_date"

############################################################
# variables for adjustment
from variables_vars import generate_vars_variables 
adj_variables = generate_vars_variables(
    index_date="trial_date", 
    elig=False,
    vars=match_strategy_obj["adj_vars"]
    )
############################################################

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
  population = patients.which_exist_in_file(f_path=file_path),

  trial_date = patients.with_value_from_file(
    f_path = file_path, 
    returning = return_var, 
    returning_type = "date", 
    date_format = "YYYY-MM-DD"
    ),

  ###############################################################################
  # adjustment variables
  ##############################################################################
  **adj_variables,
  
  ###############################################################################
  # outcome variables
  ##############################################################################
  
  # vaccination for censoring
    vax_date = patients.with_tpp_vaccination_record(
      target_disease_matches="SARS-2 CORONAVIRUS",
      on_or_after="trial_date",
      find_first_match_in_period=True,
      returning="date",
      date_format="YYYY-MM-DD"
    ),
  
  # deregistration date for censoring
    dereg_date=patients.date_deregistered_from_all_supported_practices(
      on_or_after="trial_date",
      date_format="YYYY-MM-DD",
    ),
    
    # Positive covid admission 
    covidadmitted_date=patients.admitted_to_hospital(
      returning="date_admitted",
      with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
      with_these_diagnoses=codelists.covid_icd10,
      on_or_after="trial_date",
      date_format="YYYY-MM-DD",
      find_first_match_in_period=True,
    ),

    # Covid critical care admission
    covidcritcare_date=patients.admitted_to_hospital(
      returning="date_admitted",
      with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
      with_these_diagnoses=codelists.covid_icd10,
      with_at_least_one_day_in_critical_care=True,
      on_or_after="trial_date",
      date_format="YYYY-MM-DD",
      find_first_match_in_period=True,
    ),
    
    # Covid-related death
    coviddeath_date=patients.with_these_codes_on_death_certificate(
      codelists.covid_icd10,
      returning="date_of_death",
      date_format="YYYY-MM-DD",
    ),
    
    # All-cause death
    death_date=patients.died_from_any_cause(
      returning="date_of_death",
      date_format="YYYY-MM-DD",
    ),

    # deaths with a cardiovascular icd10 code
    cvddeath_date=patients.with_these_codes_on_death_certificate(
      codelists.cvd_combined,
      returning="date_of_death",
      date_format="YYYY-MM-DD",
    ),

    # death with a cancer icd10 code
    cancerdeath_date=patients.with_these_codes_on_death_certificate(
      codelists.cancer,
      returning="date_of_death",
      date_format="YYYY-MM-DD",
    ),

    # fracture outcomes (negative control)
    # a+e attendance due to fractures
    fractureemergency_date=patients.attended_emergency_care(
      returning="date_arrived",
      date_format="YYYY-MM-DD",
      on_or_after="trial_date",
      with_these_diagnoses = codelists.fractures_snomedECDS,
      find_first_match_in_period=True,
    ),
  
    # admission due to fractures
    fractureadmitted_date=patients.admitted_to_hospital(
      returning="date_admitted",
      on_or_after="trial_date",
      with_these_diagnoses=codelists.fractures_icd10,
      with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
      date_format="YYYY-MM-DD",
      find_first_match_in_period=True,
    ),
  
    # death due to fractures
    fracturedeath_date=patients.with_these_codes_on_death_certificate(
      codelists.fractures_icd10,
      returning="date_of_death",
      date_format="YYYY-MM-DD",
    ),

)
