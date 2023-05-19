from cohortextractor import patients, combine_codelists
from codelists import *
import json
import codelists


def generate_pre_variables(index_date):

  pre_variables = dict(

  ################################################################################################
  ## Pre-study event dates
  ################################################################################################

  # overnight hospital admission at time of index_date
  inhospital = patients.satisfying(
  
    f"discharged_0_date >= {index_date}",
    
    discharged_0_date=patients.admitted_to_hospital(
      returning="date_discharged",
      on_or_before=f"{index_date} - 1 day", # this is the admission date
      # see https://github.com/opensafely-core/cohort-extractor/pull/497 for codes
      # see https://docs.opensafely.org/study-def-variables/#sus for more info
      with_admission_method = ['11', '12', '13', '21', '2A', '22', '23', '24', '25', '2D', '28', '2B', '81'],
      with_patient_classification = ["1"], # ordinary admissions only
      date_format="YYYY-MM-DD",
      find_last_match_in_period=True,
    ), 
  ),
  
  # date of discharge from uplanned hospital admission with covid prior to study start date
  discharged_covid_0_date=patients.admitted_to_hospital(
    returning="date_discharged",
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_these_diagnoses=codelists.covid_icd10,
    on_or_before=f"{index_date} - 1 day",
    date_format="YYYY-MM-DD",
    find_last_match_in_period=True,
  ),
  
  )
  return pre_variables

