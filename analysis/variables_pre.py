from cohortextractor import patients, combine_codelists
from codelists import *
import json
import codelists


def generate_pre_variables(index_date):

  pre_variables = dict(

  ################################################################################################
  ## Pre-study event dates
  ################################################################################################

  # unplanned hospital admission
  admitted_unplanned_0_date=patients.admitted_to_hospital(
    returning="date_admitted",
    on_or_before=f"{index_date} - 1 day",
    # see https://github.com/opensafely-core/cohort-extractor/pull/497 for codes
    # see https://docs.opensafely.org/study-def-variables/#sus for more info
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_patient_classification = ["1"], # ordinary admissions only
    date_format="YYYY-MM-DD",
    find_last_match_in_period=True,
  ),
  discharged_unplanned_0_date=patients.admitted_to_hospital(
    returning="date_discharged",
    on_or_after="admitted_unplanned_0_date + 1 day",
    # see https://github.com/opensafely-core/cohort-extractor/pull/497 for codes
    # see https://docs.opensafely.org/study-def-variables/#sus for more info
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_patient_classification = ["1"], # ordinary admissions only
    date_format="YYYY-MM-DD",
    find_first_match_in_period=True,
  ), 

  # planned hospital admission
  admitted_planned_0_date=patients.admitted_to_hospital(
    returning="date_admitted",
    on_or_before=f"{index_date} - 1 day",
    # see https://github.com/opensafely-core/cohort-extractor/pull/497 for codes
    # see https://docs.opensafely.org/study-def-variables/#sus for more info
    with_admission_method=["11", "12", "13", "81"],
    with_patient_classification = ["1"], # ordinary admissions only 
    date_format="YYYY-MM-DD",
    find_last_match_in_period=True,
  ),
  discharged_planned_0_date=patients.admitted_to_hospital(
    returning="date_discharged",
    on_or_after="admitted_planned_0_date + 1 day",
    # see https://github.com/opensafely-core/cohort-extractor/pull/497 for codes
    # see https://docs.opensafely.org/study-def-variables/#sus for more info
    with_admission_method=["11", "12", "13", "81"],
    with_patient_classification = ["1"], # ordinary admissions only
    date_format="YYYY-MM-DD",
    find_first_match_in_period=True
  ), 
  
  # Positive covid admission prior to study start date
  admitted_covid_0_date=patients.admitted_to_hospital(
    returning="date_admitted",
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_these_diagnoses=codelists.covid_icd10,
    on_or_before=f"{index_date} - 1 day",
    date_format="YYYY-MM-DD",
    find_last_match_in_period=True,
  ),

  discharged_covid_0_date=patients.admitted_to_hospital(
    returning="date_discharged",
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_these_diagnoses=codelists.covid_icd10,
    on_or_after="admitted_covid_0_date + 1 day",
    date_format="YYYY-MM-DD",
    find_first_match_in_period=True,
  ),
  
  )
  return pre_variables

