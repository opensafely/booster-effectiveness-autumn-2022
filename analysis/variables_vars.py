from cohortextractor import patients, combine_codelists
from codelists import *
import codelists


def generate_vars_variables(
    index_date,
    # extract variables for assessing eligibility by default,
    # but don't do this in controlfinal
    elig = True,
    # default to everything, so that when extracting 
    # treated and potential for matchround1
    # all potiential matching variables extracted
    vars = "everything", 
    ):
  
  # empty dict for variables
  variables = dict()

  if elig:
    # elig variables
    from variables_elig import generate_elig_variables 
    elig_variables = generate_elig_variables(index_date=index_date)
    # jcvi variables
    from variables_jcvi import generate_jcvi_variables 
    jcvi_variables = generate_jcvi_variables(index_date=index_date)
    # extract variables that are used in assessing prematch eligibility    
    variables.update(
      **elig_variables,
      **jcvi_variables,
    )

  # only extract the following variables if specified:

  # stp
  if any(x in vars for x in {"everything", "stp"}):
    variables.update(
      # stp is an NHS administration region based on geography
      stp=patients.registered_practice_as_of(
        f"{index_date} - 1 day",
        returning="stp_code",
        return_expectations={
          "rate": "universal",
          "category": {
            "ratios": {
              "STP1": 0.1,
              "STP2": 0.1,
              "STP3": 0.1,
              "STP4": 0.1,
              "STP5": 0.1,
              "STP6": 0.1,
              "STP7": 0.1,
              "STP8": 0.1,
              "STP9": 0.1,
              "STP10": 0.1,
            }
          },
        },
      ),
    )

  # flu vaccine in 2021-2022 season
  if any(x in vars for x in {"everything", "flu_vaccine_2122"}):
    variables.update(
      flu_vaccine_2122=patients.satisfying(
        """
        flu_vaccine_tpp_table_2122>0 OR
        flu_vaccine_med_2122>0 OR
        flu_vaccine_clinical_2122>0
        """,
        flu_vaccine_tpp_table_2122=patients.with_tpp_vaccination_record(
            target_disease_matches="INFLUENZA",
            between=["2021-07-01", "2022-03-31"], 
            returning="binary_flag",
        ),
        flu_vaccine_med_2122=patients.with_these_medications(
            codelists.flu_med_codes,
            between=["2021-07-01", "2022-03-31"], 
            returning="binary_flag",
        ),
        flu_vaccine_clinical_2122=patients.with_these_clinical_events(
            codelists.flu_clinical_given_codes,
            ignore_days_where_these_codes_occur=codelists.flu_clinical_not_given_codes,
            between=["2021-07-01", "2022-03-31"], 
            returning="binary_flag",
        ),
        return_expectations={"incidence": 0.5, },
      ),
    )

  # flu vaccine in any of 2018-19, 2019-20, 2020-2021 season
  if any(x in vars for x in {"everything", "flu_vaccine_1821"}):
    variables.update(
      flu_vaccine_1821=patients.satisfying(
        """
        flu_vaccine_tpp_table_2021>0 OR
        flu_vaccine_med_2021>0 OR
        flu_vaccine_clinical_2021>0 OR
        flu_vaccine_tpp_table_1920>0 OR
        flu_vaccine_med_1920>0 OR
        flu_vaccine_clinical_1920>0 OR
        flu_vaccine_tpp_table_1819>0 OR
        flu_vaccine_med_1819>0 OR
        flu_vaccine_clinical_1819>0
        """,
        flu_vaccine_tpp_table_2021=patients.with_tpp_vaccination_record(
            target_disease_matches="INFLUENZA",
            between=["2020-07-01", "2021-03-31"], 
            returning="binary_flag",
        ),
        flu_vaccine_med_2021=patients.with_these_medications(
            codelists.flu_med_codes,
            between=["2020-07-01", "2021-03-31"], 
            returning="binary_flag",
        ),
        flu_vaccine_clinical_2021=patients.with_these_clinical_events(
            codelists.flu_clinical_given_codes,
            ignore_days_where_these_codes_occur=codelists.flu_clinical_not_given_codes,
            between=["2020-07-01", "2021-03-31"], 
            returning="binary_flag",
        ),
        flu_vaccine_tpp_table_1920=patients.with_tpp_vaccination_record(
            target_disease_matches="INFLUENZA",
            between=["2019-07-01", "2020-03-31"], 
            returning="binary_flag",
        ),
        flu_vaccine_med_1920=patients.with_these_medications(
            codelists.flu_med_codes,
            between=["2019-07-01", "2020-03-31"], 
            returning="binary_flag",
        ),
        flu_vaccine_clinical_1920=patients.with_these_clinical_events(
            codelists.flu_clinical_given_codes,
            ignore_days_where_these_codes_occur=codelists.flu_clinical_not_given_codes,
            between=["2019-07-01", "2020-03-31"], 
            returning="binary_flag",
        ),
        flu_vaccine_tpp_table_1819=patients.with_tpp_vaccination_record(
            target_disease_matches="INFLUENZA",
            between=["2018-07-01", "2019-03-31"], 
            returning="binary_flag",
        ),
        flu_vaccine_med_1819=patients.with_these_medications(
            codelists.flu_med_codes,
            between=["2018-07-01", "2019-03-31"], 
            returning="binary_flag",
        ),
        flu_vaccine_clinical_1819=patients.with_these_clinical_events(
            codelists.flu_clinical_given_codes,
            ignore_days_where_these_codes_occur=codelists.flu_clinical_not_given_codes,
            between=["2018-07-01", "2019-03-31"], 
            returning="binary_flag",
        ),
        return_expectations={"incidence": 0.5, },
      ),
    )    

  # date of last discharged from unplanned hospital admission
  # don't need to worry about people who were discharged after riskscore_start_date, 
  # as they'll be excluded anyway
  if any(x in vars for x in {"everything", "timesince_discharged"}):
    variables.update(
        unplanneddischarged_0_date=patients.admitted_to_hospital(
                returning = "date_discharged",
                on_or_before = f"{index_date} - 1 day", # this is the admission date
                # see https://github.com/opensafely-core/cohort-extractor/pull/497 for codes
                # see https://docs.opensafely.org/study-def-variables/#sus for more info
                with_admission_method = ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
                with_patient_classification = ["1"], # ordinary overnight admissions only
                date_format = "YYYY-MM-DD",
                find_last_match_in_period = True,
                return_expectations={"date": {"earliest": "2000-01-01", "latest": "today"},},
         ), 

    )

  if any(x in vars for x in {"everything", "cancer"}):
    variables.update(
      cancer = patients.satisfying(
        "cancer_hospitalisation OR cancer_primarycare",
        cancer_hospitalisation = patients.admitted_to_hospital(
          returning = "binary_flag",
          with_these_diagnoses = codelists.cancer,
          between = [f"{index_date} - {3*365} days", f"{index_date} - 1 day"],
          ),
        cancer_primarycare = patients.with_these_clinical_events(
          combine_codelists(
            codelists.cancer_haem_snomed, 
            codelists.cancer_nonhaem_nonlung_snomed, 
            codelists.cancer_lung_snomed
            ),
            returning = "binary_flag",
            between = [f"{index_date} - {3*365} days", f"{index_date} - 1 day"],
          )
      )
    )
  
  return variables
