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
group = params["group"]

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
    f_path=f"output/postmatch/eligible/data_{group}.csv.gz"
    ),

  trial_date = patients.with_value_from_file(
    f_path=f"output/postmatch/eligible/data_{group}.csv.gz", 
    returning="trial_date", 
    returning_type="date", 
    date_format="YYYY-MM-DD"
    ),

  ###############################################################################
  # covariates
  ##############################################################################
  # BMI category
    # https://github.com/opensafely/risk-factors-research/issues/51
    bmi=patients.categorised_as(

      {
        "Not obese": "DEFAULT",
        "Obese I (30-34.9)": """ bmi_value >= 30 AND bmi_value < 35""",
        "Obese II (35-39.9)": """ bmi_value >= 35 AND bmi_value < 40""",
        "Obese III (40+)": """ bmi_value >= 40 AND bmi_value < 100""",
        # set maximum to avoid any impossibly extreme values being classified as obese
      },
      
      bmi_value=patients.most_recent_bmi(
        between=["trial_date - 5 years", "trial_date - 1 day"],
        minimum_age_at_measurement=16
      ),
    
      return_expectations={
        "rate": "universal",
        "category": {
          "ratios": {
            "Not obese": 0.7,
            "Obese I (30-34.9)": 0.1,
            "Obese II (35-39.9)": 0.1,
            "Obese III (40+)": 0.1,
          }
        },
      },
    ),

    # flu vaccine in flu seasons 18-19, 19-20 or 20-21
    flu_vaccine=patients.satisfying(
        """
        flu_vaccine_tpp_table>0 OR
        flu_vaccine_med>0 OR
        flu_vaccine_clinical>0
        """,
        
        flu_vaccine_tpp_table=patients.with_tpp_vaccination_record(
            target_disease_matches="INFLUENZA",
            between=["2018-07-01", "2021-06-30"], 
            returning="binary_flag",
        ),
        
        flu_vaccine_med=patients.with_these_medications(
            codelists.flu_med_codes,
            between=["2018-07-01", "2021-06-30"], 
            returning="binary_flag",
        ),
        flu_vaccine_clinical=patients.with_these_clinical_events(
            codelists.flu_clinical_given_codes,
            ignore_days_where_these_codes_occur=codelists.flu_clinical_not_given_codes,
            between=["2018-07-01", "2021-06-30"], 
            returning="binary_flag",
        ),
        return_expectations={"incidence": 0.5, },
    ),
  
)