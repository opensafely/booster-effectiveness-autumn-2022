from cohortextractor import patients
from codelists import *
import codelists

def generate_covs_variables(index_date):

  covs_variables = dict(

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
        between=[f"{index_date} - 5 years",f"{index_date} - 1 day"],
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

    # pregnancy
    pregnancy=patients.satisfying(
        """
        (preg_36wks_date) AND
        (pregdel_pre_date <= preg_36wks_date OR NOT pregdel_pre_date)
        """,
        # date of last pregnancy code in 36 weeks before ref_cev
        preg_36wks_date=patients.with_these_clinical_events(
            preg_primis,
            returning="date",
            find_last_match_in_period=True,
            between=[f"{index_date} - 252 days", f"{index_date} - 1 day"],
            date_format="YYYY-MM-DD",
        ),
        # date of last delivery code recorded in 36 weeks before elig_date
        pregdel_pre_date=patients.with_these_clinical_events(
            pregdel_primis,
            returning="date",
            find_last_match_in_period=True,
            between=[f"{index_date} - 252 days", f"{index_date} - 1 day"],
            date_format="YYYY-MM-DD",
        ),
    ),

    # during unvaccinated time (from when tests widely availabe to start of vaccinations)
    prior_test_frequency=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="any",
        between=["2020-05-18", "2020-12-05"], # day before 1st vaccine eligibility date
        returning="number_of_matches_in_period", 
        date_format="YYYY-MM-DD",
        restrict_to_earliest_specimen_date=False,
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
            flu_med_codes,
            between=["2018-07-01", "2021-06-30"], 
            returning="binary_flag",
        ),
        flu_vaccine_clinical=patients.with_these_clinical_events(
            flu_clinical_given_codes,
            ignore_days_where_these_codes_occur=flu_clinical_not_given_codes,
            between=["2018-07-01", "2021-06-30"], 
            returning="binary_flag",
        ),
        return_expectations={"incidence": 0.5, },
    ),
  
  )
  return covs_variables
