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

  # region
  if any(x in vars for x in {"everything", "region"}):
    variables.update(
      # NHS administrative region
      region=patients.registered_practice_as_of(
          f"{index_date} - 1 day",
          returning="nuts1_region_name",
          return_expectations={
            "rate": "universal",
            "category": {
              "ratios": {
                "North East": 0.1,
                "North West": 0.1,
                "Yorkshire and The Humber": 0.2,
                "East Midlands": 0.1,
                "West Midlands": 0.1,
                "East": 0.1,
                "London": 0.1,
                "South East": 0.1,
                "South West": 0.1
                #"" : 0.01
                },
              },
          },
      ),
    )

  # flu vaccine in flu seasons 18-19, 19-20 or 20-21
  if any(x in vars for x in {"everything", "flu_vaccine"}):
    variables.update(
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

    return variables