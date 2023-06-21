from cohortextractor import patients, combine_codelists
from codelists import *
import codelists


def generate_match_variables(
    index_date, 
    # default to everything, so that when extracting 
    # treated and potential for matchround1
    # all potiential matching variables extracted
    match_vars = "everything", 
    ):
  
  ############################################################
  ## elig variables
  from variables_elig import generate_elig_variables 
  elig_variables = generate_elig_variables(index_date=index_date)
  ############################################################
  ## jcvi variables
  from variables_jcvi import generate_jcvi_variables 
  jcvi_variables = generate_jcvi_variables(index_date=index_date)
  ############################################################
  
  # always extract these variables
  variables = dict(
    # variables that are used in assessing prematch eligibility    
    **elig_variables,
    **jcvi_variables,
  )

  # only extract the following variables when needed in matching
  if any(x in match_vars for x in {'everything', 'region'}):
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

    if any(x in match_vars for x in {'everything', 'imd_Q5'}):

      variables.update(
        
        imd_Q5=patients.categorised_as(
        
            {
                "Unknown": "DEFAULT",
                "1 (most deprived)": "imd_temp >= 0 AND imd_temp < 32844*1/5",
                "2": "imd_temp >= 32844*1/5 AND imd_temp < 32844*2/5",
                "3": "imd_temp >= 32844*2/5 AND imd_temp < 32844*3/5",
                "4": "imd_temp >= 32844*3/5 AND imd_temp < 32844*4/5",
                "5 (least deprived)": "imd_temp >= 32844*4/5 AND imd_temp <= 32844",
            },
            return_expectations={
                "rate": "universal",
                "category": {"ratios": {"Unknown": 0.02, "1 (most deprived)": 0.18, "2": 0.2, "3": 0.2, "4": 0.2, "5 (least deprived)": 0.2}},
            },
    
            imd_temp=patients.address_as_of(
                f"{index_date} - 1 day",
                returning="index_of_multiple_deprivation",
                round_to_nearest=100,
                return_expectations={
                    "category": {"ratios": {c: 1/320 for c in range(100, 32100, 100)}}
                }
            ),

        ),
      )

    if any(x in match_vars for x in {'everything', 'imd'}):
      
      variables.update(

        imd=patients.address_as_of(
                f"{index_date} - 1 day",
                returning="index_of_multiple_deprivation",
                round_to_nearest=100,
                return_expectations={
                    "category": {"ratios": {c: 1/320 for c in range(100, 32100, 100)}}
                }
            ),

      )

    # if any(x in match_vars for x in {'everything', 'cv','multimorb'}):
    #     variables.update(
      
    #     )

    return variables