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

    return variables