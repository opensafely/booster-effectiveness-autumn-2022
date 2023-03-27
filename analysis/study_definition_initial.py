# Import codelists from codelists.py
import codelists

# import json module
import json

# study_dates
with open("./lib/design/study-dates.json") as f:
  study_dates = json.load(f)

dose2end_date = study_dates["dose2"]["end"]

from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
  params
)

############################################################
## inclusion variables
from variables_vax import generate_vax_variables 
vax_variables = generate_vax_variables(index_date="1900-01-01")
############################################################
# vax variables
from variables_inclusion import generate_inclusion_variables 
inclusion_variables = generate_inclusion_variables(index_date=study_dates["studystart"])
############################################################

# Specify study definition
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": "2020-01-01", "latest": "today"},
    "rate": "uniform",
    "incidence": 0.2,
    "int": {"distribution": "normal", "mean": 1000, "stddev": 100},
    "float": {"distribution": "normal", "mean": 25, "stddev": 5},
  },
  
  # This line defines the study population
  population=patients.satisfying(
    """
    registered
    AND
    NOT has_died
    AND
    age >= 50
    AND 
    primarycourse_end
    """,
    
    **inclusion_variables,  

    primarycourse_end = patients.satisfying(
      f"covid_vax_disease_2_date < {dose2end_date}"
    ),

  ),
  
  age=patients.age_as_of( 
    study_dates["boosterautumn"]["ages50to64"],
    ),
  
  #################################################################
  ## Covid vaccine dates
  #################################################################
  **vax_variables,        
  
)
