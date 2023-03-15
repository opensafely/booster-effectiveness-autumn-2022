from cohortextractor import patients, combine_codelists
from codelists import *
import json
import codelists

############################################################
## functions
from variables_functions import *
############################################################

def generate_inclusion_variables(index_date):
  inclusion_variables = dict(
    
    registered = patients.registered_as_of(
      days(index_date, -1),
    ), 

    has_died = patients.died_from_any_cause(
      on_or_before=days(index_date, -1),
      returning="binary_flag",
    ),
          
  )
  return inclusion_variables

