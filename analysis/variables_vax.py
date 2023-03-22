
from cohortextractor import patients, combine_codelists
from codelists import *
import json
import codelists

############################################################
## functions
from variables_functions import *
############################################################

def generate_vax_variables(index_date):

  vax_variables = dict(

  # pfizer bivalent
  **vaccination_date_X(
    name = "covid_vax_pfizerbivalent",
    # use 1900 to capture all possible recorded covid vaccinations, including date errors
    # any vaccines occurring before national rollout are later excluded
    index_date = index_date, 
    n = 1,
    product_name_matches="Comirnaty Original/Omicron BA.1 COVID-19 Vacc md vials"
  ),
  # moderna bivalent
  **vaccination_date_X(
    name = "covid_vax_modernabivalent",
    # use 1900 to capture all possible recorded covid vaccinations, including date errors
    # any vaccines occurring before national rollout are later excluded
    index_date = index_date, 
    n = 1,
    product_name_matches="COVID-19 Vac Spikevax (Zero)/(Omicron) inj md vials"
  ),

  # pfizer
  **vaccination_date_X(
    name = "covid_vax_pfizer",
    # use 1900 to capture all possible recorded covid vaccinations, including date errors
    # any vaccines occurring before national rollout are later excluded
    index_date = index_date, 
    n = 4,
    product_name_matches="COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)"
  ),
  
  # az
  **vaccination_date_X(
    name = "covid_vax_az",
    index_date = index_date,
    n = 2,
    product_name_matches="COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)"
  ),
  
  # moderna
  **vaccination_date_X(
    name = "covid_vax_moderna",
    index_date = index_date,
    n = 4,
    product_name_matches="COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)"
  ),
  
  # any covid vaccine
  **vaccination_date_X(
    name = "covid_vax_disease",
    index_date = index_date,
    n = 5,
    target_disease_matches="SARS-2 CORONAVIRUS"
  ),
  
  )
  return vax_variables

