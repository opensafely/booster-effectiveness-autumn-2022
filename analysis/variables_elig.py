from cohortextractor import patients, combine_codelists
from codelists import *
import json
import codelists


def generate_elig_variables(index_date):
  
    elig_variables = dict(

        age=patients.age_as_of( 
            f"{index_date} - 1 day",
        ),

        has_follow_up_previous_1year=patients.registered_with_one_practice_between(
            start_date=f"{index_date} - 365 days",
            end_date=f"{index_date} - 1 day",
        ),

        # imd
        imd=patients.address_as_of(
            f"{index_date} - 1 day",
            returning="index_of_multiple_deprivation",
            round_to_nearest=100,
            return_expectations={
                "category": {"ratios": {c: 1/320 for c in range(100, 32100, 100)}},
                "incidence": 0.99
                }
            ),
    
        # care home flag
        carehome = patients.satisfying(

            "carehome_tpp OR carehome_code",

            carehome_tpp=patients.care_home_status_as_of(
                f"{index_date} - 1 day",
            ),

            carehome_code=patients.with_these_clinical_events(
                codelists.carehome,
                on_or_before=f"{index_date} - 1 day",
                returning="binary_flag",
                return_expectations={"incidence": 0.01},
            ),

        ),

        # end of life care flag
        endoflife = patients.satisfying(
        
            "midazolam OR endoflife_coding",
    
            midazolam = patients.with_these_medications(
                codelists.midazolam,
                returning="binary_flag",
                on_or_before=f"{index_date} - 1 day",
            ),

            endoflife_coding = patients.with_these_clinical_events(
                codelists.eol,
                returning="binary_flag",
                on_or_before=f"{index_date} - 1 day",
                find_last_match_in_period = True,
            ),

        ),

        # housebound flag
        housebound = patients.satisfying(
            """
            housebound_date
            AND NOT no_longer_housebound
            AND NOT moved_into_care_home
            """,

            housebound_date=patients.with_these_clinical_events( 
                codelists.housebound, 
                on_or_before=f"{index_date} - 1 day",
                find_last_match_in_period = True,
                returning="date",
                date_format="YYYY-MM-DD",
            ),   

            no_longer_housebound=patients.with_these_clinical_events( 
                codelists.no_longer_housebound, 
                on_or_after="housebound_date",
            ),

            moved_into_care_home=patients.with_these_clinical_events(
                codelists.carehome,
                on_or_after="housebound_date",
            ),

        ),

        # overnight hospital admission at time of index_date
        inhospital = patients.satisfying(
        
            f"discharged_0_date >= {index_date}",

            discharged_0_date=patients.admitted_to_hospital(
                returning="date_discharged",
                on_or_before=f"{index_date} - 1 day", # this is the admission date
                # see https://github.com/opensafely-core/cohort-extractor/pull/497 for codes
                # see https://docs.opensafely.org/study-def-variables/#sus for more info
                with_admission_method = ['11', '12', '13', '21', '2A', '22', '23', '24', '25', '2D', '28', '2B', '81'],
                with_patient_classification = ["1"], # ordinary admissions only
                date_format="YYYY-MM-DD",
                find_last_match_in_period=True,
            ), 
        ),
    
        # date of discharge from uplanned hospital admission with covid prior to study start date
        discharged_covid_0_date=patients.admitted_to_hospital(
            returning="date_discharged",
            with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
            with_these_diagnoses=codelists.covid_icd10,
            on_or_before=f"{index_date} - 1 day",
            date_format="YYYY-MM-DD",
            find_last_match_in_period=True,
        ),

      )

    return elig_variables

