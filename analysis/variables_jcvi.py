from cohortextractor import patients
from codelists import *
import codelists

def generate_jcvi_variables(index_date):

  jcvi_variables = dict(

  ########################################################################
  ## Clinical information for jcvi grouping as at index date (from PRIMIS)
  ########################################################################

    asthma = patients.satisfying(
      """
      astadm OR
      (ast AND astrxm1 AND astrxm2 AND astrxm3)
      """,
      # Asthma Admission codes
      astadm=patients.with_these_clinical_events(
        codelists.astadm,
        returning="binary_flag",
        on_or_before=f"{index_date} - 1 day",
      ),
      # Asthma Diagnosis code
      ast = patients.with_these_clinical_events(
        codelists.ast,
        returning="binary_flag",
        on_or_before=f"{index_date} - 1 day",
      ),
      # Asthma systemic steroid prescription code in month 1
      astrxm1=patients.with_these_medications(
        codelists.astrx,
        returning="binary_flag",
        between=[f"{index_date} - 30 days", index_date],
      ),
      # Asthma systemic steroid prescription code in month 2
      astrxm2=patients.with_these_medications(
        codelists.astrx,
        returning="binary_flag",
        between=[f"{index_date} - 60 days", f"{index_date} - 31 days"],
      ),
      # Asthma systemic steroid prescription code in month 3
      astrxm3=patients.with_these_medications(
        codelists.astrx,
        returning="binary_flag",
        between=[f"{index_date} - 90 days", f"{index_date} - 61 days"],
      ),
    ),

    # Chronic Neurological Disease including Significant Learning Disorder
    chronic_neuro_disease=patients.with_these_clinical_events(
      codelists.cns_cov,
      returning="binary_flag",
      on_or_before=f"{index_date} - 1 day",
    ),

    # Chronic Respiratory Disease
    chronic_resp_disease = patients.satisfying(
      "asthma OR resp_cov",
      resp_cov=patients.with_these_clinical_events(
        codelists.resp_cov,
        returning="binary_flag",
        on_or_before=f"{index_date} - 1 day",
      ),
    ),
    
    bmi_value = patients.most_recent_bmi(
        on_or_before=f"{index_date} - 1 day",
        minimum_age_at_measurement=18,
        include_measurement_date=False,
        date_format="YYYY-MM-DD",
      ),
      
    bmi=patients.categorised_as(

      {
        "Not obese": "DEFAULT",
        "Obese I (30-34.9)": """ bmi_value >= 30 AND bmi_value < 35""",
        "Obese II (35-39.9)": """ bmi_value >= 35 AND bmi_value < 40""",
        "Obese III (40+)": """ bmi_value >= 40 AND bmi_value < 100""",
        # set maximum to avoid any impossibly extreme values being classified as obese
      },
    
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

    sev_obesity = patients.satisfying(
      """
      sev_obesity_date > bmi_date OR
      bmi_value >= 40
      """,
      
      bmi_date = patients.date_of("bmi_value"),

      bmi_stage_date=patients.with_these_clinical_events(
        codelists.bmi_stage,
        returning="date",
        find_last_match_in_period=True,
        on_or_before=f"{index_date} - 1 day",
        date_format="YYYY-MM-DD",
      ),

      sev_obesity_date=patients.with_these_clinical_events(
        codelists.sev_obesity,
        returning="date",
        find_last_match_in_period=True,
        ignore_missing_values=True,
        between= ["bmi_stage_date", f"{index_date} - 1 day"],
        date_format="YYYY-MM-DD",
      ),
      
    ),

    diabetes = patients.satisfying(
      "(dmres_date < diab_date) OR (diab_date AND (NOT dmres_date))",
    
      diab_date=patients.with_these_clinical_events(
        codelists.diab,
        returning="date",
        find_last_match_in_period=True,
        on_or_before=f"{index_date} - 1 day",
        date_format="YYYY-MM-DD",
      ),

      dmres_date=patients.with_these_clinical_events(
        codelists.dmres,
        returning="date",
        find_last_match_in_period=True,
        on_or_before=f"{index_date} - 1 day",
        date_format="YYYY-MM-DD",
      ),
    ),

    sev_mental=patients.satisfying(
      "(smhres_date < sev_mental_date) OR (sev_mental_date AND (NOT smhres_date))",

      # Severe Mental Illness codes
      sev_mental_date=patients.with_these_clinical_events(
        codelists.sev_mental,
        returning="date",
        find_last_match_in_period=True,
        on_or_before=f"{index_date} - 1 day",
        date_format="YYYY-MM-DD",
      ),
    
      # Remission codes relating to Severe Mental Illness
      smhres_date=patients.with_these_clinical_events(
        codelists.smhres,
        returning="date",
        find_last_match_in_period=True,
        on_or_before=f"{index_date} - 1 day",
        date_format="YYYY-MM-DD",
      ),
    ),

  # Chronic heart disease codes
  chronic_heart_disease=patients.with_these_clinical_events(
    codelists.chd_cov,
    returning="binary_flag",
    on_or_before=f"{index_date} - 1 day",
  ),

  chronic_kidney_disease=patients.satisfying(
    """
    ckd OR
    (ckd15_date AND ckd35_date >= ckd15_date)
    """,

    # Chronic kidney disease codes - all stages
    ckd15_date=patients.with_these_clinical_events(
      codelists.ckd15,
      returning="date",
      find_last_match_in_period=True,
      on_or_before=f"{index_date} - 1 day",
      date_format="YYYY-MM-DD",
    ),

    # Chronic kidney disease codes-stages 3 - 5
    ckd35_date=patients.with_these_clinical_events(
      codelists.ckd35,
      returning="date",
      find_last_match_in_period=True,
      on_or_before=f"{index_date} - 1 day",
      date_format="YYYY-MM-DD",
    ),

    # Chronic kidney disease diagnostic codes
    ckd=patients.with_these_clinical_events(
      codelists.ckd_cov,
      returning="binary_flag",
      on_or_before=f"{index_date} - 1 day",
    ),
  ),

  # Chronic Liver disease codes
  chronic_liver_disease=patients.with_these_clinical_events(
    codelists.cld,
    returning="binary_flag",
    on_or_before=f"{index_date} - 1 day",
  ),

  # imunosuppression
  immunosuppressed=patients.satisfying(
    "immrx OR immdx",

    # Immunosuppression diagnosis codes
    immdx=patients.with_these_clinical_events(
      codelists.immdx_cov,
      returning="binary_flag",
      on_or_before=f"{index_date} - 1 day",
    ),
    # Immunosuppression medication codes
    immrx=patients.with_these_medications(
      codelists.immrx,
      returning="binary_flag",
      between=[f"{index_date} - 182 days", f"{index_date} - 1 day"],
    ),
  ),

  # Asplenia or Dysfunction of the Spleen codes
  asplenia=patients.with_these_clinical_events(
    codelists.spln_cov,
    returning="binary_flag",
    on_or_before=f"{index_date} - 1 day",
  ),

  # Wider Learning Disability
  learndis=patients.with_these_clinical_events(
    codelists.learndis,
    returning="binary_flag",
    on_or_before=f"{index_date} - 1 day",
  ),

  ########################################################################
  # additional information for JCVI grouping
  ########################################################################

  # health or social care worker  
  hscworker = patients.with_healthcare_worker_flag_on_covid_vaccine_record(returning="binary_flag"),
  
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
    """
    midazolam OR
    endoflife_coding
    """,
  
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
    """housebound_date
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

  )
  return jcvi_variables
