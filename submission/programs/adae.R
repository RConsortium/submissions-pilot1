# Name: ADAE
#
# Label: Adverse Event Analysis Dataset
#
# Input: ae, adsl
# Output: adae.xpt
#
# Developed by : [Author/Developer name]

library(admiral)
library(admiral.test) # Contains example datasets from the CDISC pilot project
library(curl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(arsenal)  
library(diffdf)
library(xportr)
library(metacore)
library(metatools)

rm(list = ls())  # Code removes all the Objects in the envirnoment 

# ----------------------------------------------------------------------------#
# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions  #
# as needed and assign to the variables below.                                #
# For illustration purposes read in admiral test data                         #
# ----------------------------------------------------------------------------#

# ---------- #
# read in AE #
# ---------- #
ae <- haven::read_xpt("https://github.com/RConsortium/submissions-pilot3-adam/blob/main/sdtm/ae.xpt?raw=true")
suppae <- haven::read_xpt("https://github.com/RConsortium/submissions-pilot3-adam/blob/main/sdtm/suppae.xpt?raw=true")

# ------------ #
# read in ADSL #
# ------------ #
adsl <- haven::read_xpt("https://github.com/RConsortium/submissions-pilot3-adam/blob/main/submission/datasets/adsl.xpt?raw=true")

#----------------------------------------------------------------------------------------#
# When SAS datasets are imported into R using haven::read_sas(), missing                 #
# character values from SAS appear as "" characters in R, instead of appearing           #
# as NA values. Further details can be obtained via the following link:                  #  
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values #
#----------------------------------------------------------------------------------------#
ae <- convert_blanks_to_na(ae)
adsl <- convert_blanks_to_na(adsl)

#---------------------- #
# ADSL derivation start #
#---------------------- #

#------------------------------------#
# Read in specifications from define #
#------------------------------------#
## placeholder for origin=predecessor, use metatool::build_from_derived()
metacore <- spec_to_metacore("adam/TDF_ADaM - Pilot 3 Team updated.xlsx", where_sep_sheet = FALSE, quiet=T)
# Get the specifications for the dataset we are currently building
adae_spec <- metacore %>% select_dataset("ADAE")

# ----------------------#
# Get list of ADSL vars #
# ----------------------#
adsl_vars <- vars(TRTSDT
                  ,TRTEDT
                  ,STUDYID 
                  ,SITEID
                  ,TRT01A
                  ,TRT01AN
                  ,AGE
                  ,AGEGR1
                  ,AGEGR1N
                  ,RACE
                  ,RACEN
                  ,SEX
                  ,SAFFL
                  ,TRTSDT
                  ,TRTEDT
               )

#------------------#
# Merge adsl to ae #
#------------------#
adae0 <- ae %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by = vars(STUDYID, USUBJID) 
  ) %>%
  
  #------------------------------#
  # Set TRTA and TRTAN from ADSL #
  #------------------------------#
  rename(TRTA = TRT01A,
         TRTAN = TRT01AN) %>% 
  # -----------------------------#
  # Derive analysis start time --#
  # -----------------------------#
  derive_vars_dtm(
    dtc = AESTDTC,
    new_vars_prefix = "AST",
    highest_imputation = "D" 
  )   %>% 

  # -----------------------------#
  # Derive analysis end time     #
  # -----------------------------#
  derive_vars_dtm(
    dtc = AEENDTC,
    new_vars_prefix = "AEN",
    highest_imputation = "M",
    date_imputation = "last",
    time_imputation = "last",
    # max_dates = vars(DTHDT, EOSDT)
    max_dates = NULL
  )%>%
  
  
  # -----------------------------------#
  # Derive analysis start & end dates  #
  # -----------------------------------#
  derive_vars_dtm_to_dt(vars(ASTDTM , AENDTM )) %>% 
  mutate (
    TRTSDTM = as_datetime(TRTSDT)  , 
    astdt_m = as_datetime(as.Date(AESTDTC))  , 
    aendt_m = as_datetime(as.Date(AEENDTC))  
  ) %>% 

  # ----------------------------#
  # Analysis Start Relative Day #  
  # Analysis End Relative Day   #
  # ----------------------------#
  derive_vars_dy(
    reference_date = TRTSDTM,
    source_vars = vars(TRTSDTM, ASTDTM , AENDTM)
  ) %>%
  
  derive_vars_duration( 
                       new_var = ADURN,
                       new_var_unit = ADURU,
                       start_date = astdt_m,
                       end_date = aendt_m,
                       out_unit = "days"
  ) %>% 
  mutate (ADURU = str_replace(ADURU , "DAYS" , "DAY")) %>% 
  # ---------------------------------#
  # Treatment Emergent Analysis flag #
  # ---------------------------------#
  
  ### NOTE : These 5 lines below are code from admiral{} that may need to be udpated.
  # derive_var_trtemfl(
  #   new_var = TRTEMFL, start_date = ASTDT , end_date = AENDT ,
  #   trt_start_date = TRTSDT , trt_end_date = NULL, end_window = NULL,
  #   ignore_time_for_trt_end = TRUE, initial_intensity = NULL,  intensity = NULL
  # )

  mutate(TRTEMFL = if_else(ASTDT >= TRTSDT , "Y" , NA_character_) ) %>%
  
  #---------------------------------------------------------------------#
  # AOCCFL - 1st Occurrence of Any AE Flag                              #
  #---------------------------------------------------------------------#
  # Programming Notes:                                                  #
  # Subset to TRTEMFL='Y' and sort by Subject (USUBJID),                #
  # Start Date (ASTDT), and Sequence Number (AESEQ) and                 #
  # flag the first record (set AOCCFL=’Y’) within each Subject          #
  # Not matching on 01-701-1118  as TRTEMFL = "Y"   as ASTDT is missing #
  #---------------------------------------------------------------------#  
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCCFL ,
      mode = "first"
    ), filter = TRTEMFL == "Y"
  )  %>% 

  #---------------------------------------------------------------------#
  # AOCCSFL - 1st Occurrence of SOC Flag                                #
  #---------------------------------------------------------------------#
  # Subset to TRTEMFL='Y' and sort by Subject (USUBJID),                #
  # System Organ Class (AEBODSYS), Start Date (ASTDT),                  #
  # and Sequence Number (AESEQ) and flag the first record               #
  # (set AOCCSFL=’Y’) within each Subject and SOC                       #
  # Not Matching on 01-701-1180,1118 , 1363 , 1076 , 1258 , 1299  1355  #
  # because their TRTEMFL = "Y" for SOCTERM when ASTDT is missing       #
  #---------------------------------------------------------------------# 
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS ),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCCSFL ,
      mode = "first"
    ), filter = TRTEMFL == "Y"
  )  %>% 

  #---------------------------------------------------------------------#
  # AOCCPFL - 1st Occurrence of Preferred Term Flag                     #
  #---------------------------------------------------------------------#
  # Subset to TRTEMFL='Y' and sort by Subject (USUBJID),                #
  # System Organ Class (AEBODSYS), Preferred Term                       # 
  # (AEDECOD), Start Date (ASTDT), and Sequence Number                  #
  # (AESEQ) and flag the first record (set AOCCPFL=’Y’) within          #
  # each Subject, SOC, and PT                                           #
  # Not Matching on the Subjects 01-701-1180,1118 , 1363 , 1076         #
  # ,1258 , 1299  1355                                                  #
  #---------------------------------------------------------------------#  
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS , AEDECOD ),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCCPFL ,
      mode = "first"
    ), filter = TRTEMFL == "Y"
  )  %>% 
  #---------------------------------------------------------------------#
  # AOCC02FL - 1st Occurrence 02 Flag for Serious                       #
  #---------------------------------------------------------------------#
  # Subset to TRTEMFL='Y' and AESER='Y' and sort by Subject             #
  # (USUBJID), Start Date (ASTDT), and Sequence Number                  #
  # (AESEQ) and flag the first record (set AOCC02FL=’Y’)                #
  # within each Subject                                                 #
  #---------------------------------------------------------------------#
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID  ),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCC02FL ,
      mode = "first"
    ), filter = TRTEMFL == "Y" & AESER =='Y'
  )  %>% 
  
  #---------------------------------------------------------------------#
  # AOCC03FL - 1st Occurrence 03 Flag for Serious SOC                   #
  #---------------------------------------------------------------------#
  # Subset to TRTEMFL='Y' and AESER='Y' and sort by Subject             #
  # (USUBJID), System Organ Class (AEBODSYS), Start Date                #
  # (ASTDT), and Sequence Number (AESEQ) and flag the                   #
  # first record (set AOCC03FL=’Y’) within each Subject and             #
  # SOC                                                                 #
  #---------------------------------------------------------------------#
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS   ),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCC03FL ,
      mode = "first"
    ), filter = TRTEMFL == "Y" & AESER =='Y'
  )  %>% 

  #---------------------------------------------------------------------#
  # AOCC04FL - 1st Occurrence 04 Flag for Serious PT                    #
  #---------------------------------------------------------------------#
  # Subset to TRTEMFL='Y' and AESER='Y' and sort by Subject             #
  # (USUBJID), System Organ Class (AEBODSYS), Preferred                 #
  # Term (AEDECOD), Start Date (ASTDT), and Sequence                    #
  # Number (AESEQ) and flag the first record (set                       #
  # AOCC04FL=’Y’) within each Subject, SOC, and PT                      #
  #---------------------------------------------------------------------#  
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS , AEDECOD  ),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCC04FL ,
      mode = "first"
    ), filter = TRTEMFL == "Y" & AESER =='Y'
  )  %>% 

  # --------------------------------------------------------------------#
  # CQ01NAM - Customized Query 01 Name                                  #
  # --------------------------------------------------------------------#
  # If AEDECOD contains any of the character strings of                 #
  # ('APPLICATION', 'DERMATITIS', 'ERYTHEMA', 'BLISTER')                #
  # OR if AEBODSYS='SKIN AND SUBC UTANEOUS TISSUE                       #
  # DISORDERS' but AEDECOD is not in ('COLD SWEAT',                     #
  #                                   'HYPERHIDROSIS', 'ALOPECIA') then #
  # CQ01NAM='DERMATOLOGIC EVENTS' Otherwise  CQ01NAM=NULL               #
  # --------------------------------------------------------------------#  
  mutate ( CQ01NAM  = ifelse(str_detect(.$AEDECOD , "APPLICATION" ) |
                             str_detect(.$AEDECOD , "DERMATITIS")   |
                             str_detect(.$AEDECOD , "ERYTHEMA")     |
                             str_detect(.$AEDECOD , "BLISTER")      | 
                             str_detect(.$AEBODSYS , "SKIN AND SUBCUTANEOUS TISSUE DISORDERS") &
                             !str_detect(.$AEDECOD , "COLD SWEAT|HYPERHIDROSIS|ALOPECIA")  , 
                             "DERMATOLOGIC EVENTS" , 
                             NA_character_) 
  ) %>%

  # --------------------------------------------------------------------#
  # AOCC01FL - 1st Occurrence 01 Flag for CQ01                          #
  # --------------------------------------------------------------------#
  # Subset to CQ01NAM='' and TRTEMFL='Y' and sort by                    #
  # Subject (USUBJID), Start Date (ASTDT), and Sequence                 #
  # Number (AESEQ) and flag the first record (set                       #
  # AOCC01FL=’Y’) within each Subject (Flag First Treatment             #
  # Emergent Dermatological Event for Time to Event Analysis)           #
  # --------------------------------------------------------------------#  
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID   ),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCC01FL ,
      mode = "first"
    ), filter = TRTEMFL == "Y" & CQ01NAM =='DERMATOLOGIC EVENTS'
  )  

# --------------------------------------------- #
# Check variables against define &              #
# Assign dataset labels, var labels and formats #
# --------------------------------------------- #
adae <- adae0 %>% 
  drop_unspec_vars(adae_spec) %>% # Check all variables specified are present and no more
  check_ct_data(adae_spec, na_acceptable = TRUE) %>% # Checks all variables with CT only contain values within the CT
  order_cols(adae_spec) %>% # Orders the columns according to the spec
  sort_by_key(adae_spec) %>% 
  xportr_df_label(adae_spec) %>% #dataset label 
  xportr_label(adae_spec) #variable labels

adae1 <- convert_blanks_to_na(adae) #blanks to NA

# --------------#
# Export to xpt #
# ------------- #
adae1 %>%
  xportr_write("submission/datasets/adae.xpt", 
               label = "Adverse Events Analysis Dataset")




# -------------------------------------------#
# QC/Check against original TDF ADAE dataset #
# -------------------------------------------#
adsl  <- haven::read_xpt("https://github.com/RConsortium/submissions-pilot3-adam/blob/main/adam/adsl.xpt?raw=true")
adae_orig <- haven::read_xpt("https://github.com/RConsortium/submissions-pilot3-adam/blob/main/adam/adae.xpt?raw=true")

adae_orig <- convert_blanks_to_na(adae_orig) #blanks to NA

#---------#
# Compare #
#---------#
diffdf(adae1  , adae_orig , keys = c("STUDYID","USUBJID","AESEQ")   ) 
