# Name: ADAE
#
# Label: Adverse Event Analysis Dataset
#
# Input: ae, adsl

library(admiral)
library(admiral.test) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)
library(stringr)
library(arsenal)  
library(diffdf)
library(xportr)

# Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data
rm(list = ls())  # Code removes all the Objects in the envirnoment 


# ---------- #
# read in AE #
# ---------- #
ae <- haven::read_xpt("https://github.com/RConsortium/submissions-pilot3-adam/blob/main/sdtm/ae.xpt?raw=true")
suppae <- haven::read_xpt("https://github.com/RConsortium/submissions-pilot3-adam/blob/main/sdtm/suppae.xpt?raw=true")

# ------------ #
# read in ADSL #
# ------------ #
adsl <- haven::read_xpt("https://github.com/RConsortium/submissions-pilot3-adam/blob/main/submission/datasets/adsl.xpt?raw=true")

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values

ae <- convert_blanks_to_na(ae)

# Derivations ----

# Get list of ADSL vars required for derivations
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

adae <- ae %>%
  # join adsl to ae
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
  
  ## Derive analysis start time ----
derive_vars_dtm(
  dtc = AESTDTC,
  new_vars_prefix = "AST",
  highest_imputation = "D" 
)   %>% 

  ## Derive analysis end time ----
derive_vars_dtm(
  dtc = AEENDTC,
  new_vars_prefix = "AEN",
  highest_imputation = "M",
  date_imputation = "last",
  time_imputation = "last",
  # max_dates = vars(DTHDT, EOSDT)
  max_dates = NULL
)%>%
  
  
  ## Derive analysis end/start date ----
derive_vars_dtm_to_dt(vars(ASTDTM , AENDTM )) %>% 
  

  mutate (
    TRTSDTM = as_datetime(TRTSDT)  , 
    astdt_m = as_datetime(as.Date(AESTDTC))  , 
    aendt_m = as_datetime(as.Date(AEENDTC))  
  ) %>% 
# Analysis Start Relative Day   
# Analysis End Relative Day   
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
 # Treatment Emergent Analysis flag  
  # derive_var_trtemfl(
  #   new_var = TRTEMFL, start_date = ASTDT , end_date = AENDT ,
  #   trt_start_date = TRTSDT , trt_end_date = NULL, end_window = NULL,
  #   ignore_time_for_trt_end = TRUE, initial_intensity = NULL,  intensity = NULL
  # )

  mutate(TRTEMFL = if_else(ASTDT >= TRTSDT , "Y" , NA_character_) ) %>%
# Subset to TRTEMFL='Y' and sort by Subject (USUBJID),
# Start Date (ASTDT), and Sequence Number (AESEQ) and
# flag the first record (set AOCCFL=’Y’) within each Subject
# Not matching on 01-701-1118  as TRTEMFL = "Y"   as ASTDT is missing 
  
   restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCCFL ,
      mode = "first"
    ), filter = TRTEMFL == "Y"
  )  %>% 
 
#   Subset to TRTEMFL='Y' and sort by Subject (USUBJID),
# System Organ Class (AEBODSYS), Start Date (ASTDT),
# and Sequence Number (AESEQ) and flag the first record
# (set AOCCSFL=’Y’) within each Subject and SOC  
# Not Matching on 01-701-1180,1118 , 1363 , 1076 , 1258 , 1299  1355
# because their TRTEMFL = "Y" for SOCTERM when ASTDT is missing
 
 
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS ),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCCSFL ,
      mode = "first"
    ), filter = TRTEMFL == "Y"
  )  %>% 

#   Subset to TRTEMFL='Y' and sort by Subject (USUBJID),
# System Organ Class (AEBODSYS), Preferred Term
# (AEDECOD), Start Date (ASTDT), and Sequence Number
# (AESEQ) and flag the first record (set AOCCPFL=’Y’) within
# each Subject, SOC, and PT
# Not Matching on the Subjects 01-701-1180,1118 , 1363 , 1076 , 1258 , 1299  1355
  
  
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS , AEDECOD ),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCCPFL ,
      mode = "first"
    ), filter = TRTEMFL == "Y"
  )  %>% 

# Subset to TRTEMFL='Y' and AESER='Y' and sort by Subject
# (USUBJID), Start Date (ASTDT), and Sequence Number
# (AESEQ) and flag the first record (set AOCC02FL=’Y’)
# within each Subject

  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID  ),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCC02FL ,
      mode = "first"
    ), filter = TRTEMFL == "Y" & AESER =='Y'
  )  %>% 
  
#   Subset to TRTEMFL='Y' and AESER='Y' and sort by Subject
# (USUBJID), System Organ Class (AEBODSYS), Start Date
# (ASTDT), and Sequence Number (AESEQ) and flag the
# first record (set AOCC03FL=’Y’) within each Subject and
# SOC

  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS   ),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCC03FL ,
      mode = "first"
    ), filter = TRTEMFL == "Y" & AESER =='Y'
  )  %>% 

  #   Subset to TRTEMFL='Y' and AESER='Y' and sort by Subject
# (USUBJID), System Organ Class (AEBODSYS), Preferred
# Term (AEDECOD), Start Date (ASTDT), and Sequence
# Number (AESEQ) and flag the first record (set
# AOCC04FL=’Y’) within each Subject, SOC, and PT 
  
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS , AEDECOD  ),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCC04FL ,
      mode = "first"
    ), filter = TRTEMFL == "Y" & AESER =='Y'
  )  %>% 

# If AEDECOD contains any of the character strings of
# ('APPLICATION', 'DERMATITIS', 'ERYTHEMA', 'BLISTER')
# OR if AEBODSYS='SKIN AND SUBC UTANEOUS TISSUE
# DISORDERS' but AEDECOD is not in ('COLD SWEAT',
#                                   'HYPERHIDROSIS', 'ALOPECIA') then
# CQ01NAM='DERMATOLOGIC EVENTS' Otherwise  CQ01NAM=NULL 
  
  mutate ( CQ01NAM  = ifelse(str_detect(.$AEDECOD , "APPLICATION" ) |
                               str_detect(.$AEDECOD , "DERMATITIS")   |
                               str_detect(.$AEDECOD , "ERYTHEMA")     |
                               str_detect(.$AEDECOD , "BLISTER")      | 
                               str_detect(.$AEBODSYS , "SKIN AND SUBCUTANEOUS TISSUE DISORDERS") &
                               !str_detect(.$AEDECOD , "COLD SWEAT|HYPERHIDROSIS|ALOPECIA")  , 
                             "DERMATOLOGIC EVENTS" , 
                             NA_character_) 
  ) %>%
#   
#   Subset to CQ01NAM='' and TRTEMFL='Y' and sort by
# Subject (USUBJID), Start Date (ASTDT), and Sequence
# Number (AESEQ) and flag the first record (set
# AOCC01FL=’Y’) within each Subject (Flag First Treatment
# Emergent Dermatological Event for Time to Event Analysis) 
  
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID   ),
      order = vars(  ASTDT, AESEQ),
      new_var = AOCC01FL ,
      mode = "first"
    ), filter = TRTEMFL == "Y" & CQ01NAM =='DERMATOLOGIC EVENTS'
  )  %>% 
  

  arrange(USUBJID,ASTDT  , AENDT ) %>% 
  select (USUBJID , AESTDTC,AEENDTC  , ASTDT , ASTDY , AENDY ,ASTDTF ,   AEBODSYS , AOCCSFL,AOCCPFL,
          astdt_m , aendt_m , AENDTM , AENDT , ADURN , ADURU  , TRTEMFL , TRTSDT, AESEQ , AOCCFL
          , AEDECOD , AESER , AOCC02FL , AOCC03FL , AOCC04FL , CQ01NAM , AOCC01FL , 
          AETERM , AELLT , AELLTCD , AEDECOD , AEPTCD , AEHLT , AEHLTCD , 
          AEHLGT , AEHLGT , AEHLGTCD , AEBODSYS , AESOC , AESOCCD , AESEV ,AESER , AESCAN , AESCONG , AESDISAB , 
          AESDTH , AESHOSP , AESLIFE , AESOD , AEREL , AEACN , AEOUT , AESEQ 
          , STUDYID                  
          ,SITEID
          ,TRTA
          ,TRTAN
          ,AGE
          ,AGEGR1
          ,AGEGR1N
          ,RACE
          ,RACEN
          ,SEX
          ,SAFFL
          ,TRTSDT
          ,TRTEDT
          ,ASTDTF
          ,ASTDY
          ,AENDT
          ,AENDY
          ,ADURN
          ,ADURU
          ,AOCCFL
          ,AOCCSFL)

adae2 <- adae %>% 
  arrange(USUBJID , AESEQ , ASTDT ,AEBODSYS , AEDECOD  ) %>% 
  select (USUBJID , AESEQ , ASTDT ,AEBODSYS, TRTEMFL,   AOCCSFL ,
          AEDECOD ,AOCCPFL, AESER , AOCC02FL  , AOCC03FL , AOCC04FL , CQ01NAM , AOCC01FL) 


adae1 <- adae %>% 
  arrange(USUBJID , AESEQ , ASTDT ,AEBODSYS ) %>% 
  select (USUBJID , AESEQ , ASTDT ,AEBODSYS, TRTEMFL,   AOCCPFL , AEDECOD , 
          AESER , AOCC02FL  , AOCC03FL  , AOCC04FL ,CQ01NAM , AOCC01FL , 
          AETERM , AELLT , AELLTCD , AEDECOD , AEPTCD , AEHLT , AEHLTCD , 
          AEHLGT , AEHLGT , AEHLGTCD , AEBODSYS , AESOC , AESOCCD , AESEV ,AESER , AESCAN , AESCONG , AESDISAB , 
          AESDTH , AESHOSP , AESLIFE , AESOD , AEREL , AEACN , AEOUT , AESEQ 
          , STUDYID
          ,SITEID
          ,TRTA
          ,TRTAN
          ,AGE
          ,AGEGR1
          ,AGEGR1N
          ,RACE
          ,RACEN
          ,SEX
          ,SAFFL
          ,TRTSDT
          ,TRTEDT
          ,ASTDTF
          ,ASTDY
          ,AENDT
          ,AENDY
          ,ADURN
          ,ADURU
          ,AOCCFL
          ,AOCCSFL)





 
metacore <- data.frame(
  dataset = "adae",
  variable = c("USUBJID", "TRTA", "TRTAN", 
               "TRTSDT", "TRTEDT" , 
               "ASTDT" , "ASTDTF" , "ASTDY",
               "AENDT",  "AENDY" ,
               "ADURN","ADURU" , 
               "AETERM", "AELLT", 
               "AELLTCD", "AEDECOD" , 
               "AEPTCD","AEHLT","AEHLTCD", "AEHLGT", "AEHLGTCD",
               "AEBODSYS","AESOC","AESOCCD","AESEV","AESER","AESCAN"
               ,"AESCONG","AESDISAB"), 
  label = c("Unique Subject Identifier",  "Actual Treatment",   "Actual Treatment (N)" , 
            "Date of First Exposure to Treatment",
            "Date of Last Exposure to Treatment",
            "Analysis Start Date",
            "Analysis Start Date Imputation Flag" , 
            "Analysis Start Relative Day",
            "Analysis End Date",  "Analysis End Relative Day",
            "AE Duration (N)",  "AE Duration Units" , 
            "Reported Term for the Adverse Event",
            "Lowest Level Term",
            "Lowest Level Term Code",
            "Dictionary-Derived Term" ,
            "Preferred Term Code",
            "High Level Term",
            "High Level Term Code" , "High Level Group Term",
            "High Level Group Term Code", 
            "Body System or Organ Class", "Primary System Organ Class", 
            "Primary System Organ Class Code" ,  "Severity/Intensity", "Serious Event" , "Involves Cancer" ,
            "Congenital Anomaly or Birth Defect" , 
            "Persist or Signif Disability/Incapacity") 
) 



adae2 <- xportr_label(adae, metacore)
labels(adae2)


adae1 <-  adae1 %>%   
          arrange(USUBJID , AESEQ , ASTDT ,AEBODSYS ) %>% 
          set_labels(c(STUDYID = "Study Identifier",
                       SITEID = "Study Site Identifier",
                       USUBJID = "Unique Subject Identifier",
                       TRTA = "Actual Treatment",
                       TRTAN = "Actual Treatment (N)",
                       AGE = "Age",
                       AGEGR1 = "Pooled Age Group 1",
                       AGEGR1N = "Pooled Age Group 1 (N)",
                       RACE = "Race",
                       RACEN = "Race (N)",
                       SEX = "Sex",
                       SAFFL = "Safety Population Flag",
                       TRTSDT = "Date of First Exposure to Treatment",
                       TRTEDT = "Date of Last Exposure to Treatment",
                       ASTDT = "Analysis Start Date",
                       ASTDTF = "Analysis Start Date Imputation Flag",
                       ASTDY = "Analysis Start Relative Day",
                       AENDT = "Analysis End Date",
                       AENDY = "Analysis End Relative Day",
                       ADURN = "AE Duration (N)",
                       ADURU = "AE Duration Units",
                       AETERM = "Reported Term for the Adverse Event",
                       AELLT = "Lowest Level Term",
                       AELLTCD = "Lowest Level Term Code",
                       AEDECOD = "Dictionary-Derived Term",
                       AEPTCD = "Preferred Term Code",
                       AEHLT = "High Level Term",
                       AEHLTCD = "High Level Term Code",
                       AEHLGT = "High Level Group Term",
                       AEHLGTCD = "High Level Group Term Code",
                       AEBODSYS = "Body System or Organ Class",
                       AESOC = "Primary System Organ Class",
                       AESOCCD = "Primary System Organ Class Code",
                       AESEV = "Severity/Intensity",
                       AESER = "Serious Event",
                       AESCAN = "Involves Cancer",
                       AESCONG = "Congenital Anomaly or Birth Defect",
                       AESDISAB = "Persist or Signif Disability/Incapacity",
                       AESDTH = "Results in Death",
                       AESHOSP = "Requires or Prolongs Hospitalization",
                       AESLIFE = "Is Life Threatening",
                       AESOD = "Occurred with Overdose",
                       AEREL = "Causality",
                       AEACN = "Action Taken with Study Treatment",
                       AEOUT = "Outcome of Adverse Event",
                       AESEQ = "Sequence Number",
                       TRTEMFL = "Treatment Emergent Analysis Flag",
                       AOCCFL = "1st Occurrence of Any AE Flag",
                       AOCCSFL = "1st Occurrence of SOC Flag",
                       AOCCPFL = "1st Occurrence of Preferred Term Flag",
                       AOCC02FL = "1st Occurrence 02 Flag for Serious",
                       AOCC03FL = "1st Occurrence 03 Flag for Serious SOC",
                       AOCC04FL = "1st Occurrence 04 Flag for Serious PT",
                       CQ01NAM = "Customized Query 01 Name",
                       AOCC01FL = "1st Occurrence 01 Flag for CQ01"
                       
                       ))

adae1 <- convert_blanks_to_na(adae1)
 
labels(adae1)

str(adae)



 
library(haven)
library(curl)

adsl  <- read_xpt("https://github.com/RConsortium/submissions-pilot3-adam/blob/main/adam/adsl.xpt?raw=true")
adae_or <- read_xpt("https://github.com/RConsortium/submissions-pilot3-adam/blob/main/adam/adae.xpt?raw=true")

str(adae_or, vec.len = 1)

label <- str(adae)

summary(adae_or)

adae_CHK <- adae_or %>% 
            arrange(USUBJID,ASTDT  , AENDT ) 

adae_CHK <- adae_CHK%>% 
  arrange(USUBJID , AESEQ , ASTDT ,AEBODSYS ) 

adae_CHK <- convert_blanks_to_na(adae_CHK)


summary(comparedf(adae1  , adae_CHK , by = "row.names"   ) ) 

