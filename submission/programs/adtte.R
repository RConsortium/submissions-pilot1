###########################################################################
#' developers : Steven Haesendonckx/Bingjuam Wing/Ben Straub
#' date: 13NOV2022
#' modification History:
#' 
###########################################################################

# Set up ------------------------------------------------------------------

fcts <- c("eff_models.R", "fmt.R", "helpers.R", "Tplyr_helpers.R")
invisible(sapply(fcts, FUN = function(x) source(file.path("R/", x), )))

library(haven)
library(admiral)
library(dplyr)
library(xportr)
library(metacore)
library(metatools)
library(tidyr)
library(diffdf)

# read source -------------------------------------------------------------

adsl <- read_xpt(file.path("adam", "adsl.xpt"))
adae <- read_xpt(file.path("adam", "adae.xpt"))
ds <- read_xpt(file.path("sdtm", "ds.xpt"))

# First dermatological event (ADAE.AOCC01FL = 'Y' and ADAE.CQ01NAM != '') 

# TRTEMFL
#' If ASTDT >= TRTSDT > . then TRTEMFL='Y'. Otherwise TRTEMFL='N'

# CQ01NAM
#' If AEDECOD contains ('APPLICATION', 'DERMATITIS', 'ERYTHEMA', 'BLISTER') OR
#' if AEBODSYS='SKIN AND SUBC UTANEOUS TISSUE DISORDERS' 
#' but AEDECOD is not in ('COLD SWEAT', 'HYPERHIDROSIS', 'ALOPECIA') 
#' then CQ01NAM='DERMATOLOGIC EVENTS' Otherwise CQ01NAM=NULL

# AOCC01FL
#' Subset to CQ01NAM=''and TRTEMFL='Y' <- error in define, this should be CQ01NAM != ""
#' sort by Subject (USUBJID), Start Date (ASTDT), and Sequence Number (AESEQ)
#' flag the first record (set AOCC01FL='Y') within each Subject

## placeholder for origin=predecessor, use metatool::build_from_derived()
metacore <- spec_to_metacore("adam/TDF_ADaM - Pilot 3 Team updated.xlsx", where_sep_sheet = FALSE)
# Get the specifications for the dataset we are currently building
adtte_spec <- metacore %>%
  select_dataset("ADTTE")


event <- adae %>%
  filter(AOCC01FL == "Y", CQ01NAM == "DERMATOLOGIC EVENTS", SAFFL =="Y") %>%
  select(STUDYID, USUBJID, AESEQ, AOCC01FL, CQ01NAM, ASTDT)


# Censor events --------------------------------------------------------- 
## discontinuation, completed, death
ds00 <- ds %>%
  select(STUDYID, USUBJID, DSCAT, DSDECOD, DSSTDTC) %>%
  derive_vars_dt(
    .,
    dtc = DSSTDTC,
    new_vars_prefix = "DSST"
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds00,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE" & DSDECOD != "FINAL LAB VISIT"
  )

censor <- adsl %>%
  filter(SAFFL == "Y") %>%
  # Analysis uses DEATH date rather than discontinuation when subject dies even if discontinuation occurs before death
  # Observed through QC - However not described in specs
  mutate(EOS2DT = case_when(DCDECOD == "DEATH" ~ as.Date(RFENDTC),
                                   DCDECOD != "DEATH" ~ EOSDT)
                )%>%
  select(STUDYID, SITEID, USUBJID, AGE, AGEGR1, AGEGR1N, RACE, RACEN,
                SEX, TRTSDT, TRTEDT, TRTDURD, EOSDT, EOS2DT, EOSSTT, SAFFL,
                TRT01P, TRT01PN, TRT01A, TRT01AN) %>%
  rename(TRTDUR = TRTDURD, # total duration is same as in days
                TRTPN = TRT01PN,
                TRTP = TRT01P,
                TRTAN = TRT01AN,
                TRTA = TRT01A) 

# Combine event and censor ---------------------------------------------- 

adtte_pre <- event %>%
  right_join(censor, by = c("STUDYID", "USUBJID")) %>%
  mutate(PARAM = "Time to First Dermatologic Event",
                PARAMCD = "TTDE",
                STARTDT = TRTSDT,
                #pmin = vectorized solution required as min took the minimum across both vectors
                ADT = pmin(EOS2DT, ASTDT, na.rm = TRUE), 
                AVAL = as.numeric(ADT-STARTDT+1),
                CNSR = ifelse(!is.na(ASTDT) & ADT == ASTDT, 0, 1),
                # NA were causing issues hence the long date comparison expressions
                # Study discontinuation is also seen as study completion
                EVNTDESC = case_when((is.na(ASTDT) | (!is.na(ASTDT) & ASTDT != ADT) ) & EOSSTT == "DEATH" ~ "Death Date",
                                     (is.na(ASTDT) | (!is.na(ASTDT) & ASTDT != ADT) ) & EOSSTT %in% c("COMPLETED", "DISCONTINUED") ~ "Study Completion Date",
                                     (is.na(ASTDT) | (!is.na(ASTDT) & ASTDT != ADT) ) & EOSSTT == "DISCONTINUED" ~ "Study Discontinuation Date",
                                     !is.na(ASTDT) & (ADT == ASTDT) ~ "Dematologic Event Occured"),
                SRCDOM = case_when(EVNTDESC ==  "Dematologic Event Occured" ~ "ADAE",
                                   TRUE ~ "ADSL"),
                SRCVAR = case_when(EVNTDESC == "Death Date" ~ "DTHDT",
                                   EVNTDESC == "Study Completion Date" ~ "RFENDT",
                                   EVNTDESC ==  "Dematologic Event Occured" ~ "ASTDT"),
                SRCSEQ = case_when(!is.na(ASTDT) & (ADT == ASTDT) ~ as.numeric(AESEQ))
                ) 


adtte <- adtte_pre %>% 
  drop_unspec_vars(adtte_spec) %>% # only keep vars from define
  order_cols(adtte_spec) %>% # order columns based on define
  set_variable_labels(adtte_spec) %>% # apply variable labels based on define
  # xportr_type(adtte_spec, "ADTTE") %>%
  #xportr_length(adtte_spec, "ADTTE") %>%
  # unresolved issue in xportr_length due to:
  # https://github.com/tidyverse/haven/issues/699
  # no difference found by diffdf after commenting out xportr_length()
  xportr_format(adtte_spec$var_spec %>%
                  mutate_at(c("format"), ~ replace_na(., "")), "ADTTE") %>%
  xportr_write("submission/datasets/adadas.xpt",
               label = "AE Time To 1st Derm. Event Analysis"
  )

# END of Code -------------------------------------------------------------