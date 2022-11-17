###########################################################################
#' developpers : Steven Haesendonckx/
#' date: 13NOV2022
#' modification History:
#' 
###########################################################################

# Set up ------------------------------------------------------------------

fcts <- c("eff_models.R", "fmt.R", "helpers.R", "Tplyr_helpers.R")
sapply(fcts, FUN = function(x) source(file.path("R/", x)))

library(haven)
library(admiral)
library(dplyr)

# read source -------------------------------------------------------------

adsl <- haven::read_xpt(file.path("adam", "adsl.xpt"))
adae <- haven::read_xpt(file.path("adam", "adae.xpt"))
ds <- haven::read_xpt(file.path("sdtm", "ds.xpt"))
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


event <- adae %>%
  dplyr::filter(AOCC01FL == "Y", CQ01NAM == "DERMATOLOGIC EVENTS", SAFFL =="Y") %>%
  dplyr::select(STUDYID, USUBJID, AESEQ, AOCC01FL, CQ01NAM, ASTDT)


# Censor events --------------------------------------------------------- 

## discontinuation, completed, death
ds00 <- ds %>%
  dplyr::select(STUDYID, USUBJID, DSCAT, DSDECOD, DSSTDTC) %>%
  admiral::derive_vars_dt(
    .,
    dtc = DSSTDTC,
    new_vars_prefix = "DSST"
  )

adsl <- adsl %>%
  admiral::derive_vars_merged(
    dataset_add = ds00,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE" & DSDECOD != "FINAL LAB VISIT"
  )

censor <- adsl %>%
  dplyr::filter(SAFFL == "Y") %>%
  dplyr::select(STUDYID, SITEID, USUBJID, AGE, AGEGR1, AGEGR1N, RACE, RACEN,
                SEX, TRTSDT, TRTEDT, TRTDURD, EOSDT, EOSSTT, SAFFL,
                TRT01P, TRT01PN, TRT01A, TRT01AN) %>%
  dplyr::rename(TRTDUR = TRTDURD, # total duration is same as in days
                TRTPN = TRT01PN,
                TRTP = TRT01P,
                TRTAN = TRT01AN,
                TRTA = TRT01A) 

# Combine event and censor ---------------------------------------------- 

adtte_pre <- event %>%
  dplyr::right_join(censor, by = c("STUDYID", "USUBJID")) %>%
  dplyr::mutate(PARAM = "Time to First Dermatologic Event",
                PARAMCD = "TTDE",
                STARTDT = TRTSDT,
                #pmin = vectorized solution required as min took the minimum across both vectors
                ADT = pmin(EOSDT, ASTDT, na.rm = TRUE), 
                AVAL = ADT-STARTDT+1,
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
                ) %>%
  dplyr::select(STUDYID, SITEID, USUBJID, AGE, AGEGR1, AGEGR1N, RACE, RACEN, SEX, TRTSDT,
                TRTEDT, TRTDUR, TRTP, TRTA, TRTAN, PARAM, PARAMCD, AVAL, STARTDT, ADT,
                CNSR, EVNTDESC, SRCDOM, SRCVAR, SRCSEQ, SAFFL, EOSDT, ASTDT)

adtte <- adtte_pre %>%
  dplyr::select(-EOSDT, -ASTDT)

# Add Labels --------------------------------------------------------------

labs <- sapply(colnames(adtte), FUN = function(x) attr(adtte[[x]], "label"))
labs[unlist(lapply(labs,is.null))]

adtte[["AVAL"]] <- as.numeric(adtte[["AVAL"]])

attr(adtte[["TRTP"]], "label") <- "Planned Treatment"
attr(adtte[["TRTA"]], "label") <- "Actual Treatment"
attr(adtte[["TRTAN"]], "label") <- "Actual Treatment (N)"
attr(adtte[["PARAM"]], "label") <- "Parameter"
attr(adtte[["PARAMCD"]], "label") <- "Parameter Code"
attr(adtte[["STARTDT"]], "label") <- "Time to Event Origin Date for Subject"
attr(adtte[["ADT"]], "label") <- "Analysis Date"
attr(adtte[["AVAL"]], "label") <- "Analysis Value"
attr(adtte[["CNSR"]], "label") <- "Censor"
attr(adtte[["EVNTDESC"]], "label") <- "Event or Censoring Description"
attr(adtte[["SRCDOM"]], "label") <- "Source Domain"
attr(adtte[["SRCVAR"]], "label") <- "Source Variable"
attr(adtte[["SRCSEQ"]], "label") <- "Source Sequence Number"
attr(adtte[["TRTDUR"]], "label") <- "Duration of treatment (days)"

labsupdated <- sapply(colnames(adtte), FUN = function(x) attr(adtte[[x]], "label"))
labsupdated[unlist(lapply(labsupdated,is.null))]

# read original ADTTE -----------------------------------------------------

prod <- haven::read_xpt(file.path("adam", "adtte.xpt"))
labsprod <- sapply(colnames(prod), FUN = function(x) attr(prod[[x]], "label"))

# QC dev vs prod ----------------------------------------------------------

## Metadata compare (labels)

difflabels <- dplyr::setdiff(labsprod, labsupdated)
discr_labels <- unlist(labsprod)[which(unlist(labsprod) %in% difflabels)]

## Content check using in-house package

 # dfcompare( 
 #      file = "tlcompare"
 #     ,left = prod
 #     ,right = adtte
 #     ,keys = c("STUDYID", "USUBJID")
 #     ,showdiffs = 1000
 #     ,debug = FALSE
 # )

# Output ------------------------------------------------------------------


# END of Code -------------------------------------------------------------