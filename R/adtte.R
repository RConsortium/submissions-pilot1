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

adtte <- event %>%
  dplyr::right_join(censor, by = c("STUDYID", "USUBJID")) %>%
  dplyr::mutate(PARAM = "Time to First Dermatologic Event",
                PARAMCD = "TTDE",
                STARTDT = TRTSDT,
                ADT = pmin(EOSDT, ASTDT), #pmin = vectorized solution
                AVAL = ADT-STARTDT+1,
                CNSR = ifelse(ADT == ASTDT, 0, 1),
                EVNTDESC = case_when(ADT != ASTDT & EOSSTT == "DEATH" ~ "Death Date",
                                     ADT != ASTDT & EOSSTT == "COMPLETED" ~ "Study Completion Date",
                                     ADT != ASTDT & EOSSTT == "DISCONTINUED" ~ "Study Discontinuation Date",
                                     ADT == ASTDT ~ "Dematologic Event Occured"),
                SRCDOM = case_when(EVNTDESC ==  "Dematologic Event Occured" ~ "ADAE",
                                   TRUE ~ "ADSL"),
                SRCVAR = case_when(ADT != ASTDT & EOSSTT == "DEATH" ~ "DTHDT",
                                   ADT != ASTDT & EOSSTT == "COMPLETED" ~ "RFENDT",
                                   ADT != ASTDT & EOSSTT == "DISCONTINUED" ~ "EOSDT",
                                   ADT == ASTDT ~ "ASTDT"),
                SRCSEQ = case_when(ADT == ASTDT ~ as.numeric(AESEQ))
                ) %>%
  dplyr::select(STUDYID, SITEID, USUBJID, AGE, AGEGR1, AGEGR1N, RACE, RACEN, SEX, TRTSDT,
                TRTEDT, TRTDUR, TRTP, TRTA, TRTAN, PARAM, PARAMCD, AVAL, STARTDT, ADT,
                CNSR, EVNTDESC, SRCDOM, SRCVAR, SRCSEQ, SAFFL, EOSDT)

adtte <- adtte %>%
  dplyr::select(-EOSDT)

str(adtte$AVAL)

# Add Labels --------------------------------------------------------------

labs <- sapply(colnames(adtte), FUN = function(x) attr(adtte[[x]], "label"))
labs[unlist(lapply(labs,is.null))]

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
attr(adtte[["AVAL"]], "units") <- NULL

labsupdated <- sapply(colnames(adtte), FUN = function(x) attr(adtte[[x]], "label"))
labsupdated[unlist(lapply(labsupdated,is.null))]

# read original ADTTE -----------------------------------------------------

prod <- haven::read_xpt(file.path("adam", "adtte.xpt"))
labsprod <- sapply(colnames(prod), FUN = function(x) attr(prod[[x]], "label"))
# unique(prod[["PARAM"]])
# unique(prod[["EVNTDESC"]])

# QC dev vs prod ----------------------------------------------------------

## Metadata compare (labels)

difflabels <- dplyr::setdiff(labsprod, labsupdated)
discr_labels <- unlist(labsprod)[which(unlist(labsprod) %in% difflabels)]

## Content check using inhouse package

# dfcompare( 
#      file = "tlcompare"
#     ,left = prod
#     ,right = adtte
#     ,keys = c("STUDYID", "USUBJID")
#     ,showdiffs = 1000
#     ,debug = FALSE
# )


# END of Code -------------------------------------------------------------