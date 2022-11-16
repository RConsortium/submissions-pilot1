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

## First dermatological event - (ADAE.CQ01NAM ADAE.AOCC01FL)
event <- adae %>%
  dplyr::filter(AOCC01FL == "Y", CQ01NAM == "DERMATOLOGIC EVENTS", SAFFL =="Y") %>%
  dplyr::select(STUDYID, USUBJID, AESEQ, AOCC01FL, CQ01NAM, ASTDT)

## get censor events - discontinuation, completed, death
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
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
  )

censor <- adsl %>%
  dplyr::filter(SAFFL == "Y") %>%
  dplyr::select(STUDYID, SITEID, USUBJID, AGE, AGEGR1, AGEGR1N, RACE, RACEN,
                SEX, TRTSDT, TRTEDT, TRTDURD, EOSDT, EOSSTT, SAFFL,
                TRT01P, TRT01PN, TRT01A, TRT01AN) %>%
  dplyr::rename(TRTDUR = TRTDURD, #not sure why it is not consistent across
                TRTPN = TRT01PN,
                TRTP = TRT01P,
                TRTAN = TRT01AN,
                TRTA = TRT01A) 

## Merge dates and take the earliest as enddt
adtte <- event %>%
  dplyr::left_join(censor, by = c("STUDYID", "USUBJID")) %>%
  dplyr::mutate(PARAM = "Time to First Dermatologic Event",
                PARAMCD = "TTDE",
                STARTDT = TRTSDT,
                ADT = min(EOSDT, ASTDT),
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
                CNSR, EVNTDESC, SRCDOM, SRCVAR, SRCSEQ, SAFFL)


# Add Labels --------------------------------------------------------------

labs <- sapply(colnames(adtte), FUN = function(x) attr(adtte[[x]], "label"))
labs[unlist(lapply(labs,is.null))]

attr(adtte$PARAM, "label") <- "Parameter"
attr(adtte$PARAMCD, "label") <- "Parameter Code"
attr(adtte$ADT, "label") <- "Parameter Code"
attr(adtte$CNSR, "label") <- "Parameter Code"
attr(adtte$EVNTDESC, "label") <- "Censor"
attr(adtte$SRCDOM, "label") <- "Source Domain"
attr(adtte$SRCVAR, "label") <- "Source Variable"
attr(adtte$SRCSEQ, "label") <- "Source Sequence Number"

labsupdated <- sapply(colnames(adtte), FUN = function(x) attr(adtte[[x]], "label"))
labsupdated[unlist(lapply(labsupdated,is.null))]

# read original ADTTE -----------------------------------------------------

prod <- haven::read_xpt(file.path("adam", "adtte.xpt"))
labsprod <- sapply(colnames(prod), FUN = function(x) attr(prod[[x]], "label"))
# unique(prod$PARAM)
# unique(prod$EVNTDESC)

# QC dev vs prod ----------------------------------------------------------

## Metadata compare (labels)


## Content check


# END of Code -------------------------------------------------------------