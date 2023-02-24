###########################################################################
#' developpers : Steven Haesendonckx/Bingjun Wang
#' date: 13NOV2022
#' modification History:
#' 1. Try to use admiral to generate ADTTE
###########################################################################

# Set up ------------------------------------------------------------------

fcts <- c("eff_models.R", "fmt.R", "helpers.R", "Tplyr_helpers.R")
invisible(sapply(fcts, FUN = function(x) source(file.path("R/", x), )))

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


# event <- adae %>%
#   dplyr::filter(AOCC01FL == "Y", CQ01NAM == "DERMATOLOGIC EVENTS", SAFFL =="Y") %>%
#   dplyr::select(STUDYID, USUBJID, AESEQ, AOCC01FL, CQ01NAM, ASTDT)

event <- event_source(
  dataset_name = "adae",
  filter = AOCC01FL == "Y" & CQ01NAM == "DERMATOLOGIC EVENTS" & SAFFL =="Y",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "Dematologic Event Occured",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)


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
  ) %>%
  # Analysis uses DEATH date rather than discontinuation when subject dies even if discontinuation occurs before death
  # Observed through QC - However not described in specs
  dplyr::mutate(EOS2DT = case_when(DCDECOD == "DEATH" ~ as.Date(RFENDTC),
                                   DCDECOD != "DEATH" ~ EOSDT)
  )

censor <- censor_source(
  dataset_name = "adsl",
  date = EOS2DT,
  set_values_to = vars(
    EVNTDESC = "Study Completion Date",
    SRCDOM = "ADSL",
    SRCVAR = "RFENDT"
  )
)


adtte <- derive_param_tte(
  dataset_adsl = adsl,
  start_date = TRTSDT,
  event_conditions = list(event),
  censor_conditions = list(censor),
  source_datasets = list(adsl = adsl, adae = adae),
  set_values_to = vars(PARAMCD = "TTDE", PARAM = "Time to First Dermatologic Event")
) |> 
  derive_vars_duration(
    new_var = AVAL,
    start_date = STARTDT,
    end_date = ADT
) |> 
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = vars(AGE, AGEGR1, AGEGR1N, RACE, RACEN, SAFFL, SEX, SITEID, TRT01A,
                    TRT01AN, TRTDURD, TRTEDT, TRT01P, TRTSDT),
    by_vars = vars(STUDYID, USUBJID)
) |> 
  dplyr::rename(TRTA = TRT01A,
                   TRTAN = TRT01AN,
                   TRTDUR = TRTDURD,
                   TRTP = TRT01P)

# Add Labels --------------------------------------------------------------

labs <- sapply(colnames(adtte), FUN = function(x) attr(adtte[[x]], "label"))
labs[unlist(lapply(labs,is.null))]

adtte[["AVAL"]] <- as.numeric(adtte[["AVAL"]])
adtte[["CNSR"]] <- as.numeric(adtte[["CNSR"]])

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
attr(adtte[["STUDYID"]], "label") <- "Study Identifier"
attr(adtte[["USUBJID"]], "label") <- "Unique Subject Identifier"

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
# 
# all_equal(prod, adtte)

# Output ------------------------------------------------------------------

xportr::xportr_write(adtte, file.path("submission/datasets/adtte.xpt"), label = "AE Time To 1st Derm. Event Analysis")

# END of Code -------------------------------------------------------------


