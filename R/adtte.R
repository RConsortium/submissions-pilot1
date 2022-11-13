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

# read source -------------------------------------------------------------

adsl <- haven::read_xpt(file.path("adam", "adsl.xpt"))
adae <- haven::read_xpt(file.path("adam", "adae.xpt"))


## First dermatological event - (ADAE.CQ01NAM ADAE.AOCC01FL)
event <- adae %>%
  dplyr::filter(AOCC01FL == "Y", CQ01NAM == "DERMATOLOGIC EVENTS")

## get censor events
censor <- adsl %>%
  dplyr::filer(SAFFL == "Y") %>%
  dplyr::select(STUDYID, USUBJID, DTHDT, discontinuationdate_EOSDT_toderive, completiondate_EOSDT_toderive)

## Merge dates and take the earliest as enddt
adtte <- event %>%
  dplyr::inner_join(censor, by = c("STUDYID", "USUBJID")) %>%
  dplyr::mutate(PARAM = "Time to First Dermatologic Event",
                PARAMCD = "TTDE",
                STARTDT = TRTSDT,
                ADT = min(DTHDT, EOSDT, AESTDT),
                AVAL = ENDDT-STARTDT+1,
                CNSR = ifelse(ENDDT = ASTDT, 0, 1),
                EVNTDESV = ifelse(ENDDT == ASTDT, "Dematologic Event Occured",
                                  ifelse(ENDDT == DTHDT, "DEATH", "...")),
                )



# read original ADTTE -----------------------------------------------------

prod <- haven::read_xpt(file.path("adam", "adtte.xpt"))
unique(prod$PARAM)

