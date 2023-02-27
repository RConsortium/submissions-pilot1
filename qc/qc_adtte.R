###########################################################################
#' developers : Robert Devine/
#' date: 26FEB2023
#' modification History:
#' QC ADTTE
###########################################################################
library(haven)
library(diffdf)

adtte <- read_xpt(file.path("submission", "datasets", "adtte.xpt"))
qc_adtte <- read_xpt(file.path("adam", "adtte.xpt"))

diffdf(adtte, qc_adtte, keys = c("STUDYID", "USUBJID", "PARAMCD", "SRCDOM", "STARTDT"))
