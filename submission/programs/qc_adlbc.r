###########################################################################
#' developers : Thomas Neitmann
#' date: 08FEB2023
#' modification History:
#' QC ADLBC
###########################################################################
library(haven)
library(diffdf)

adlbc <- read_xpt(file.path("submission", "adam", "adlbc.xpt"))
qc_adlbc <- read_xpt(file.path("adam", "adlbc.xpt"))

diffdf(adlbc, qc_adlbc, keys = c("STUDYID", "USUBJID", "AVISIT", "LBSEQ"))
