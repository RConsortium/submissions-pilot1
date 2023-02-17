###########################################################################
#' developers : Thomas Neitmann/
#' date: 08FEB2023
#' modification History:
#' QC ADLBC
###########################################################################
library(haven)
library(diffdf)

adlbc <- read_xpt(file.path("submission", "datasets", "adlbc.xpt"))
qc_adlbc <- read_xpt(file.path("adam", "adlbc.xpt"))

diffdf(adlbc, qc_adlbc, keys = c("STUDYID", "USUBJID", "AVISIT", "LBSEQ"))


# Dataset of missing observations
missing <- anti_join(qc_adlbc, adlbc, by = c("USUBJID",  "LBSEQ", "AVISIT"))
