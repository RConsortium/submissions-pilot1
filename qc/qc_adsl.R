###########################################################################
#' developers : Thomas Neitmann/
#' date: 16DEC2022
#' modification History:
#' QC ADSL
###########################################################################
library(haven)
library(diffdf)

adsl <- read_xpt(file.path("submission", "datasets", "adsl.xpt"))
qc_adsl <- read_xpt(file.path("adam", "adsl.xpt"))

diffdf(adsl, qc_adsl, keys = c("STUDYID", "USUBJID"))
