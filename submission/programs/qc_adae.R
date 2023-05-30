###########################################################################
#' developers : Thomas Neitmann
#' date: 16DEC2022
#' QC ADAE
#' modification History:
#'  laxamanaj 08FEB2023 : updated to QC ADAE
###########################################################################
library(haven)
library(diffdf)


# QC/Check against original TDF ADAE dataset
# -------------------------------------------#
adae <- read_xpt(file.path("submission", "adam", "adae.xpt"))
adae_orig <- read_xpt(file.path("adam", "adae.xpt"))

# Compare
#---------#
diffdf(adae, adae_orig, keys = c("STUDYID", "USUBJID", "AESEQ"))
