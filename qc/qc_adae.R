###########################################################################
#' developers : Thomas Neitmann/
#' date: 16DEC2022
#' QC ADAE
#' modification History:
#'  laxamanaj 08FEB2023 : updated to QC ADAE
###########################################################################
library(haven)
library(diffdf)
library(arsenal)

# -------------------------------------------#
# QC/Check against original TDF ADAE dataset #
# -------------------------------------------#
adae <- read_xpt(file.path("submission", "datasets", "adae.xpt")) %>%
  convert_blanks_to_na()
adae_orig <- read_xpt(file.path("adam", "adae.xpt")) %>%
  convert_blanks_to_na()

#---------#
# Compare #
#---------#
diffdf(adae, adae_orig, keys = c("STUDYID", "USUBJID", "AESEQ")) # using diffdf{}
summary(comparedf(adae, adae_orig, by = c("STUDYID", "USUBJID", "AESEQ"))) # using arsenal{}
