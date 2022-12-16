###########################################################################
#' developers : Kangjie Zhang/
#' date: 29NOV2022
#' modification History:
#' QC ADADAS
###########################################################################

# Set up
library(haven)
library(diffdf)
library(dplyr)

# read source
adadas <- haven::read_xpt(file.path("submission/datasets/", "adadas.xpt"))
qc_adadas <- haven::read_xpt(file.path("adam", "adadas.xpt"))

# compare R generated submission/datasets/adadas with adam/adadas
diffdf(adadas, qc_adadas, keys = c("USUBJID", "PARAMCD", "AVISIT", "ADT"))
