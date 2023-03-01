###########################################################################
#' developers : Kangjie Zhang
#' date: 29NOV2022
#' modification History:
#' QC ADADAS
###########################################################################
library(haven)
library(diffdf)
library(dplyr)

adadas <- read_xpt(file.path("submission/datasets/", "adadas.xpt"))
qc_adadas <- read_xpt(file.path("adam", "adadas.xpt"))

diffdf(adadas, qc_adadas, keys = c("USUBJID", "PARAMCD", "AVISIT", "ADT"))
