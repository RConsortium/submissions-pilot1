###########################################################################
#' developers : Steven Haesendonckx/
#' date: 28NOV2022
#' modification History:
#' 
###########################################################################

# Set up ------------------------------------------------------------------

library(haven)
library(admiral)
library(dplyr)

# read source -------------------------------------------------------------
# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values


lb <- admiral::convert_blanks_to_na(haven::read_xpt(file.path("sdtm", "lb.xpt")))
supplb <- admiral::convert_blanks_to_na(haven::read_xpt(file.path("sdtm", "supplb.xpt")))
sv <- admiral::convert_blanks_to_na(haven::read_xpt(file.path("sdtm", "sv.xpt")))

adsl <- admiral::convert_blanks_to_na(haven::read_xpt(envsetup::read_path(adam, "adsl.xpt")))

prodc <- admiral::convert_blanks_to_na(haven::read_xpt(file.path(adam[2], "adlbc.xpt")))

toprogram <- setdiff(colnames(prodc), c(colnames(lb), unique(supplb[["QNAM"]])))

# Formats -----------------------------------------------------------------

format_paramn <- function(x){
  dplyr::case_when(
    x == "SODIUM" ~ 18,
    x == "K" ~ 19,
    x == "CL" ~ 20,
    x == "BILI" ~ 21,
    x == "ALP" ~ 22,
    x == "GGT" ~ 23,
    x == "ALT" ~ 24,
    x == "AST" ~ 25,
    x == "BUN" ~ 26,
    x == "CREAT" ~ 27,
    x == "URATE" ~ 28,
    x == "PHOS" ~ 29,
    x == "CA" ~ 30,
    x == "GLUC" ~ 31,
    x == "PROT" ~ 32,
    x == "ALB" ~ 33,
    x == "CHOL" ~ 34,
    x == "CK" ~ 35
  )
}
# Add supplemental information --------------------------------------------

sup <- supplb %>%
  dplyr::select(STUDYID, USUBJID, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL) %>%
  tidyr::pivot_wider(id_cols  = c(STUDYID, USUBJID, IDVARVAL),
                     names_from = QNAM,
                     values_from = QVAL) %>%
  dplyr::mutate(LBSEQ = as.numeric(IDVARVAL)) %>%
  dplyr::select(-IDVARVAL)

adlb00 <- lb %>%
  dplyr::left_join(sup, by = c("STUDYID", "USUBJID", "LBSEQ")) %>%
  dplyr::filter(LBCAT == "CHEMISTRY")

# ADSL information --------------------------------------------------------

adsl <- adsl %>%
  dplyr::select(STUDYID, USUBJID, TRT01PN, TRT01P, TRT01AN, TRT01A, TRTSDT, TRTEDT, AGE, AGEGR1, AGEGR1N, RACE, RACEN, SEX,
                COMP24FL, DSRAEFL, SAFFL) %>%
  dplyr::mutate(SUBJID = sub(".*-", "", USUBJID))


adlb01 <- adlb00 %>%
  dplyr::left_join(adsl, by = c("STUDYID", "USUBJID"))

# Dates -------------------------------------------------------------------
# x <- sapply(lb$LBDTC, FUN = nchar)
# x[x!=16]

adlb02 <- adlb01 %>%
  admiral::derive_vars_dtm(
    new_vars_prefix = "A",
    dtc = LBDTC,
    highest_imputation = "s", # admiral assumes seconds are present before populating ADTM
    ignore_seconds_flag = T
  ) %>%
  admiral::derive_vars_dt(
    new_vars_prefix = "A",
    dtc = LBDTC,
    highest_imputation = "n"
  ) %>%
  admiral::derive_vars_dy(reference_date = TRTSDT, source_vars = vars(ADT))


# adlb02[which(nchar(adlb02$LBDTC) == 16), c("LBDTC", "ADTM")]
# adlb02[which(nchar(adlb02$LBDTC) == 16), c("LBDTC", "ADTM", "ATMF")]
# adlb02[which(nchar(adlb02$LBDTC) == 10), c("LBDTC", "ADTM", "ADT")]
# adlb02[which(nchar(adlb02$LBDTC) != 10 & nchar(adlb02$LBDTC) != 16), c("LBDTC", "ADTM", "ADT")]

# AVAL(C) -----------------------------------------------------------------
# No imputations are done for values below LL or above UL

adlb03 <- adlb02 %>%
  dplyr::mutate(AVAL = LBSTRESN,
                AVALC = ifelse(is.na(AVAL), LBSTRESC, NA))

# Parameter ---------------------------------------------------------------

adlb04 <- adlb03 %>%
  dplyr::mutate(PARAM = paste0(LBTEST, " (", LBSTRESU,")"),
                PARAMCD = LBTESTCD,
                PARAMN = format_paramn(LBTESTCD),
                PARCAT1 = "Chemistry"
  )

# Baseline ----------------------------------------------------------------

bsl <- adlb04 %>%
  dplyr::filter(ADT <= TRTSDT) %>%
  dplyr::arrange(STUDYID, USUBJID, PARAMCD, ADT, ADTM, LBSEQ) %>%
  dplyr::group_by(STUDYID, USUBJID, PARAMCD) %>%
  dplyr::slice(n()) %>%
  dplyr::mutate(BASE = AVAL, ABLFL = "Y", B1LO = LBSTNRLO, B1HI = LBSTNRHI) %>%
  dplyr::select(STUDYID, USUBJID, PARAMCD, LBSEQ, BASE, ABLFL, B1LO, B1HI)

adlb05 <- adlb04 %>%
  dplyr::left_join(bsl, by = c("STUDYID", "USUBJID", "PARAMCD", "LBSEQ")) %>%
  dplyr::mutate(CHG = AVAL - BASE)

# VISITS ------------------------------------------------------------------

eot <- adlb05 %>%
  dplyr::filter(ENDPOINT == "Y") %>%
  dplyr::mutate(AVISIT = "End of Treatment",
                AVISITN = 99)

adlb06 <- adlb05 %>%
  dplyr::filter(grepl("WEEK", VISIT, fixed = TRUE)) %>%
  dplyr::mutate(AVISIT = dplyr::case_when(ABLFL == "Y" ~ "Baseline",
                                          TRUE ~ VISIT),
                AVISITN = dplyr::case_when(ABLFL == "Y" ~ 0,
                                          TRUE ~ as.numeric(gsub(".*?([0-9]+).*", "\\1", VISIT)) )) %>%
  dplyr::bind_rows(eot)


# Limits ------------------------------------------------------------------

adlb07 <- adlb06 %>%
  dplyr::mutate(ANRIND = dplyr::case_when(AVAL < 0.5*LBSTNRLO ~ "L",
                                          AVAL > 1.5* LBSTNRHI ~ "H",
                                          TRUE ~ 'N'),
                BNRIND = dplyr::case_when(BASE < 0.5*LBSTNRLO ~ "L",
                                          BASE > 1.5* LBSTNRHI ~ "H",
                                          TRUE ~ 'N'),
                
                A1LO = LBSTNRLO,
                A1HI = LBSTNRHI,
                R2A1LO = AVAL / A1HI,
                R2A1HI = AVAL / A1LO,
                BR2A1LO = AVAL / B1HI,
                BR2A1HI = AVAL / B1LO,
                ALBTRVAL = max((LBSTRESN-(1.5*LBSTNRHI)), ((.5*LBSTNRLO) - LBSTRESN))
              )


"ANL01FL"  "AENTMTFL"

ANL01FL Analysis Record Flag 1

AENTMTFL Last value in treatment visit text 1 Y_BLANK  Last observed value for this lab parameter during
  treatment phase: 'Y' if VISITNUM=12, if subject discontinues prior to VISIT 12, then this variable is set to
  'Y' if this is the last assessment of this analyte for the subject 

