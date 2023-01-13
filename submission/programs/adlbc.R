###########################################################################
#' developers : Steven Haesendonckx/
#' date: 28NOV2022
#' modification History:
#' Dadong Zhang, 17DEC2022
#' Nicole Jones, 12Jan2023
###########################################################################

# Set up ------------------------------------------------------------------
rm(list = ls())
library(haven)
library(admiral)
library(dplyr)
library(labelled) # for variable labelling

# read source -------------------------------------------------------------
# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values

# Read and convert NA for SDTM DATASET
## Laboratory Tests Results (LB)
lb <- admiral::convert_blanks_to_na(haven::read_xpt(file.path("sdtm", "lb.xpt")))
## Supplemental Qualifiers for LB (SUPPLB)
supplb <- admiral::convert_blanks_to_na(haven::read_xpt(file.path("sdtm", "supplb.xpt")))
## Subject Visits (SV)
sv <- admiral::convert_blanks_to_na(haven::read_xpt(file.path("sdtm", "sv.xpt")))

# Read and convert NA for ADaM DATASET
## Subject-Level Analysis
adsl <- admiral::convert_blanks_to_na(haven::read_xpt(file.path("adam", "adsl.xpt")))
## Analysis Dataset Lab Blood Chemistry
prodc <- admiral::convert_blanks_to_na(haven::read_xpt(file.path("adam", "adlbc.xpt")))

#Variables for programming
toprogram <- setdiff(colnames(prodc), c(colnames(lb), unique(supplb[["QNAM"]])))

# Formats -----------------------------------------------------------------
## map parameter code and parameter
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
  labelled::set_variable_labels(LBSEQ = "Sequence Number") %>%
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
  admiral::derive_vars_dy(reference_date = TRTSDT, source_vars = vars(ADT)) %>%
  labelled::set_variable_labels(ADTM = "Derived datatime object from LBDTC", #these labels don't match final dataset?
                                ATMF = "ADTC Imputation flag (at second)",
                                ADT = "Derived date from LBDTC",
                                ADY = "Derived relative day ADT from TRTSDT")


# adlb02[which(nchar(adlb02$LBDTC) == 16), c("LBDTC", "ADTM")]
# adlb02[which(nchar(adlb02$LBDTC) == 16), c("LBDTC", "ADTM", "ATMF")]
# adlb02[which(nchar(adlb02$LBDTC) == 10), c("LBDTC", "ADTM", "ADT")]
# adlb02[which(nchar(adlb02$LBDTC) != 10 & nchar(adlb02$LBDTC) != 16), c("LBDTC", "ADTM", "ADT")]

# AVAL(C) -----------------------------------------------------------------
# No imputations are done for values below LL or above UL

adlb03 <- adlb02 %>%
  dplyr::mutate(AVAL = LBSTRESN,
                AVALC = ifelse(!is.na(AVAL), LBSTRESC, NA)) %>% #added ! so that AVALC is populated
  labelled::set_variable_labels(AVALC = "Impute AVAL with LBSTRESC")

# Parameter ---------------------------------------------------------------

adlb04 <- adlb03 %>%
  dplyr::mutate(PARAM = paste0(LBTEST, " (", LBSTRESU,")"),
                PARAMCD = LBTESTCD,
                PARAMN = format_paramn(LBTESTCD),
                PARCAT1 = "CHEM" #changed to match prod dataset
  ) %>%
  labelled::set_variable_labels(PARAM = "Chemistry (unit)",
                                PARAMN = "Chemistry Sequence Number",
                                PARCAT1 = "Parameter Category 1") #added label

# Baseline ----------------------------------------------------------------

bsl <- adlb04 %>%
  dplyr::filter(LBBLFL=='Y') %>% #updated to use LBBLFL to get baseline
  dplyr::arrange(STUDYID, USUBJID, PARAMCD, ADT, ADTM, LBSEQ) %>%
  dplyr::group_by(STUDYID, USUBJID, PARAMCD) %>%
  dplyr::slice(n()) %>%
  dplyr::mutate(BASE = AVAL, B1LO = LBSTNRLO, B1HI = LBSTNRHI) %>% #moved ABLFL code below
  dplyr::select(STUDYID, USUBJID, PARAMCD, BASE, B1LO, B1HI) #LBSEQ isn't needed for merging
  

adlb05 <- adlb04 %>%
  dplyr::left_join(bsl, by = c("STUDYID", "USUBJID", "PARAMCD")) %>%
  dplyr::mutate(CHG = ifelse(is.na(LBBLFL),  AVAL - BASE, NA),
                ABLFL = LBBLFL) %>% #CHG should be NA for baseline values
  labelled::set_variable_labels(ABLFL = "Baseline Flag",
                                CHG = "Change from Baseline")


# VISITS ------------------------------------------------------------------

eot <- adlb05 %>%
  dplyr::filter(ENDPOINT == "Y") %>%
  dplyr::mutate(AVISIT = "End of Treatment",
                AVISITN = 99)

adlb06 <- adlb05 %>%
  dplyr::filter(grepl("WEEK", VISIT, fixed = TRUE) | 
                  grepl("UNSCHEDULED", VISIT, fixed = TRUE) |
                  grepl("SCREENING", VISIT, fixed = TRUE)) %>% #added conditions to include screening and unscheduled visits
  dplyr::mutate(AVISIT = dplyr::case_when(ABLFL == "Y" ~ "Baseline",
                                          grepl("UNSCHEDULED", VISIT) ==TRUE ~ "",
                                          TRUE ~ stringr::str_to_sentence(VISIT)),
                AVISITN = dplyr::case_when(AVISIT == "Baseline"~ 0,
                                           AVISIT =="" ~ -1,
                                          TRUE ~ as.numeric(gsub("[^0-9]","",AVISIT)) 
                                          )) %>%
  sjmisc::add_rows(eot) %>% #keeps labelling
  #dplyr::bind_rows(eot) %>%
  dplyr::mutate(ANL01FL = ifelse(grepl('UN', VISIT), "", "Y"), #how should ANL01FL be defined? doesn't seem to match up with prod dataset
                AVISITN = ifelse(AVISITN == -1, "", AVISITN))


# Limits ------------------------------------------------------------------

adlb07 <- adlb06 %>%
  dplyr::arrange(STUDYID, USUBJID, PARAMCD, ANL01FL, ADT, ADTM, LBSEQ) %>%
  dplyr::mutate(DIFF = ifelse(ANL01FL == "Y" & dplyr::lag(ANL01FL) == "Y", abs(AVAL-dplyr::lag(AVAL)), NA)) %>%
  dplyr::mutate(ANRIND = dplyr::case_when(AVAL < 0.5*LBSTNRLO | abs(DIFF) < ((LBSTNRHI-LBSTNRLO) * 0.50) ~ "L",
                                          AVAL > 1.5* LBSTNRHI | abs(DIFF) > ((LBSTNRHI-LBSTNRLO) * 0.50) ~ "H",
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
              ) %>%
  dplyr::arrange(STUDYID, USUBJID, PARAMCD, ADT, ADTM) %>%
  dplyr::group_by(STUDYID, USUBJID, PARAMCD) %>%
  dplyr::mutate(AENTMTFL = ifelse(VISITNUM == 12, "Y", ifelse((row_number() == n()-1 | row_number() == n()) & VISITNUM < 12, "Y", ""))) %>% #n-1 to avoid avisitn = 99
  dplyr::ungroup()


# Treatment Vars ------------------------------------------------------------

adlb08 <-adlb07 %>% 
  dplyr::mutate(TRTP = TRT01P,
                TRTPN = TRT01PN,
                TRTA = TRT01A,
                TRTAN = TRT01AN) %>% 
  select(STUDYID, SUBJID, USUBJID, TRTP,
         TRTPN, TRTA, TRTAN, TRTSDT,
         TRTEDT, AGE, AGEGR1, AGEGR1N,
         RACE, RACEN, SEX, COMP24FL,
         DSRAEFL, SAFFL, AVISIT, AVISITN,
         ADY, ADT, VISIT, VISITNUM, PARAM,
         PARAMCD, PARAMN, PARCAT1, AVAL,
         BASE, CHG, A1LO, A1HI, R2A1LO,
         R2A1HI, BR2A1LO, BR2A1HI, ANL01FL,
         ALBTRVAL, ANRIND, BNRIND, ABLFL,
         AENTMTFL, LBSEQ, LBNRIND, LBSTRESN
  )

 

