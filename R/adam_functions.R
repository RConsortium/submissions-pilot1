#' Derive pooled site group
#'
#' `format_sitegr1` derives the `SITEGR1` variable, which pools sites with fewer than 3 subjects in any one treatment group.
#'
#' @param x The input vector of values to format
#'
#' @export
format_sitegr1 <- function(x) {
  case_when(
    x %in% c("702", "706", "707", "711", "714", "715", "717") ~ "900",
    TRUE ~ x
  )
}

#' Derive pooled age group
#'
#' `format_agegr1` derives the `AGEGR1` variable, which pools subjects into age groups.
#'
#' @inheritParams format_sitegr1
#'
#' @export
format_agegr1 <- function(x) {
  case_when(
    x < 65 ~ "<65",
    between(x, 65, 80) ~ "65-80",
    x > 80 ~ ">80",
  )
}

#' Derive pooled age group (N)
#'
#' `format_agegr1n` derives the `AGEGR1N` variable, which is the numeric code for `AGEGR1`.
#'
#' @inheritParams format_sitegr1
#'
#' @export
format_agegr1n <- function(x) {
  case_when(
    x < 65 ~ 1,
    between(x, 65, 80) ~ 2,
    x > 80 ~ 3,
  )
}

#' Derive race (N)
#'
#' `format_racen` derives the `RACEN` variable, which is the numeric code for `RACE`.
#'
#' @inheritParams format_sitegr1
#'
#' @export
format_racen <- function(x) {
  case_when(
    x == "WHITE" ~ 1,
    x == "BLACK OR AFRICAN AMERICAN" ~ 2,
    x == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 6
  )
}

#' Derive pooled baseline BMI group
#'
#' `format_bmiblgr1` derives the `BMIBLGR1` variable, which pools subjects into baseline BMI groups.
#'
#' @inheritParams format_sitegr1
#'
#' @export
format_bmiblgr1 <- function(x) {
  case_when(
    !is.na(x) & x < 25 ~ "<25",
    25 <= x & x < 30 ~ "25-<30",
    30 <= x ~ ">=30"
  )
}

#' Derive pooled disease duration group
#'
#' `format_durdsgr1` derives the `DURDSGR1` variable, which pools subjects into disease duration groups.
#'
#' @inheritParams format_sitegr1
#'
#' @export
format_durdsgr1 <- function(x) {
  case_when(
    !is.na(x) & x < 12 ~ "<12",
    12 <= x ~ ">=12"
  )
}

#' Derive subject's disposition reason(s)
#'
#' `format_dcsreas` derives the `DCSREAS` variable, which contains subjects' disposition reason(s).
#'
#' @inheritParams format_sitegr1
#'
#' @export
format_dcsreas <- function(x) {
  case_when(
    x == "ADVERSE EVENT" ~ "Adverse Event",
    x == "STUDY TERMINATED BY SPONSOR" ~ "Sponsor Decision",
    x == "DEATH" ~ "Death",
    x == "WITHDRAWAL BY SUBJECT" ~ "Withdrew Consent",
    x == "PHYSICIAN DECISION" ~ "Physician Decision",
    x == "PROTOCOL VIOLATION" ~ "Protocol Violation",
    x == "LOST TO FOLLOW-UP" ~ "Lost to Follow-up",
    x == "LACK OF EFFICACY" ~ "Lack of Efficacy"
  )
}
