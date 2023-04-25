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

#' Derive subject's end of study status
#'
#' `format_eosstt` derives the `EOSSTT` variable, which contains subject's end of study status
#'
#'
#' @inheritParams format_sitegr1
#'
#' @export
format_eosstt <- function(x) {
  case_when(
    x %in% c("SCREEN FAILURE", "SCREENING NOT COMPLETED") ~ "NOT STARTED",
    x == "COMPLETED" ~ "COMPLETED",
    !x %in% c("COMPLETED", "SCREEN FAILURE", "SCREENING NOT COMPLETED") &
      !is.na(x) ~ "DISCONTINUED",
    TRUE ~ "ONGOING"
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

#' Round the value based on SAS rounding convention
#'
#' `round_sas` rounds the values in its first argument to the specified number
#' of decimal places (default 0) (align SAS rounding convention)
#'
#' @param x a numeric vector.
#' @param digits integer indicating the number of decimal places (round) or 
#' significant digits to be used.
#'
#' @export
round_sas <- function(x, digits = 0) {
  # x is the value to be rounded
  # digits is the precision of the rounding
  scale <- 10^digits
  y <- trunc(x * scale + sign(x) * 0.5) / scale
  # Return the rounded number
  return(y)
}
