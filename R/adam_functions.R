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
