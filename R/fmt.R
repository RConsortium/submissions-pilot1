#' Format numeric value
#'
#' @inheritParams base::formatC
#'
#' @examples
#' fmt_num(1.25, digits = 1)
#' @export
fmt_num <- function(x, digits, width = digits + 4) {
  formatC(x,
    digits = digits,
    format = "f",
    width = width
  )
}

#' Format point estimator
#'
#' @param .mean mean of an estimator.
#' @param .sd sd of an estimator.
#' @param digits number of digits for `.mean` and `.sd`.
#'
#' @examples
#' fmt_est(1.25, 0.5)
#' @export
fmt_est <- function(.mean,
                    .sd,
                    digits = c(1, 2)) {
  .mean <- fmt_num(.mean, digits[1], width = digits[1] + 4)
  .sd <- fmt_num(.sd, digits[2], width = digits[2] + 3)
  paste0(.mean, " (", .sd, ")")
}

#' Format confidence interval
#'
#' @param .est an estimator.
#' @param .lower lower confidence interval bound of an estimator.
#' @param .upper upper confidence interval bound of an estimator.
#' @param digits number of digits for `.est`, `.lower`, and `.upper`.
#' @param width the total field width.
#'
#' @examples
#' fmt_ci(1, -0.25, 1.32)
#' @export
fmt_ci <- function(.est,
                   .lower,
                   .upper,
                   digits = 2,
                   width = digits + 3) {
  .est <- fmt_num(.est, digits, width)
  .lower <- fmt_num(.lower, digits, width)
  .upper <- fmt_num(.upper, digits, width)
  paste0(.est, " (", .lower, ",", .upper, ")")
}

#' Format p-Value
#'
#' @param .p a p-value.
#' @param digits number of digits for `.est`, `.lower`, and `.upper`.
#'
#' @examples
#' fmt_pval(0.2)
#' @export
fmt_pval <- function(.p, digits = 3) {
  scale <- 10^(-1 * digits)
  p_scale <- paste0("<", digits)
  ifelse(.p < scale, p_scale, fmt_num(.p, digits = digits))
}
