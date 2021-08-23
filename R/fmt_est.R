#' The function assume 1 or 2 column.
#'   If there is only 1 column, only represent mean
#'   If there are 2 column, represent mean (sd) or mean(se)
#' Decimals will understand the number will be formatted as x.x(x.xx)
#' @noRd
fmt_est <- function(data, columns = c("mean", "sd"), decimals = c(1, 2)) {
  .mean <- formatC(data[[columns[[1]]]], digits = decimals[1], format = "f", flag = "0")
  if (length(columns) > 1) {
    .sd <- formatC(data[[columns[[2]]]], digits = decimals[2], format = "f", flag = "0")
    paste0(.mean, " (", .sd, ")")
  } else {
    .mean
  }
}