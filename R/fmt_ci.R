#' @noRd
fmt_ci <- function(data, columns = c("lower.CL", "upper.CL"), decimals = 2) {
  .lower <- formatC(data[[columns[[1]]]], digits = decimals, format = "f", flag = "0")
  .upper <- formatC(data[[columns[[2]]]], digits = decimals, format = "f", flag = "0")
  paste0("(", .lower, ", ", .upper, ")")
}
