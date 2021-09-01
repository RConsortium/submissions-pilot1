#' @noRd
fmt_pval <- function(data, columns = "p.value", decimals = 3) {
  scale <- 10^(-1 * decimals)
  p_scale <- paste0("<", scale)
  ifelse(data[[columns[[1]]]] < scale, p_scale,
          formatC(data[[columns[[1]]]], digits = decimals, format = "f", flag = "0")
  )
}