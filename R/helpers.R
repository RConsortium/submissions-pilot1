#' Add a padding row below data
#'
#' @param .data Data to pad
#' @param n Number of rows to pad
#' 
#' @importFrom stringr str_pad
#'
#' @return Dataframe with extra blank rows
#' @export
pad_row <- function(.data, n=1) {
  .data[(nrow(.data)+1):(nrow(.data)+n), ] <- ""
  .data
}

#' Number formatter
#'
#' Format numbers for presentation, with proper rounding of data
#' 
#' @param var Variable to format
#' @param digits Desired number of decimal places
#' @param size String size
#' @param int_len Space allotted for integer side of the decimal
#'
#' @return Formatted string
#' @export 
num_fmt <- Vectorize(function(var, digits=0, size=10, int_len=3) {
  # Formats summary stat strings to align display correctly
  
  if (is.na(var)) return('')
  
  # Set nsmall to input digits
  nsmall = digits
  
  # Incremement digits for to compensate for display
  if (digits > 0) {
    digits = digits + 1
  }
  
  # Form the string
  return(str_pad(
    format(
      # Round
      round(var, nsmall),
      # Set width of format string
      width=(int_len+digits),
      # Decimals to display
      nsmall=nsmall
    ),
    # Overall width padding
    side='right', size
  ))
})