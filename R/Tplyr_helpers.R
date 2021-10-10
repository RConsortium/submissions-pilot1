#' Nest Row Labels in a Tplyr table
#' 
#' This is a (high ungeneralized) helper function. Current function assumes that 
#' row_label1 groups row_label2, and turns row_label1 into a stub over its 
#' related groups of row_label2.
#'
#' @param .dat Input data set - should come from a built Tplyr table. 
#'
#' @importFrom dplyr distinct rename bind_rows mutate select arrange across
#' @importFrom dplyr starts_with
#' @importFrom tidyr replace_na
#' 
#' @return data.frame with row labels nested
#' @export 
nest_rowlabels <- function(.dat) {
  stubs <- .dat %>% 
    distinct(row_label1, ord_layer_index) %>% 
    rename(row_label = row_label1) %>% 
    mutate(
      ord_layer_1 = 0, 
      ord_layer_2 = 0
    )
  
  .dat %>% 
    select(-row_label1, row_label=row_label2) %>% 
    bind_rows(stubs) %>% 
    arrange(ord_layer_index, ord_layer_1, ord_layer_2) %>% 
    mutate(
      across(starts_with('var'), ~ tidyr::replace_na(., ''))
    )
}