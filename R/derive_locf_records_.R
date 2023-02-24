#' Derive LOCF (Last Observation Carried Forward) Records
#'
#' Adds LOCF records as new observations for each 'by group' when the dataset
#' does not contain observations for missed visits/time points.
#'
#' @param dataset Input dataset
#'
#'   The columns specified by the `by_vars` and the `order`
#'   parameter are expected.
#'
#' @param dataset_expected_obs Expected observations dataset
#'
#'   Data frame with all the combinations of `PARAMCD`, `PARAM`, `AVISIT`,
#'   `AVISITN`, ... which are expected in the dataset is expected.
#'
#' @param by_vars Grouping variables
#'
#'   For each group defined by `by_vars` those observations from `dataset_expected_obs`
#'   are added to the output dataset which do not have a corresponding observation
#'   in the input dataset or for which `AVAL` is NA for the corresponding observation
#'   in the input dataset. Only variables specified in `by_vars` will be populated
#'   in the newly created records.
#'
#' @param order List of variables for sorting a dataset
#'
#'   The dataset is sorted by `order` before carrying the last
#'   observation forward (eg. `AVAL`) within each `by_vars`.
#'
#' @param keep_vars Variables that need carrying the last observation forward
#'
#'   Keep variables that need carrying the last observation forward other than `AVAL`
#'   (eg. `PARAMN`, `VISITNUM`). If by default `NULL`, only variables specified in
#'   `by_vars` and `AVAL` will be populated in the newly created records.
#'
#' @author G Gayatri
#'
#' @details For each group (with respect to the variables specified for the
#' by_vars parameter) those observations from dataset_expected_obs are added to
#' the output dataset
#' - which do not have a corresponding observation in the input dataset or
#' - for which `AVAL` is NA for the corresponding observation in the input dataset.
#'
#'   For the new observations, `AVAL` is set to the non-missing `AVAL` of the
#'   previous observation in the input dataset (when sorted by `order`) and
#'   `DTYPE` is set to "LOCF".
#'
#' @return The input dataset with the new "LOCF" observations added for each
#' `by_vars`. Note, a variable will only be populated in the new parameter rows
#' if it is specified in `by_vars`.
#'
#' @keywords der_prm_bds_findings
#' @family der_prm_bds_findings
#'
#' @export
derive_locf_records_ <- function(dataset,
                                 dataset_expected_obs,
                                 by_vars,
                                 order,
                                 keep_vars = NULL) {
  #### Input Checking ####

  # Check if input parameters is a valid list of variables
  assert_vars(by_vars, optional = TRUE)
  assert_vars(keep_vars, optional = TRUE)
  assert_order_vars(order)

  # Check by_vars and order variables in input datasets
  assert_data_frame(dataset_expected_obs)
  assert_data_frame(
    dataset,
    required_vars = quo_c(by_vars, extract_vars(order), chr2vars(colnames(dataset_expected_obs)))
  )
  assert_data_frame(
    dataset,
    required_vars = keep_vars,
    optional = TRUE
  )


  #### Prepping 'dataset_expected_obs' ####


  # Get the IDs from input dataset for which the expected observations are to be added

  ids <- dataset %>%
    select(!!!setdiff(by_vars, chr2vars(colnames(dataset_expected_obs)))) %>%
    distinct()

  exp_obsv <- ids %>%
    crossing(dataset_expected_obs)



  ##### Add LOCF records ####

  # Split input dataset into the missing and non-missing AVAL records
  aval_missing <- dataset %>%
    filter(is.na(AVAL))

  aval_not_missing <- dataset %>%
    drop_na(AVAL)


  # Get the variable names from the expected observation dataset
  exp_obs_vars <- exp_obsv %>%
    colnames()


  # Get unique combination of visits/timepoints per parameter per subject
  # from the original input dataset (with non-missing AVAL)
  advs_unique_original <- aval_not_missing %>%
    select(all_of(exp_obs_vars)) %>%
    distinct()


  tmp_dtype <- get_new_tmp_var(exp_obsv, prefix = "tmp_dtype")

  # Get all the expected observations that are to be added to the input
  # dataset (with non-missing AVAL)
  advs_exp_obsv3 <- exp_obsv %>%
    mutate(!!tmp_dtype := "LOCF") %>%
    anti_join(advs_unique_original, by = c(exp_obs_vars))

  # Merge the expected observations with the input dataset (with non-missing AVAL)
  # Arrange the dataset by 'order' and group it by 'by_vars'
  # Use fill() to fill the AVAL from the previous observation for the newly added records


  aval_not_missing_locf <- aval_not_missing %>%
    full_join(advs_exp_obsv3, by = c(exp_obs_vars))

  if ("DTYPE" %in% colnames(aval_not_missing)) {
    aval_not_missing_locf <- aval_not_missing_locf %>%
      mutate(DTYPE = if_else(!!tmp_dtype == "LOCF", "LOCF", DTYPE, missing = DTYPE)) %>%
      select(-!!tmp_dtype)
  } else {
    aval_not_missing_locf <- rename(aval_not_missing_locf, DTYPE = !!tmp_dtype)
  }

  aval_not_missing_locf <- aval_not_missing_locf %>%
    arrange(!!!by_vars, !!!order) %>%
    group_by(!!!by_vars) %>%
    fill("AVAL", !!!keep_vars) %>%
    ungroup()



  # Output dataset - merge the AVAL missing with non-missing+newly added LOCF records
  bind_rows(aval_not_missing_locf, aval_missing)
}
