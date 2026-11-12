#' Fill county name literals from FIPS county codes for file years during bedrock system (2010 - 2015)
#'
#' Looks up each value in `fips_col` against the county FIPS crosswalk
#' (`params$code_sets$wa_county_code_to_fips`, loaded from CSV) and writes the
#' matching county name, upper-cased, into `literal_col`, but only for rows
#' where `file_year` is earlier than `year_threshold`. Rows from
#' `year_threshold` onward keep their existing `literal_col` value.
#'
#' Handles one FIPS/literal column pair per call. It is applied to each pair
#' listed in `params$code_sets$wa_fips_literal_pair` via `purrr::reduce()`.
#' It is meant to run after [county_wa_code_to_fips()] has populated
#' `fips_col`.
#'
#' @section Errors:
#' Stops with an error if:
#' * `file_year`, `fips_col`, or `literal_col` is missing from `data`
#' * the join changes the number of rows
#' * any row with `file_year < year_threshold` has a non-missing `fips_col`
#'   value that is not found in the crosswalk (or has no label there)
#'
#' @param data A data frame with a `file_year` column and both `fips_col` and
#'   `literal_col` columns. `literal_col` must already exist.
#' @param fips_col String. Name of the column containing county FIPS codes.
#' @param literal_col String. Name of the column to fill with county names.
#' @param year_threshold Numeric. Rows with `file_year` below this value get
#'   their county name from the crosswalk.
#'
#' @return `data` with `literal_col` populated from the crosswalk for rows
#'   before `year_threshold`. No columns are added or removed.
county_fips_to_literals <- function(
  data,
  fips_col,
  literal_col,
  year_threshold
) {
  crosswalk <- params$code_sets$wa_county_code_to_fips

  # Input checks
  missing_cols <- setdiff(c("file_year", fips_col, literal_col), names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(glue::glue(
      "county_fips_to_literals(): `data` is missing column(s): {paste(missing_cols, collapse = ', ')}"
    ))
  }

  # Record row count before join
  n_before <- nrow(data)

  # Left join
  result <- data %>%
    left_join(
      crosswalk %>% select(county_fips_code, county_fips_label),
      by = setNames("county_fips_code", fips_col),
      relationship = "many-to-one"
    )

  # Row count check after join
  if (nrow(result) != n_before) {
    rlang::abort(glue::glue(
      "county_fips_to_literals(): join changed row count for {fips_col} from {n_before} to {nrow(result)}."
    ))
  }

  # Unmatched codes: rows that should convert but found no crosswalk label
  unmatched <- result %>%
    filter(
      file_year < year_threshold,
      !is.na(.data[[fips_col]]),
      is.na(county_fips_label)
    )

  if (nrow(unmatched) > 0) {
    rlang::abort(glue::glue(
      "county_fips_to_literals(): {nrow(unmatched)} row(s) in {fips_col} have no county label match. ",
      "Unmatched codes: {paste(unique(unmatched[[fips_col]]), collapse = ', ')}"
    ))
  }

  # Fill in user specified literal column
  result %>%
    mutate(
      !!literal_col := case_when(
        file_year < year_threshold ~ str_to_upper(county_fips_label),
        TRUE ~ .data[[literal_col]]
      )
    ) %>%
    select(-county_fips_label)
}
