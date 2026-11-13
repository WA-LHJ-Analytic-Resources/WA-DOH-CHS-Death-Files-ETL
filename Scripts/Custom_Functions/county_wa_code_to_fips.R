#' Fill FIPS county codes from WA county codes for certain file years
#'
#' Looks up each value in `wa_col` against the WA county code to FIPS crosswalk
#' (`params$code_sets$wa_county_code_to_fips`, loaded from CSV) and writes the
#' matching FIPS code into `fips_col`, but only for rows where `file_year` is
#' earlier than `year_threshold`. Rows from `year_threshold` onward keep their
#' existing `fips_col` value.
#'
#' Handles one WA/FIPS column pair per call. It is applied to each pair listed
#' in `params$code_sets$wa_fips_column_pairs` via `purrr::reduce()`.
#'
#' @section Errors:
#' Stops with an error if:
#' * `file_year`, `wa_col`, or `fips_col` is missing from `data`
#' * the join changes the number of rows
#' * any row with `file_year < year_threshold` has a non-missing `wa_col` value
#'   that is not found in the crosswalk
#'
#' @param data A data frame with a `file_year` column and both `wa_col` and
#'   `fips_col` columns. `fips_col` must already exist.
#' @param wa_col String. Name of the column containing WA county codes.
#' @param fips_col String. Name of the column to fill with FIPS county codes.
#' @param year_threshold Numeric. Rows with `file_year` below this value get
#'   their FIPS code from the crosswalk.
#'
#' @return `data` with `fips_col` populated from the crosswalk for rows before
#'   `year_threshold`. No columns are added or removed.
county_wa_code_to_fips <- function(data, wa_col, fips_col, year_threshold) {
  crosswalk <- params$code_sets$wa_county_code_to_fips

  # Input checks
  missing_cols <- setdiff(c("file_year", wa_col, fips_col), names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(glue::glue(
      "county_wa_code_to_fips(): `data` is missing column(s): {paste(missing_cols, collapse = ', ')}"
    ))
  }

  # Row count before join
  n_before <- nrow(data)

  # Left join
  result <- data %>%
    left_join(
      crosswalk %>% select(county_wa_code, county_fips_code),
      by = setNames("county_wa_code", wa_col),
      relationship = "many-to-one"
    )

  # Row count check after join
  if (nrow(result) != n_before) {
    rlang::abort(glue::glue(
      "county_wa_code_to_fips(): join changed row count for {wa_col} from {n_before} to {nrow(result)}."
    ))
  }

  # Unmatched codes: rows that should convert but found no crosswalk match
  unmatched <- result %>%
    filter(
      file_year < year_threshold,
      !is.na(.data[[wa_col]]),
      is.na(county_fips_code)
    )

  if (nrow(unmatched) > 0) {
    rlang::abort(glue::glue(
      "county_wa_code_to_fips(): {nrow(unmatched)} row(s) in {wa_col} have no FIPS match. ",
      "Unmatched codes: {paste(unique(unmatched[[wa_col]]), collapse = ', ')}"
    ))
  }

  # Fill in user specified fips column
  result %>%
    mutate(
      !!fips_col := case_when(
        file_year < year_threshold ~ county_fips_code,
        TRUE ~ .data[[fips_col]]
      )
    ) %>%
    select(-county_fips_code)
}
