#' Convert Washington County Codes to FIPS Codes
#'
#' Replaces county FIPS codes using a lookup table based on Washington county
#' codes for records before a specified year threshold. This function is useful
#' when historical datasets contain Washington-specific county codes rather than
#' standard county FIPS codes.
#'
#' The function joins the input data to the internal
#' `wa_county_code_to_fips` crosswalk contained in
#' `params$code_sets$wa_county_code_to_fips`, then conditionally updates the
#' specified FIPS column when `file_year` is less than `year_threshold`.
#'
#' A `many-to-one` join relationship is enforced to guard against accidental
#' duplication of rows if duplicate Washington county codes are introduced into
#' the lookup table.
#'
#' @param data A data frame or tibble containing Washington county codes,
#'   a FIPS code column, and a `file_year` variable.
#' @param wa_col A character string specifying the column in `data` containing
#'   Washington county codes used to join to the lookup table.
#' @param fips_col A character string specifying the column in `data` that
#'   contains county FIPS codes and will be conditionally updated.
#' @param year_threshold A numeric or integer year. For records where
#'   `file_year < year_threshold`, the FIPS code will be replaced using the
#'   Washington county code crosswalk.
#'
#' @return
#' A data frame of the same class and number of rows as `data`, with the
#' specified FIPS column updated for records before `year_threshold`.
#'
#' @details
#' The function expects a lookup table named
#' `params$code_sets$wa_county_code_to_fips` containing the variables:
#' \describe{
#'   \item{county_wa_code}{Washington county code}
#'   \item{county_fips_code}{Corresponding county FIPS code}
#' }
#'
#' The input dataset must contain a variable named `file_year`, which is used
#' to determine whether the FIPS code should be updated.
#'
#' @export

county_wa_code_to_fips <- function(data, wa_col, fips_col, year_threshold) {
  data %>%
    left_join(
      params$code_sets$wa_county_code_to_fips %>%
        select(county_wa_code, county_fips_code),
      by = setNames("county_wa_code", wa_col),
      relationship = "many-to-one" #safe guard against duplicate key being added to wa_county_code_to_fips as row duplication would occur
    ) %>%
    mutate(
      !!fips_col := case_when(
        file_year < year_threshold ~ county_fips_code,
        TRUE ~ .data[[fips_col]]
      )
    ) %>%
    select(-county_fips_code)
}
