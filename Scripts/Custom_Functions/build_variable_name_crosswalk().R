# build_variable_name_crosswalk().R

## Note: This function takes variable_name_crosswalk.csv, takes the from_name column, splits it by the | delimiter (as there may be multiple slight variations of the from_name)
## and prepares the variable rename crosswalk.

build_variable_name_crosswalk <- function(
  df,
  variable_name_cw,
  year_col = "file_year"
) {
  yr <- unique(df[[year_col]])
  variable_name_cw %>%
    dplyr::filter(start_year <= yr, end_year >= yr) %>%
    tidyr::separate_rows(from_name, sep = "\\|") %>% # explode aliases
    dplyr::filter(from_name %in% names(df)) %>%
    dplyr::group_by(to_name) %>%
    dplyr::summarise(from_name = dplyr::first(from_name), .groups = "drop") %>%
    tibble::deframe()
}
