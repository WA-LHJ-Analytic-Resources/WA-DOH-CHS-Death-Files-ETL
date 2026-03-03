# helpers.R

# build_variable_name_crosswalk() ------
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


# collect_unmapped() -----

## Note: Used in harmonize_all_vintages(); Collects an extract of unmapped codes (from BEDROCK --> WHALES) that was not captured in current
## 3-Schema Harmonization crosswalks.

collect_unmapped <- function(df_values, file_row) {
  qa_unmapped <- attr(df_values, "qa_unmapped")
  if (!is.null(qa_unmapped) && nrow(qa_unmapped) > 0) {
    qa_unmapped %>%
      mutate(
        vintage_label = file_row$vintage_label,
        source_file = file_row$file_location,
        system = file_row$system,
        file_year = file_row$file_year
      )
  } else {
    tibble()
  }
}


# load_crosswalk() -----

load_crosswalk <- function(filepath) {
  cw <- readr::read_csv(file = filepath, show_col_types = FALSE)
}
