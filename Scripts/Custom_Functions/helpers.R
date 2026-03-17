# helpers.R

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

# zero_pad_2() -----

zero_pad_2_across <- function() {
  ~ sql(
    paste0(
      "lpad(NULLIF(trim(",
      cur_column(),
      "), ''), 2, '0')"
    )
  )
}
