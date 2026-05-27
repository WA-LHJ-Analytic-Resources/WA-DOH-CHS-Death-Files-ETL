# Convert FIPS codes to Literal Names
county_fips_to_literals <- function(
  data,
  fips_col,
  literal_col,
  year_threshold
) {
  data %>%
    left_join(
      params$code_sets$wa_county_code_to_fips %>%
        select(county_fips_code, county_fips_label),
      by = setNames("county_fips_code", fips_col)
    ) %>%
    mutate(
      !!literal_col := case_when(
        file_year < year_threshold ~ str_to_upper(county_fips_label),
        TRUE ~ .data[[literal_col]]
      )
    ) %>%
    select(-county_fips_label)
}
