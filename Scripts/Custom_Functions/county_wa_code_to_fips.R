# Convert WA Codes to FIPS codes

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
