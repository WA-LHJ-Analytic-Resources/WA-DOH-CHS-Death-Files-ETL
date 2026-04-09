# Convert WA Codes to FIPS codes

county_code_pairs <- list(
  list(
    wa_col = "death_county_wa_code",
    fips_col = "death_county_fips",
    year_threshold = 2022
  ),
  list(
    wa_col = "residence_county_wa_code",
    fips_col = "residence_county_fips",
    year_threshold = 2016
  ),
  list(
    wa_col = "injury_county_wa_code",
    fips_col = "injury_county_fips",
    year_threshold = 2022
  )
)

county_wa_code_to_fips <- function(data, wa_col, fips_col, year_threshold) {
  data %>%
    left_join(
      params$code_sets$wa_county_code_to_fips %>%
        select(county_wa_code, county_fips_code),
      by = c(wa_col = "county_wa_code")
    ) %>%
    mutate(
      !!fips_col := case_when(
        file_year < year_threshold ~ county_fips_code,
        TRUE ~ .data[[fips_col]]
      )
    ) %>%
    select(-county_fips_code)
}
