# 2_clean_harmonized_data.R

# Load Harmonized Data -----
load(file = "harmonized_data.RData")

# Convert Harmonized Data to Final Data Types ------

## Load in Final Harmonized Data Schema
schema_data_types <- readr::read_csv(
  file = here("Resources", "Schemas", "schema_data_types.csv"),
  show_col_types = FALSE
) %>%
  select(-notes, -flag)

## Implement Data Type Conversions
harmonized_data <- clean_data_types(
  df = harmonized_data,
  df_schema = schema_data_types
)

## (Optional) Review how data types were converted
# data_type_conversion_audit <- attr(harmonized_data, "schema_audit")

## Apply Labels to Factor Variables ------
if (params$apply_variable_labels == TRUE) {
  ## Load in DF Factor Schema
  schema_factors = readr::read_csv(
    file = here("Resources", "Schemas", "schema_factors.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(order = as.integer(order)) %>%
    select(variable, level, label, order, ordered, data_type)

  harmonized_data <- apply_variable_labels(
    df = harmonized_data,
    dict_df = schema_factors
  )
}


# Clean Data -----
harmonized_data_clean <- harmonized_data %>%

  mutate(
    across(
      c(
        injury_state,
        death_state,
        birthplace_state_fips_code,
        residence_state_fips_code,
        death_county_wa_code,
        injury_county_wa_code
      ),
      zero_pad_2_across()
    )
  )


# Peform Joins (4_Code_Set_Expansion) -----

harmonized_data_clean <- harmonized_data_clean %>%
  # Expand Coded Variables
  ## Country Codes -----
  left_join(
    .,
    params$code_sets$country %>%
      select(code, label),
    by = join_by(birthplace_country == code)
  ) %>%
  rename(birthplace_country_label = label) %>%
  ## WA County-City Codes: REVIEW -- GET ASSISTANCE WITH WA COUNTY-CITY CODES -----
  # left_join(
  #   .,
  #   params$code_sets$wa_county_city %>%
  #     select(code, label),
  #   by = join_by(death_county_city_wa_code == code)
  # ) %>%
  # rename(death_county_city_wa_code_label = label) %>%
  # left_join(
  #   .,
  #   params$code_sets$wa_county_city %>%
  #     select(code, label),
  #   by = join_by(injury_county_city_wa_code == code)
  # ) %>%
  # rename(injury_county_city_wa_code_label = label) %>%
  # left_join(
  #   .,
  #   params$code_sets$wa_county_city %>%
  #     select(code, label),
  #   by = join_by(residence_county_city_wa_code == code)
  # ) %>%
  ## WA County Codes -----
  left_join(
    .,
    params$code_sets$wa_county %>%
      select(code, label),
    by = join_by(death_county_wa_code == code)
  ) %>%
  rename(death_county_wa_code_label = label) %>%
  left_join(
    .,
    params$code_sets$wa_county %>%
      select(code, label),
    by = join_by(injury_county_wa_code == code)
  ) %>%
  rename(injury_county_wa_code_label = label) %>%
  left_join(
    .,
    params$code_sets$wa_county %>%
      select(code, label),
    by = join_by(residence_county_wa_code == code)
  ) %>%
  rename(residence_county_wa_code_label = label) %>%
  ## Death Facility Codes -----
  left_join(
    .,
    params$code_sets$facility %>%
      select(code, label),
    by = join_by(death_facility == code)
  ) %>%
  rename(death_facility_label = label) %>%
  ## NCHS State Codes -----
  left_join(
    .,
    params$code_sets$nchs_state %>%
      select(code, label),
    by = join_by(death_state == code)
  ) %>%
  rename(death_state_label = label) %>%
  left_join(
    .,
    params$code_sets$nchs_state %>%
      select(code, label),
    by = join_by(injury_state == code)
  ) %>%
  rename(injury_state_label = label) %>%
  left_join(
    .,
    params$code_sets$nchs_state %>%
      select(code, label),
    by = join_by(birthplace_state_fips_code == code)
  ) %>%
  rename(birthplace_state_fips_code_label = label) %>%
  left_join(
    .,
    params$code_sets$nchs_state %>%
      select(code, label),
    by = join_by(residence_state_fips_code == code)
  ) %>%
  rename(residence_state_fips_code_label = label) %>%
  ## Funeral Home Codes -----
  left_join(
    .,
    params$code_sets$funeral_home %>%
      select(code, label),
    by = join_by(funeral_home_code == code)
  ) %>%
  rename(funeral_home_label = label) %>%
  ## Occupation - Milham Codes -----
  left_join(
    .,
    params$code_sets$occupation_milham %>%
      select(code, label),
    by = join_by(occupation_milham == code)
  ) %>%
  rename(occupation_milham_label = label)


# Subset & Organize Cleaned Harmonized Data -----

harmonized_data_final <- harmonized_data_clean %>%
  # Subset & Reorder Columns
  select(
    # Data Vintage Variables
    ingestion_ts,
    system,
    file_year,
    # Unique Identifiers
    state_file_number,
    local_file_number,
    # Dates & Times
    date_of_birth,
    date_of_injury,
    date_of_death,
    date_of_death_modifier,
    date_received,
    disposition_date,
    starts_with("time_of"),
    # Cause of Death
    underlying_cod_code,
    all_cod_code,
    manner,
    # Injury
    all_acme_nature_of_injury_flag,
    injury_acme_place,
    injury_at_work,
    injury_transportation,
    # Geography
    starts_with("birthplace_country"),
    starts_with("birthplace_state_fips_code"),
    starts_with("residence_county"),
    starts_with("residence_state_fips_code"),
    residence_zip_code,
    residence_length,
    starts_with("injury_county"),
    injury_place,
    injury_state,
    injury_zip_code,
    starts_with("death_county"),
    death_state,
    death_zip_code,
    # Demographics
    starts_with("age"),
    sex,
    marital_status,
    pregnancy,
    starts_with("education"),
    armed_forces,
    industry,
    starts_with("occupation"),
    tobacco,
    starts_with("hispanic"),
    contains("race"),
    # Operations
    starts_with("funeral_home"),
    disposition_facility_code,
    informant_relationship,
    starts_with("autopsy"),
    certifier_designation,
    me_coroner_referred
  )
# Save Clean Harmonized Data -----

## Save Cleaned Harmonized Data to Table
dbWriteTable(
  con,
  "harmonized_data_clean",
  harmonized_data_final,
  overwrite = TRUE
)

## Remove Raw Harmonized Table
# dbRemoveTable(con, "harmonized_data_raw")

## Disconnect from DuckDB
dbDisconnect(con) # Close database connection after finishing run all of R script

# Clean Up -----
# rm(harmonized_data_clean, harmonized_data_final)
