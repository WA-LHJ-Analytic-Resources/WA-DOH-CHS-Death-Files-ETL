# 2_clean_harmonized_data.R

# Load Raw Harmonized Data -----

## Connect to DuckDB database
con <- dbConnect(duckdb::duckdb(), dbdir = params$duckdb_filepath)

# Clean Data -----

harmonized_data_clean <- tbl(con, "harmonized_data_raw") %>%
  ## Demographics
  mutate(
    age = as.numeric(age),
    age_years = as.numeric(age_years)
  ) %>%
  # Combine Cause of Death variables
  combine_code_columns(
    input_vars = c(
      underlying_cod_code,
      matches("^record_axis_code_(?:[2-9]|1[0-9]|20)$")
    ),
    output_var = "all_cod_code" # underyling_cod_code;record_axis_code_2;...;record_axis_code_20
  ) %>%
  ## Format Code Variable Data Types (for joins)
  mutate(
    birthplace_country = as.numeric(birthplace_country),
    death_facility = as.numeric(death_facility),
    funeral_home_code = as.numeric(funeral_home_code),
    occupation_milham = as.numeric(occupation_milham)
  ) %>%
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
  ) %>%
  # Pull the data into RAM to allow for additional cleaning (not SQL compatible) & joins
  collect() %>%
  # Parse Date Variables
  mutate(ingestion_ts = as_datetime(ingestion_ts)) %>%
  clean_date_vars(df = ., vars = params$date_vars)


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
