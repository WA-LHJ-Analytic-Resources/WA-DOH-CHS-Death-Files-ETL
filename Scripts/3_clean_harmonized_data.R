# 3_clean_harmonized_data.R

# Initialize Audit List -----
audits <- list()

# Clean ICD-10 Codes & Create a Combined Underlying (underlying_cod_code) & Contributing Cause of Death (record_axis_#) Code Variable ----
harmonized_data <- harmonized_data %>%
  # Implement rads ICD-10 clean functions for all COD code variables.
  mutate(
    across(
      .cols = c(underlying_cod_code, matches("^record_axis_code_")),
      .fns = ~ rads::death_icd10_clean(icdcol = .x)
    )
  ) %>%
  # Create Combined COD Code Variable
  combine_code_columns(
    df = .,
    input_vars = c(
      underlying_cod_code,
      matches("^record_axis_code_(?:[2-9]|1[0-9]|20)$") # Function also removes record_axis_code_1 (as it is redundant with underlying_cod_code)
    ),
    delimiter = ";",
    output_var = "all_cod_code",
    remove_inputs = FALSE # TRUE = Removes all record_axis_code variables as they have all been condensed into all_cod_code
  )

# Clean Date & Time Variables ------

## Clean Date Variables
harmonized_data <- clean_date_variables(df = harmonized_data)
audits$date_parse_errors <- attr(harmonized_data, "date_parsing_errors")

## Clean Time Variables
harmonized_data <- clean_time_variables(df = harmonized_data)
audits$time_parse_errors <- attr(harmonized_data, "time_parsing_errors")

# Convert Variables to Final Data Types (Specified by Harmonized Data Schema) -----

## Convert all variables to proper data types
harmonized_data <- clean_data_types(
  df = harmonized_data,
  df_schema = params$harmonized_data_schema
) # If there's a mismatch in variables (in df vs df_schema), a warning message will indicate what variables are differing (and their data types will remain the same)

## Audit how data types were converted
audits$data_type_conversions <- attr(harmonized_data, "data_type_conversions")

# (Optional) Apply Labels to Factor Variables ------
if (params$apply_variable_labels == TRUE) {
  harmonized_data <- apply_variable_labels(
    df = harmonized_data,
    df_schema = params$harmonized_data_schema
  )

  ## Audit how the factor labels were applied
  audits$factor_conversion <- attr(harmonized_data, "factor_audit")
}

# Adjust string variable case -----

harmonized_data <- harmonized_data %>%
  mutate(across(
    .cols = c(occupation, industry, informant_relationship),
    .fns = ~ str_to_title(.x)
  )) # All caps to Title Case

# Reorder Harmonized Data Variables -----

harmonized_data <- harmonized_data %>%
  # Subset & Reorder Columns
  select(
    # Data Vintage Variables
    date_harmonized,
    vintage_label,
    source_system,
    file_year,
    source_file,
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
    time_of_death,
    time_of_injury,
    # Cause of Death
    underlying_cod_code,
    all_cod_code,
    starts_with("record_axis_code"), # Comment out if we do not want all record_axis_code_# variables!
    manner,
    disposition,
    # Injury
    injury_acme_place,
    injury_at_work,
    # Geography
    starts_with("birthplace_country"),
    starts_with("birthplace_state_fips_code"),
    starts_with("residence_city"),
    residence_county,
    residence_county_fips,
    starts_with("residence_state_fips_code"),
    residence_zip_code,
    residence_city_limits,
    residence_length,
    residence_length_type,
    injury_place,
    starts_with("injury_city"),
    injury_county,
    injury_county_fips,
    injury_state,
    injury_zip_code,
    starts_with("death_city"),
    # death_county,
    death_county_fips,
    death_state,
    death_zip_code,
    place_of_death_type,
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
    death_facility,
    informant_relationship,
    autopsy,
    certifier_designation,
    me_coroner_referred
  )

# Implement Variable Completeness Check -----
visualize_completeness(
  df = harmonized_data,
  completeness_threshold = 1, # change completeness_threshold to 0.95 (or other value) to subset to variables with lower incompleteness/higher variability
  plotly = TRUE
)

# Check rads R Package Death Functions Compatability -----
rads::death_validate_data(harmonized_data, check_multicause = TRUE)

# Save Clean Harmonized Data -----

## Save Harmonized Data (as a .parquet file)
arrow::write_parquet(
  harmonized_data,
  sink = here(
    Sys.getenv("HARMONIZED_DEATH_FILE_FOLDER"),
    "harmonized_data.parquet"
  )
)

## Create a Dictionary for the Harmonized Data
data_dictionary <- rads::create_dictionary(
  ph.data = harmonized_data,
  source = "harmonized_data",
  max_unique_values = 30,
  truncation_threshold = 15
)

## Save the Harmonized Data Dictionary
writexl::write_xlsx(
  x = data_dictionary,
  path = here::here(
    "Resources",
    "Data Dictionary.xlsx"
  )
)
