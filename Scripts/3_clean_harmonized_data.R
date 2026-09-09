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

# Convert Harmonized Data to Final Data Types ------

## Clean Date Variables
cleaned_date_output <- clean_date_variables(df = harmonized_data)
harmonized_data <- cleaned_date_output$df_clean
# audits$date_parse_errors <- cleaned_date_output$parsing_errors # Contains all unique examples of when date string parsing failed (i.e. values in final harmonized data set are NA)

## Clean Time Variables
cleaned_time_output <- clean_time_variables(df = harmonized_data)
harmonized_data <- cleaned_time_output$df_clean
# audits$time_parse_errors <- cleaned_time_output$parsing_errors # Contains all unique examples of when time string parsing failed (i.e. values in final harmonized data set are NA)

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
) # If there's a mismatch in variables (df vs df_schema have more), a warning message will indicate what variables are differing (and their data types will remain the same)

## Audit how data types were converted
audits$data_type_conversions <- attr(harmonized_data, "schema_audit")

# (Optional) Apply Labels to Factor Variables ------
if (params$apply_variable_labels == TRUE) {
  ## Load in DF Factor Schema
  schema_factors <- readr::read_csv(
    file = here("Resources", "Schemas", "schema_factors.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(order = as.integer(order)) %>%
    select(variable, level, label, order, ordered, data_type)

  harmonized_data <- apply_variable_labels(
    df = harmonized_data,
    dict_df = schema_factors
  )

  ## Audit how the factor labels were applied
  audits$factor_labels <- attr(harmonized_data, "factor_audit")
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

## Parquet File
arrow::write_parquet(
  harmonized_data,
  sink = here(
    Sys.getenv("HARMONIZED_DEATH_FILE_FOLDER"),
    "harmonized_data.parquet"
  )
)

## Create & Write Data Dictionary - Harmonized Data
data_dictionary <- create_data_dictionary(
  df = harmonized_data,
  vars_no_val = c("source_file"), # Dont show example values for these provided variable names.
  vars_no_val_limit = 30 # Only show example values for variables with <= 30 distinct values (avoids unique IDs/high cardinal vars)
)

writexl::write_xlsx(
  x = data_dictionary,
  path = here::here(
    "Resources",
    "Schemas",
    "Data Dictionary - Harmonized Data.xlsx"
  )
)


# Clean up -----

rm(
  cleaned_date_output,
  cleaned_time_output
)
