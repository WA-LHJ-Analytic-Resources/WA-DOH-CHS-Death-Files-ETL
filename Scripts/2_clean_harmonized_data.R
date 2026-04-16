# 2_clean_harmonized_data.R

# Initialize Audit List -----
audits <- list()

## Combine Underlying COD Code & All Record Axis Codes --> 1 Variable
harmonized_data <- combine_code_columns(
  df = harmonized_data,
  input_vars = c(
    underlying_cod_code,
    matches("^record_axis_code_(?:[2-9]|1[0-9]|20)$") # Function also removes record_axis_code_1 (as it is redundant with underlying_cod_code)
  ),
  delimiter = ";",
  output_var = "all_cod_code",
  remove_inputs = FALSE # TRUE = Removes all record_axis_code variables as they have all been condensed into all_cod_code
)

# Convert Harmonized Data to Final Data Types ------

## Clean Date & Time Variables
harmonized_data <- harmonized_data %>%
  clean_date_variables(
    df = .,
    vars = c(
      "date_of_birth",
      "date_of_death",
      "date_of_injury",
      "date_received",
      "disposition_date"
    )
  ) %>%
  clean_time_variables(df = .)


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

  ## Audit how the factor labels were applied
  audits$factor_labels <- attr(harmonized_data, "factor_audit")
}

# WA County Code to FIPS Code Conversion -----

# UNDER DEVELOPMENT #
# County code pair list based on what years we want to convert WA County codes to FIPS County Codes
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
# Ensure that WA County code columns that have values 0 - 9 have a leading 0 in the front

TEST <- harmonized_data %>%
  mutate(across(
    c(residence_county_wa_code, death_county_wa_code, injury_county_wa_code),
    ~ str_pad(
      .,
      width = 2,
      side = "left",
      pad = "0"
    )
  ))

# Using county code pairs to iterate function over the dataframe
TEST <- county_code_pairs %>%
  reduce(
    function(data, pair) {
      county_wa_code_to_fips(
        data,
        pair$wa_col,
        pair$fips_col,
        pair$year_threshold
      )
    },
    .init = TEST
  )
# Switch FIPS codes with value of "00" to NA
TEST <- TEST |>
  mutate(
    across(
      c(death_county_fips, residence_county_fips, injury_county_fips),
      ~ na_if(., "00")
    )
  )

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
    # starts_with("record_axis_code"), # Uncomment if we actually do need all record_axis_code_# variables!
    manner,
    disposition,
    # Injury
    injury_acme_place,
    injury_at_work,
    # Geography
    starts_with("birthplace_country"),
    starts_with("birthplace_state_fips_code"),
    starts_with("residence_city"),
    starts_with("residence_county"),
    starts_with("residence_state_fips_code"),
    residence_zip_code,
    residence_city_limits,
    residence_length,
    residence_length_type,
    starts_with("injury_city"),
    starts_with("injury_county"),
    injury_place,
    injury_state,
    injury_zip_code,
    starts_with("death_city"),
    starts_with("death_county"),
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

# Save Clean Harmonized Data -----

## Parquet File
arrow::write_parquet(
  harmonized_data,
  sink = here(
    Sys.getenv("HARMONIZED_DEATH_FILE_FOLDER"),
    "harmonized_data.parquet"
  )
)
