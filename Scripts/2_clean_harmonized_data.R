# 2_clean_harmonized_data.R

# Initialize Audit List -----
audits <- list()

# Unify Variable Versions -----

## Disposition Facility Codes & Names
harmonized_data <- unify_variables(
  df = harmonized_data,
  vars = "disposition facility",
  code_set = params$code_sets$cemetery
)

## Funeral Home Codes & Names
harmonized_data <- unify_variables(
  df = harmonized_data,
  vars = "funeral home",
  code_set = params$code_sets$funeral_home
)

## Combine Underlying COD Code & All Record Axis Codes --> 1 Variable
harmonized_data <- combine_code_columns(
  df = harmonized_data,
  input_vars = c(
    underlying_cod_code,
    matches("^record_axis_code_(?:[2-9]|1[0-9]|20)$") # Function also removes record_axis_code_1 (as it is redundant with underlying_cod_code)
  ),
  delimiter = ";",
  output_var = "all_cod_code",
  remove_inputs = TRUE # Removes all record_axis_code variables as they have all been condensed into all_cod_code
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
)

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


# Joining Code Sets -----

#### UNDER DEVELOPMENT ####
# NEED TO COLLABORATE WITH PROJECT TEAM ON CODE JOINING/HARMONIZING FIPS/WA CODES

# TEST <- harmonized_data %>%
#   # Ensure single digits are 0 padded for coded variables
#   mutate(
#     across(
#       c(
#         injury_state,
#         death_state,
#         birthplace_state_fips_code,
#         residence_state_fips_code,
#         death_county_wa_code,
#         injury_county_wa_code
#       ),
#       .fns = ~ str_pad(., width = 2, side = "left", pad = "0")
#     )
#   ) %>%
#   # Expand Coded Variables
#   ## Country Codes -----
#   left_join(
#     .,
#     params$code_sets$country %>%
#       by = join_by(birthplace_country == code)
#   ) %>%
#   rename(birthplace_country_label = label) %>%
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
# left_join(
#   .,
#   params$code_sets$wa_county %>%
#     select(code, label),
#   by = join_by(death_county_wa_code == code)
# ) %>%
# rename(death_county_wa_code_label = label) %>%
# left_join(
#   .,
#   params$code_sets$wa_county %>%
#     select(code, label),
#   by = join_by(injury_county_wa_code == code)
# ) %>%
# rename(injury_county_wa_code_label = label) %>%
# left_join(
#   .,
#   params$code_sets$wa_county %>%
#     select(code, label),
#   by = join_by(residence_county_wa_code == code)
# ) %>%
# rename(residence_county_wa_code_label = label) %>%
# ## Death Facility Codes -----
# left_join(
#   .,
#   params$code_sets$facility %>%
#     select(code, label),
#   by = join_by(death_facility == code)
# ) %>%
# rename(death_facility_label = label) %>%
# ## NCHS State Codes -----
# left_join(
#   .,
#   params$code_sets$nchs_state %>%
#     select(code, label),
#   by = join_by(death_state == code)
# ) %>%
# rename(death_state_label = label) %>%
# left_join(
#   .,
#   params$code_sets$nchs_state %>%
#     select(code, label),
#   by = join_by(injury_state == code)
# ) %>%
# rename(injury_state_label = label) %>%
# left_join(
#   .,
#   params$code_sets$nchs_state %>%
#     select(code, label),
#   by = join_by(birthplace_state_fips_code == code)
# ) %>%
# rename(birthplace_state_fips_code_label = label) %>%
# left_join(
#   .,
#   params$code_sets$nchs_state %>%
#     select(code, label),
#   by = join_by(residence_state_fips_code == code)
# ) %>%
# rename(residence_state_fips_code_label = label)

# Subset & Organize Cleaned Harmonized Data -----

harmonized_data <- harmonized_data %>%
  # Subset & Reorder Columns
  select(
    # Data Vintage Variables
    date_harmonized,
    source_system,
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
    time_of_death,
    time_of_injury,
    # Cause of Death
    # underlying_cod_code,
    all_cod_code,
    manner,
    disposition,
    # Injury
    injury_acme_place,
    injury_at_work,
    # Geography
    starts_with("birthplace_country"),
    starts_with("birthplace_state_fips_code"),
    starts_with("residence_county"),
    starts_with("residence_state_fips_code"),
    residence_zip_code,
    residence_city_limits,
    residence_length,
    residence_length_type,
    starts_with("injury_county"),
    injury_place,
    injury_state,
    injury_zip_code,
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
    funeral_home_name,
    disposition_facility_name,
    informant_relationship,
    autopsy,
    certifier_designation,
    me_coroner_referred
  )

# Save Clean Harmonized Data -----
saveRDS(
  harmonized_data,
  file = here(Sys.getenv("HARMONIZED_DEATH_FILE_FOLDER"), "harmonized_data.rds")
)
