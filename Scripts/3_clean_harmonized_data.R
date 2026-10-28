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
  # Create all_cod_code Variable (underlying_cod_code + all record_axis_code_(1-20))
  combine_code_columns(
    df = .,
    input_vars = c(
      underlying_cod_code,
      matches("^record_axis_code_(?:[1-9]|1[0-9]|20)$")
    ),
    delimiter = ";",
    output_var = "all_cod_code"
  ) %>%
  # Create all_contributing_cod_code Variable (record_axis_code_(2-20))
  combine_code_columns(
    df = .,
    input_vars = c(
      matches("^record_axis_code_(?:[2-9]|1[0-9]|20)$") #
    ),
    delimiter = ";",
    output_var = "all_contributing_cod_code"
  )

# Create ICD-10 Condition Flags -----
harmonized_data <- harmonized_data %>%
  create_condition_flags(df = .)

# Clean Age Variables & Create Standard Age Bins -------
harmonized_data <- harmonized_data %>%
  # Clean age variable
  mutate(age = if_else(age_type == 9 & age == 999, NA, age)) %>% # Convert age to NA (if age_type is 9/Unknown and age is 999)
  # Clean age_years
  mutate(
    age_years = as.integer(age_years), # to integer
    age_years = if_else(age_years > 120, NA, age_years) # age_years over 120 to NA
  ) %>%
  # Create Age Year Bins (age_6g, age_11g, age_20g, age_5yr)
  mutate(
    age_6g = rads::bin_age(age_years, cuts = params$age_6g_levels),
    age_11g = rads::bin_age(
      age_years,
      cuts = params$age_11g_levels
    ),
    age_20g = rads::bin_age(
      age_years,
      cuts = params$age_20g_levels
    ),
    age_5yr = rads::bin_age(
      age_years,
      cuts = params$age_5yr_levels
    )
  )

# Create Race-Ethnicity Groupings -------
harmonized_data <- harmonized_data %>%
  # Create hispanic
  mutate(
    hispanic = case_when(
      hispanic_nchs_bridge == 0 ~ 0,
      hispanic_nchs_bridge %in% c(1:5) ~ 1,
      TRUE ~ NA
    )
  ) %>%
  # Create race_ethnicity2
  mutate(
    race_ethnicity2 = case_when(
      hispanic == 0 ~ "Not Hispanic",
      hispanic == 1 ~ "Hispanic",
      TRUE ~ NA
    )
  ) %>%
  # Create race6
  mutate(
    race6 = case_when(
      race_summary_code == 10 ~ "White Only",
      race_summary_code == 11 ~ "Black Only",
      race_summary_code == 12 ~ "American Indian/Alaska Native Only",
      race_summary_code == 13 ~ "Asian Only",
      race_summary_code == 14 ~ "Native Hawaiian and Pacific Islander Only",
      race_summary_code %in% c(99, NA) ~ NA,
      TRUE ~ "Multi Race"
    )
  ) %>%
  # Create race_ethnicity7
  mutate(
    race_ethnicity7 = case_when(
      ## Step 1: Classify by Hispanic/Latine Identity
      hispanic == 1 ~ "Hispanic as Race",
      ## Step 2: Classify by race6
      hispanic %in% c(0, NA) & race6 == "White Only" ~ "White Only-NH",
      hispanic %in% c(0, NA) & race6 == "Black Only" ~ "Black Only-NH",
      hispanic %in%
        c(0, NA) &
        race6 ==
          "American Indian/Alaska Native Only" ~ "American Indian/Alaska Native Only-NH",
      hispanic %in% c(0, NA) & race6 == "Asian Only" ~ "Asian Only-NH",
      hispanic %in%
        c(0, NA) &
        race6 ==
          "Native Hawaiian and Pacific Islander Only" ~ "Native Hawaiian and Pacific Islander Only-NH",
      hispanic %in% c(0, NA) & race6 == "Multi Race" ~ "Multi-Race-NH",
      TRUE ~ NA
    )
  ) %>%
  # Create race_asian & race_nhpi (based on more-specific subgroupings) %>%
  mutate(
    race_asian = if_else(
      if_any(
        c(
          race_asian_indian,
          race_chinese,
          race_filipino,
          race_japanese,
          race_korean,
          race_other_asian,
          race_vietnamese
        ),
        ~ .x == "Y"
      ),
      "Y",
      "N"
    ),
    race_nhpi = if_else(
      if_any(
        c(
          race_hawaiian,
          race_samoan,
          race_guamanian_or_chamorro,
          race_other_pacific_islander
        ),
        ~ .x == "Y"
      ),
      "Y",
      "N"
    )
  ) %>%
  # Create race_code (5-digit SADE Race Codes for AOIC: White|Black|AIAN|Asian|NHPI)
  mutate(
    race_code = str_c(
      if_else(race_white == "Y", "1", "0"),
      if_else(race_black == "Y", "1", "0"),
      if_else(race_amer_indian_alaskan == "Y", "1", "0"),
      if_else(race_asian == "Y", "1", "0"),
      if_else(race_nhpi == "Y", "1", "0")
    )
  )

# Clean Date & Time Variables ------

## Clean Date Variables
harmonized_data <- clean_date_variables(df = harmonized_data)
audits$date_parse_errors <- attr(harmonized_data, "date_parsing_errors")

## Clean Time Variables
harmonized_data <- clean_time_variables(df = harmonized_data)
audits$time_parse_errors <- attr(harmonized_data, "time_parsing_errors")

# Convert Variables to Final Data Types (Specified by Harmonized Data Schema) & Apply Factor Labels -----

## Convert all variables to proper data types
harmonized_data <- clean_data_types(
  df = harmonized_data,
  df_schema = params$harmonized_data_schema
) # If there's a mismatch in variables (in df vs df_schema), a warning message will indicate what variables are differing (and their data types will remain the same)

## Audit how data types were converted
audits$data_type_conversions <- attr(harmonized_data, "data_type_conversions")

# Final Cleaning of Variables (Predominantly Strings) -----

harmonized_data <- harmonized_data %>%
  ## Convert All Caps String Variables to Title Case
  mutate(across(
    .cols = c(
      occupation,
      industry,
      informant_relationship,
      contains("cod_text"),
      contains("name"),
      injury_description
    ),
    .fns = ~ str_to_title(.x)
  )) %>%
  ## Ensure Zip Codes follow 5-digit formatting
  mutate(across(
    .cols = c(death_zip_code, injury_zip_code, residence_zip_code),
    .fns = ~ if_else(str_detect(.x, "^[0-9]{5}$"), .x, NA) # if ZIP is not following 5 digit format, convert to NA
  )) %>%
  ## Convert Unknown Placeholder Values to NA (Note: This may not capture all placeholder values, but aiming to convert the most frequently occurring ones)
  mutate(
    across(
      .cols = c(industry, occupation),
      .fns = ~ case_when(
        .x %in% c("-", "--", "---", "------", ".", "?") ~ NA_character_,
        str_detect(
          .x,
          regex("Not Applicable", ignore_case = TRUE)
        ) ~ NA_character_,
        str_detect(.x, regex("Unknown", ignore_case = TRUE)) ~ NA_character_,
        TRUE ~ .x
      )
    ),
    injury_place = case_when(
      injury_place %in% c("-", "?") ~ NA,
      injury_place %in% c("NONE", "UNKNOWN", "NOT APPLICABLE") ~ NA,
      TRUE ~ injury_place
    ),
    residence_length = if_else(residence_length == "999", NA, residence_length) # Could be possible based on residence_length_type but unlikely
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
    # Unique Identifiers
    state_file_number,
    local_file_number,
    contains("name_"), # Names
    social_security_number,
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
    contains("cod_text"), # literal narrative
    manner,
    disposition,
    # Injury
    injury_acme_place,
    injury_at_work,
    injury_description, # literal
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
    contains("age"),
    sex,
    marital_status,
    pregnancy,
    starts_with("education"),
    armed_forces,
    industry,
    starts_with("occupation"),
    tobacco,
    contains("hispanic"),
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
var_descriptions <- params$harmonized_data_schema %>%
  mutate(source = "WA DOH CHS Death Certificate Files") %>%
  distinct(source, varname = variable, desc = description)


data_dictionary <- rads::create_dictionary(
  ph.data = harmonized_data,
  ph.ref = var_descriptions,
  source = "WA DOH CHS Death Certificate Files",
  suppress = c(
    "social_security_number",
    "name_first",
    "name_middle",
    "name_last",
    "name_suffix",
    "cod_text_1",
    "cod_text_2",
    "cod_text_3",
    "cod_text_4",
    "cod_text_other",
    "injury_description"
  ),
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
