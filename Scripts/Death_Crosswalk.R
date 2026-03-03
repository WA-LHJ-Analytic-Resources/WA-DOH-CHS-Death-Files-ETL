# Death_Crosswalk.R

# Setup -----

# Install/Load R Packages
pacman::p_load(
  arrow,
  here,
  forcats,
  janitor,
  fs,
  glue,
  tidyverse,
  dplyr,
  lubridate,
  stringr,
  readr,
  rio,
  digest # for stable row_id hashing
)

# Load All Custom functions
list.files(
  path = here::here("Scripts", "Custom_Functions"),
  pattern = "\\.R$",
  full.names = TRUE,
  recursive = TRUE
) %>%
  lapply(source)

# Define Parameters (params)
params <- list()
params$code_sets <- list() # to store 4_Code_Set_Expansion lookup tables
params$root_folder <- Sys.getenv("RAW_DEATH_FILES_FOLDER")

## Data Vintages (to process)
params$years_bedrock <- 2010:2015
params$years_whales <- 2016:2024 # EDIT BASED ON AVAILABLE WHALES DATA IN SECURE ACCESS WASHINGTON

# Crosswalks -----

# fmt: skip
{
  ## Define Crosswalk Filepaths
  params$cw_filepath <- here::here("Resources", "Crosswalks")
  params$codes_filepath <- here::here(params$cw_filepath,"4_Code_Set_Expansion")

  ## Variable Name Crosswalk (1_Schema_Harmonization)
  params$variable_name_cw <- load_crosswalk(filepath = here(params$cw_filepath,"1_Schema_Harmonization","variable_name_crosswalk.csv"))

  ## Variable Code Crosswalk (3_Value_Harmonization)
  params$variable_code_cw <- load_crosswalk(filepath = here(params$cw_filepath,"3_Value_Harmonization","variable_code_crosswalk.csv"))

  ## Code Sets (4_Code_Set_Expansion)
  code_sets <- c("cemetery", "country", "facility", "fips", "funeral_home", "nchs_county", "nchs_state", "occupation_milham", "wa_county", "wa_county_city")

  for(set in code_sets){

    print(glue("Loading code sets for: {set}"))
    params$code_sets[[set]] <- load_crosswalk(filepath = here(params$codes_filepath, paste0(set,"_codes.csv")))
  }
}

rm(code_sets)

# Identify All Death Statistical File Vintages -----

files <- identify_death_files(folder = params$root_folder)

death_stat_files <- files %>%
  filter(
    file_type == "stat",
    file_status == "F",
    file_year %in% c(params$years_bedrock, params$years_whales)
  ) %>%
  mutate(vintage_label = glue("{system}_{file_year}"))

# Harmonize Data Vintages -----

harmonized_output_list <- harmonize_all_vintages(
  files = death_stat_files,
  variable_name_cw = params$variable_name_cw,
  variable_code_cw = params$variable_code_cw,
  keep_labels = FALSE, # TRUE: adds a {var}_label that provides the coded value descriptions; FALSE: {var}_label not created.
  present = "crosswalked", # "crosswalked": only BEDROCK-->WHALES converted codes provided; "both" original BEDROCK and BEDROCK-->WHALES codes provided.
  verbose = TRUE,
  timed = TRUE
)


## Extract Harmomized Data & QA Report
harmonized_data <- purrr::map_dfr(harmonized_output_list, "data") # Pull out and append all harmonized data vintages
qa_unmapped_codes_report <- purrr::map_dfr(harmonized_output_list, "qa") # Pull out and append all QA reports for each data vintage

# Clean Data -----
skimr::skim(harmonized_data)

params$date_vars <- c(
  "date_of_birth",
  "date_of_death",
  "date_of_injury",
  "date_received",
  "disposition_date"
)

harmonized_data_v2 <- harmonized_data %>%
  ## Demographics
  mutate(
    age = as.numeric(age),
    age_years = as.numeric(age_years)
  ) %>%
  combine_code_columns(
    input_cols = c(
      underlying_cod_code,
      matches("^record_axis_code_(?:[2-9]|1[0-9]|20)$")
    ),
    new_var = all_cod_code
  ) %>%
  # Combine ACME Nature of Injury flags
  combine_code_columns(
    input_cols = matches("^acme_nature_of_injury_flag_(?:[1-9]|1[0-9]|20)$"),
    new_var = all_acme_nature_of_injury_flag
  ) %>%
  ## Format Code Variable Data Types (for joins)
  mutate(
    birthplace_country = as.numeric(birthplace_country),
    death_facility = as.numeric(death_facility),
    funeral_home_code = as.numeric(funeral_home_code),
    occupation_milham = as.numeric(occupation_milham),
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
      zero_pad_2
    )
  ) %>%
  # Parse Date Variables
  mutate(ingestion_ts = as_datetime(ingestion_ts)) %>%
  clean_date_vars(df = ., vars = params$date_vars) %>%
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


# Subset & Organize Harmonized Data -----

harmonized_data_v3 <- harmonized_data_v2 %>%
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

# Save Harmonized Data -----

## TBD - Still being developed

## CSV
# Pros: Familiar file format
# Cons: Excel opening limit is ~1 million rows, not optimized for large data
# harmonized_data %>% write_csv(x = ., file = "Data/harmonized_data.csv")

## Parquet
# Pros: Optimized for large data, coding language agnostic, can easily join/paritition data by year
# Cons: A new file format, saves to whole directories, optimized for large data = lazy evaluation (maybe a new paradigm for folks)
# Source: https://r4ds.hadley.nz/arrow.html

# arrow::write_dataset(
#   dataset = harmonized_data,
#   path = here::here("Data/Harmonized_Data"),
#   format = "parquet",
#   partitioning = c("system", "file_year"), # c("system", "file_year")
#   basename_template = "death_statistical_{i}.parquet", # filename pattern
#   hive_style = TRUE, # key=value/ subdirs (year=2020/…)
#   existing_data_behavior = "overwrite" # "error" or "delete_matching"
# )

## DuckDB
# Pros: 1 file...
# Cons: Database connections may be new for folks...
