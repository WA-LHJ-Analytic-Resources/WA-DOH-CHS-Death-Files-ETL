# 1_harmonize_death_files

# Identify All Death Statistical File Vintages -----
death_files <- identify_death_files(folder = params$raw_data_folder)

death_stat_files <- death_files %>%
  # Filter to Finalized Death Statistical Files
  filter(
    file_type == "Stat",
    file_ext == "csv", # avoid including .xlsx or other documents (PDFs)
    file_status == "F" # Filter data vintages only (for now)
  ) %>%
  # Add Vintage Label tag
  mutate(vintage_label = glue("{system}_{file_year}")) %>%
  # Move Vintage Label to 1st position
  relocate(vintage_label, .before = everything())


# Harmonization Process ----

tictoc::tic("Harmonize all death data vintages")

## Step 0: Initiate Data Storage Lists
raw_list <- list()
clean_list <- list()

## Load & Recode Each Data Vintage
for (file_yr in death_stat_files$file_year) {
  tictoc::tic((glue("Processing the data vintage for: {file_yr}"))) # Start data vintage level timer

  ### Step 1a: Load in Variable Rename Crosswalk
  var_rename_crosswalk <- load_crosswalk(file_year = file_yr, type = "rename")

  ### Step 1b: Identify Available & Missing Variables in the Data Vintage
  available_vars <- var_rename_crosswalk %>%
    filter(missing == FALSE) %>%
    pull(from_name)
  missing_vars <- var_rename_crosswalk %>%
    filter(missing == TRUE) %>%
    pull(from_name)

  ### Step 2: Load in Variable Recode Crosswalk
  var_recode_crosswalk <- load_crosswalk(file_year = file_yr, type = "recode")

  ### Step 3a: Identify death statistical file vintage to be loaded
  data_vintage <- death_stat_files %>% filter(file_year == file_yr)

  ### Step 3b: Extact vintage metadata
  provenance <- tibble(
    vintage_label = data_vintage$vintage_label,
    file_year = data_vintage$file_year,
    source_file = data_vintage$file_location,
    source_system = data_vintage$system,
    date_harmonized = as.character(lubridate::today())
  )

  ### Step 4: Load in Data Vintage (Perform 1-Data Type Harmonization
  raw_list[[as.character(file_yr)]] <- load_data_vintage(
    file_location = data_vintage$file_location,
    available_vars = available_vars,
    missing_vars = missing_vars
  )

  ### Step 5: Rename Variables (Perform 2-Schema Harmonization)
  raw_list[[as.character(file_yr)]] <- rename_variables(
    df = raw_list[[as.character(file_yr)]],
    var_rename_cw = var_rename_crosswalk
  )

  ### Step 6: Recode Coded Values (Perform 3-Value Harmonization)
  clean_list[[as.character(file_yr)]] <- recode_variables(
    df = raw_list[[as.character(file_yr)]],
    var_recode_cw = var_recode_crosswalk,
    verbose = FALSE,
    timed = FALSE
  )

  ### Step 7: Add Vintage Metadata
  clean_list[[as.character(file_yr)]] <- clean_list[[as.character(file_yr)]] %>%
    bind_cols(provenance) %>%
    dplyr::relocate(
      vintage_label,
      source_file,
      date_harmonized,
      source_system,
      file_year,
      .before = everything()
    ) # Move these variables to the front.

  tictoc::toc() # End data vintage-level timer
}

## Step 7: Append all data vintages together
harmonized_data <- bind_rows(clean_list, .id = "file_year")

tictoc::toc()

# Convert Date & Time Variables to Proper Data Types -----

harmonized_data <- harmonized_data %>%
  clean_date_variables(df = ., vars = params$date_vars) %>%
  clean_time_variables(df = .)

# Unify Disposition Facility Variables -----

disposition_facility_codes <- params$code_sets$cemetery %>%
  distinct(code, .keep_all = TRUE) %>%
  mutate(
    disposition_facility_code = as.integer(code),
    label = str_to_upper(label)
  ) %>%
  select(disposition_facility_code, label)

TEST <- harmonized_data %>%
  mutate(disposition_facility_code = as.integer(disposition_facility_code)) %>%
  left_join(
    .,
    disposition_facility_codes,
    by = join_by(disposition_facility_code)
  ) %>%
  mutate(disposition_facility_name = coalesce(label)) %>%
  select(-disposition_facility_code, -label)

# Clean up -----
rm(
  available_vars,
  missing_vars,
  data_vintage,
  provenance,
  var_rename_crosswalk,
  var_recode_crosswalk,
  file_yr
)
