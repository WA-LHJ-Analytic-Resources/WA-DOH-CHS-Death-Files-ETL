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
harmonized_list <- list()

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

  ### Step 4: Load in Data Vintage (Perform 1-Data Type Harmonization)
  harmonized_list[[as.character(file_yr)]] <- load_data_vintage(
    file_location = data_vintage$file_location,
    available_vars = available_vars,
    missing_vars = missing_vars
  )

  ### Step 5: Rename Variables (Perform 2-Schema Harmonization)
  harmonized_list[[as.character(file_yr)]] <- rename_variables(
    df = harmonized_list[[as.character(file_yr)]],
    var_rename_cw = var_rename_crosswalk
  )

  ### Step 6: Recode Coded Values (Perform 3-Value Harmonization)
  harmonized_list[[as.character(file_yr)]] <- recode_variables(
    df = harmonized_list[[as.character(file_yr)]],
    var_recode_cw = var_recode_crosswalk,
    verbose = FALSE,
    timed = FALSE
  )

  ### Step 7: Add Vintage Metadata
  harmonized_list[[as.character(file_yr)]] <- harmonized_list[[as.character(
    file_yr
  )]] %>%
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
harmonized_data <- bind_rows(harmonized_list, .id = "file_year")

tictoc::toc()

# Convert Date & Time Variables to Proper Data Types -----
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

# Unify Disposition Facility (Cemetery) Variables -----
harmonized_data <- unify_variables(
  df = harmonized_data,
  vars = "disposition facility",
  code_set = params$code_sets$cemetery
)

# Unify Funeral Home Variables -----
harmonized_data <- unify_variables(
  df = harmonized_data,
  vars = "funeral home",
  code_set = params$code_sets$funeral_home
)

# Combine All COD Codes -----
harmonized_data <- combine_code_columns(
  df = harmonized_data,
  input_vars = c(
    underlying_cod_code,
    matches("^record_axis_code_(?:[2-9]|1[0-9]|20)$") # Function also removes record_axis_code_1 (as it is redundant with underlying_cod_code)
  ),
  delimiter = ";",
  output_var = "all_cod_code",
  remove_inputs = TRUE
)

# Save Harmonized Data ------
save(harmonized_data, file = "harmonized_data.RData")

rm(harmonized_data, harmonized_list)
gc()


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
