# 1_harmonize_death_files

# Identify All Death Statistical File Vintages -----
death_files <- identify_death_files(
  folder = params$raw_data_folder
) %>%
  # Filter to Finalized Death Files
  filter(
    file_ext == "csv", # Only use .csv files (sometimes there are duplicates data vintages for a single year that are .xlsx and .csv)
    file_status == "Final" # Filter data vintages only (for now)
  ) %>%
  # Add Vintage Label tag
  mutate(vintage_label = glue("{system}_{file_year}")) %>%
  # Move Vintage Label to 1st position
  relocate(vintage_label, .before = everything())

# Harmonization Process ----

tictoc::tic("Harmonize all death data vintages")

## Step 0: Initiate Data Storage Lists
harmonized_list <- list()

## Step 1: Load, Rename, and Recode Each Data Vintage

for (file_yr in unique(death_files$file_year)) {
  tictoc::tic(glue("Processing the data vintage for {file_yr}"))

  ### Step 1.1: Identify death statistical, literals, and name file vintages to be loaded
  data_vintage <- death_files %>% filter(file_year == file_yr)

  ### Step 1.2: Extact vintage metadata
  provenance <- tibble(
    vintage_label = data_vintage$vintage_label,
    file_year = data_vintage$file_year,
    source_system = data_vintage$system,
    date_harmonized = as.character(lubridate::today())
  ) %>%
    # Take 3 rows (1 per file_type) --> 1 per file_year
    distinct()

  ### Step 1.3: Load in Statistical, Literals, and Name Files (Perform 1-Data Type & 2-Schema Harmonization)
  df_stat <- process_year(
    year = file_yr,
    cw = params$variable_rename_cw_statistical,
    file_path = data_vintage %>%
      filter(file_type == "Death Statistical") %>%
      pull(file_location)
  )
  df_literals <- process_year(
    year = file_yr,
    cw = params$variable_rename_cw_literals,
    file_path = data_vintage %>%
      filter(file_type == "Cause of Death Literals") %>%
      pull(file_location)
  )
  df_names <- process_year(
    year = file_yr,
    cw = params$variable_rename_cw_names,
    file_path = data_vintage %>%
      filter(file_type == "Death Names") %>%
      pull(file_location)
  )

  ### Step 1.4: Join All Files Together (for a Single Year)
  df_combined <- df_stat %>%
    left_join(., df_literals, by = join_by(state_file_number)) %>%
    left_join(., df_names, by = join_by(state_file_number))

  ### Step 1.5: Load Single Year Combined Data into a List (append metadata/provenance)
  harmonized_list[[as.character(file_yr)]] <- df_combined %>%
    bind_cols(provenance) %>%
    relocate(
      vintage_label,
      date_harmonized,
      source_system,
      file_year,
      .before = everything()
    ) # Move metadata variables to the front.

  ### Step 1.6: Recode Variables
  harmonized_list[[as.character(file_yr)]] <- harmonized_list[[as.character(
    file_yr
  )]] %>%
    recode_variables(
      df = .,
      year = file_yr,
      cw = params$variable_recode_cw_statistical,
      verbose = FALSE
    ) # Change verbose to TRUE (if you want to see variable recoding implemented per data vintage)

  tictoc::toc()
}

## Step 3: Append all data vintages together
harmonized_data <- bind_rows(harmonized_list)

tictoc::toc()

# Clean up -----

rm(
  file_yr,
  df_stat,
  df_literals,
  df_names,
  df_combined,
  harmonized_list,
  data_vintage,
  provenance
)
