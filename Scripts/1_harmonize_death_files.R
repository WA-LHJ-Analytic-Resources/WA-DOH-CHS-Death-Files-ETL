# 1_harmonize_death_files

# Setup -----
pacman::p_load(fs, glue, here, scales, tictoc, tidyverse)

# Load All Custom functions
list.files(
  path = here::here("R"),
  pattern = "\\.R$",
  full.names = TRUE,
  recursive = TRUE
) %>%
  lapply(source)

# Define Parameters -----
params <- list()
params$code_sets <- list() # to store 4_Code_Set_Expansion lookup tables
params$raw_data_folder <- Sys.getenv("RAW_DEATH_FILES_FOLDER")
params$output_folder <- Sys.getenv("HARMONIZED_DEATH_FILE_FOLDER")

# Identify All Death Statistical File Vintages -----
files <- identify_death_files(folder = params$raw_data_folder)

death_stat_files <- files %>%
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

tictoc::tic("Harmonize all death data vintages...")

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

  ### Step 3: Identify death statistical file to be loaded
  data_vintage <- death_stat_files %>% filter(file_year == file_yr)

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
  tictoc::toc() # End data vintage-level timer
}

## Step 7: Append all data vintages together
harmonized_data <- bind_rows(clean_list, .id = "file_year")

tictoc::toc()
