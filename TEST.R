rm(list = ls())

# load("harmonized_data.RData")

# Setup -----
pacman::p_load(fs, glue, here, tidyverse)

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

# Select File Year ------
file_yr <- 2010

# Load Crosswalks ----

## Var Rename Crosswalk
var_rename_crosswalk <- readr::read_csv(
  file = here(
    "Resources",
    "Crosswalks",
    file_yr,
    paste0("rename_variables_", file_yr, ".csv")
  )
)

available_vars <- var_rename_crosswalk %>%
  filter(missing == FALSE) %>%
  pull(from_name)
missing_vars <- var_rename_crosswalk %>%
  filter(missing == TRUE) %>%
  pull(from_name)

## Var Recode Crosswalk
var_recode_crosswalk <- readr::read_csv(
  file = here(
    "Resources",
    "Crosswalks",
    file_yr,
    paste0("recode_variables_", file_yr, ".csv")
  )
)

# TEST (Load Data)----
file_row = death_stat_files %>% filter(file_year == file_yr)

# Load Data
df_raw <- readr::read_csv(
  file = file_row$file_location,
  col_select = all_of(available_vars), # Pull all variables listed in variable_rename_YYYY.csv listed as missing == FALSE in the data set
  col_types = cols(.default = col_character()), # Data type harmonization: Reading in all variables as character data types. Prevents unintended data type coercions (useful for ID variables with leading zeros)
  na = c("", "NA"), # Treat "",  and "NA" as missing
  trim_ws = TRUE, # Trim leading/trailing whitespace in character fields
  show_col_types = FALSE # Optional: suppress column type message
) %>%
  ## Adding missing_vars as 100% NA (ensure they are character data types)
  mutate(
    !!!set_names(rep(list(NA_character_), length(missing_vars)), missing_vars)
  ) %>%
  ## 2-Schema-Harmonization: Variable Renaming
  rename(
    !!!setNames(var_rename_crosswalk$from_name, var_rename_crosswalk$to_name)
  ) %>%
  ## Reorder Variables (State_File_Number then Alphabetical Order)
  select(all_of(c("state_file_number", sort(names(.)))))

# Recode Data
df_recoded <- df_raw %>%
  recode_variables(
    df = .,
    var_recode_cw = var_recode_crosswalk,
    verbose = TRUE,
    timed = FALSE
  )

table(df_raw$tobacco)
table(df_recoded$tobacco)
