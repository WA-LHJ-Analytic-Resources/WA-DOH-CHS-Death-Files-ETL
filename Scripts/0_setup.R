# 0_setup.R

# Define Filepaths (.REnviron file) -----
file.edit(".Renviron") # Add RAW_DEATH_FILES_FOLDER (the filepath where all raw WA DOH CHS Death files are stored after being downloaded from Secure Access Washington) and HARMONIZED_DEATH_FILE_FOLDER (The file path where DuckDB File will be stored). Restart R session after creating .Renviron file for the first time.

# R Packages & Custom Functions -----

# Install/Load R Packages
pacman::p_load(fs, glue, here, readr, scales, tictoc, tidyverse)


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

## Define DuckDB Filepath
params$duckdb_filepath <- here(
  params$output_folder,
  "Harmonized_Death_Data.duckdb"
)

## Define Crosswalk Filepaths
params$cw_folder <- here::here("Resources", "Crosswalks")
params$code_sets_folder <- here::here("Resources", "Code_Set_Expansion")

## Define Data Variables
params$date_vars <- c(
  "date_of_birth",
  "date_of_death",
  "date_of_injury",
  "date_received",
  "disposition_date"
)

# Load Code Sets -----

# fmt: skip
{
  ## Code Sets (4_Code_Set_Expansion)
  code_sets <- c("cemetery", "country", "facility", "fips", "funeral_home", "nchs_county", "nchs_state", "wa_county", "wa_county_city")

  for(set in code_sets){

    print(glue("Loading code sets for: {set}"))
    params$code_sets[[set]] <- readr::read_csv(file = here(params$code_sets_folder, paste0(set,"_codes.csv")), show_col_types = FALSE)
  }
}

rm(code_sets, set)
