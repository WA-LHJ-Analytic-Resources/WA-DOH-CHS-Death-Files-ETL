# 0_setup.R

# Define Filepaths (.REnviron file) -----
file.edit(".Renviron") # Add RAW_DEATH_FILES_FOLDER (the filepath where all raw WA DOH CHS Death files are stored after being downloaded from Secure Access Washington)

# R Packages & Custom Functions -----

# Install/Load R Packages
pacman::p_load(
  DBI,
  dplyr,
  duckdb,
  forcats,
  fs,
  glue,
  here,
  janitor,
  lubridate,
  readr,
  stringi,
  stringr,
  tictoc,
  tidyverse
)

# Load All Custom functions
list.files(
  path = here::here("Scripts", "Custom_Functions"),
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
params$cw_filepath <- here::here("Resources", "Crosswalks")
params$codes_filepath <- here::here(params$cw_filepath, "4_Code_Set_Expansion")

## Define Data Variables
params$date_vars <- c(
  "date_of_birth",
  "date_of_death",
  "date_of_injury",
  "date_received",
  "disposition_date"
)

# Load Crosswalks -----

# fmt: skip
{

  ## Variable Name Crosswalk (1_Schema_Harmonization)
  params$variable_name_cw <- load_crosswalk(filepath = here(params$cw_filepath,"2_Schema_Harmonization","variable_name_crosswalk.csv"))

  ## Variable Code Crosswalk (3_Value_Harmonization)
  params$variable_code_cw <- load_crosswalk(filepath = here(params$cw_filepath,"3_Value_Harmonization","variable_code_crosswalk.csv"))

  ## Code Sets (4_Code_Set_Expansion)
  code_sets <- c("cemetery", "country", "facility", "fips", "funeral_home", "nchs_county", "nchs_state", "wa_county", "wa_county_city")

  for(set in code_sets){

    print(glue("Loading code sets for: {set}"))
    params$code_sets[[set]] <- load_crosswalk(filepath = here(params$codes_filepath, paste0(set,"_codes.csv")))
  }
}

rm(code_sets, set)
