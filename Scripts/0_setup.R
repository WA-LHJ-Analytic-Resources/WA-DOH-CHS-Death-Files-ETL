# 0_setup.R

# Setup Reproducible R Environment -----
## This project uses `rig` (R versions) & `renv` (R package versions) to maintain dependency stability over time and across collaborators.

## Restored intended R environment
# renv::restore()

## Check the status of the R environment
# renv::status()

# Define Filepaths (.Renviron file) -----
## Resource URL: https://rstats.wtf/r-startup.html#renviron
# a) Add RAW_DEATH_FILES_FOLDER (the filepath where all raw WA DOH CHS Death files are stored after being downloaded from Secure Access Washington)
# b) Add HARMONIZED_DEATH_FILE_FOLDER (The file path where the output harmonized death data file will be stored)
# c) Restart the R session after creating .Renviron file for the first time.

file.edit(".Renviron")

# Load Required R Packages -----

required_pkgs <- c(
  "arrow",
  "fs",
  "ggplot2",
  "glue",
  "here",
  "plotly",
  "purrr",
  "rads",
  "readr",
  "readxl",
  "scales",
  "stringi",
  "stringr",
  "tictoc",
  "tidyr",
  "tidyverse",
  "writexl"
)

invisible(lapply(required_pkgs, library, character.only = TRUE))

## Resolve any package conflicts
conflicted::conflict_prefer("filter", "dplyr") # dplyr::filter() > plotly::filter()
conflicted::conflict_prefer("lag", "dplyr") # dplyr::lag() > stats::lag()
conflicted::conflict_prefer("duration", "lubridate") # lubridate::duration() > arrow::duration()

# Load Custom Functions ----------------------------------------------------

custom_fns <- list.files(
  path = here::here("Scripts", "Custom_Functions"),
  pattern = "\\.R$",
  full.names = TRUE,
  recursive = TRUE
)

invisible(lapply(custom_fns, source))


# Define Parameters -----
params <- list()

## Retrieve paths from the .Renviron file
params$code_sets <- list() # to store 4_Code_Set_Expansion lookup tables
params$raw_data_folder <- Sys.getenv("RAW_DEATH_FILES_FOLDER") # Note: Should match value set in .Renviron file, if not restart R and troubleshoot .Renviron
params$output_folder <- Sys.getenv("HARMONIZED_DEATH_FILE_FOLDER") # Note: Should match value set in .Renviron file, if not restart R and troubleshoot .Renviron

## Add Variable Labels (Convert Coded Variables to Factors)
params$apply_variable_labels <- TRUE

## Define Age Grouping Levels/Cuts
params$age_6g_levels <- c(0, 1, 15, 25, 45, 65)
params$age_11g_levels <- c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85)
params$age_20g_levels <- c(
  0,
  1,
  5,
  10,
  15,
  18,
  20,
  25,
  30,
  35,
  40,
  45,
  50,
  55,
  60,
  65,
  70,
  75,
  80,
  85
)
params$age_5yr_levels <- c(
  0,
  5,
  10,
  15,
  20,
  25,
  30,
  35,
  40,
  45,
  50,
  55,
  60,
  65,
  70,
  75,
  80,
  85
)

# Load Crosswalks & Harmonized Data Schema -----
params$crosswalk_filepath <- here::here(
  "Resources",
  "Admin",
  "Crosswalks and Schemas.xlsx"
)

## Variable Renaming Crosswalks
params$variable_rename_cw_statistical <- prepare_crosswalk(
  cw_sheet = "rename_variables_statistical",
  cw_type = "Variable Renaming"
)

params$variable_rename_cw_literals <- prepare_crosswalk(
  cw_sheet = "rename_variables_literals",
  cw_type = "Variable Renaming"
)

params$variable_rename_cw_names <- prepare_crosswalk(
  cw_sheet = "rename_variables_names",
  cw_type = "Variable Renaming"
)

## Variable Recoding Crosswalk
params$variable_recode_cw_statistical <- prepare_crosswalk(
  cw_sheet = "recode_variables_statistical",
  cw_type = "Variable Recoding"
)

## Final Data Schema
params$harmonized_data_schema <- readxl::read_excel(
  path = params$crosswalk_filepath,
  sheet = "harmonized_data_schema"
) %>%
  mutate(factor_order = as.integer(factor_order)) %>%
  select(
    variable,
    data_type,
    factor_is_ordered,
    factor_value,
    factor_label,
    factor_order,
    description,
    source_file_type
  )

# Load Code Sets -----
params$code_set_files <- list.files(
  here(here::here("Resources", "Code Sets")),
  full.names = TRUE
)

for (file in params$code_set_files) {
  setname <- str_remove(basename(file), "_codes.csv")
  print(glue("Loading code sets for: {setname}"))

  params$code_sets[[setname]] <-
    read.csv(
      file = file,
      colClasses = "character" # All columns are cast to character data type (avoids read.csv() assigning data types.)
    )
}

rm(file, setname)
