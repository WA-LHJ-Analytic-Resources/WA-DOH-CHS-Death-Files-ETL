# 0_setup.R

# Define Filepaths (.REnviron file) -----
## Resource URL: https://rstats.wtf/r-startup.html#renviron
## Note(s):
# a) Add RAW_DEATH_FILES_FOLDER (the filepath where all raw WA DOH CHS Death files are stored after being downloaded from Secure Access Washington)
# b) Add HARMONIZED_DEATH_FILE_FOLDER (The file path where the output harmonized death data file will be stored)
# c) Restart the R session after creating .Renviron file for the first time.

file.edit(".Renviron")

# R Packages & Custom Functions -----

## Install PHSKC RADs R Package
## URL: https://github.com/PHSKC-APDE/rads
## Note(s): You will need RTools installed to pull down and assemble an R package from GitHub (like rads): https://cran.r-project.org/bin/windows/Rtools/

# pak::pkg_install("PHSKC-APDE/rads")

# Install/Load R Packages
pacman::p_load(
  arrow,
  fs,
  ggplot2,
  glue,
  here,
  plotly,
  rads,
  readr,
  scales,
  tictoc,
  tidyverse,
  writexl
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
params$raw_data_folder <- Sys.getenv("RAW_DEATH_FILES_FOLDER") # Note: Should match value set in .Renviron file, if not restart R and troubleshoot .Renviron
params$output_folder <- Sys.getenv("HARMONIZED_DEATH_FILE_FOLDER") # Note: Should match value set in .Renviron file, if not restart R and troubleshoot .Renviron

## Add Variable Labels (Convert Coded Variables to Factors)
params$apply_variable_labels <- TRUE

## Define Crosswalk Filepaths
params$cw_folder <- here::here("Resources", "Crosswalks")
params$code_sets_folder <- here::here("Resources", "Code Sets")

# Load Code Sets -----

# fmt: skip
## Code Sets

for(set in list.files(here(params$code_sets_folder), full.names = TRUE)){
  setname <- str_remove(basename(set), "_codes.csv") 
  print(glue("Loading code sets for: {setname}"))

  params$code_sets[[setname]] <- readr::read_csv(file = set, show_col_types = FALSE)
}

rm(set, setname)
