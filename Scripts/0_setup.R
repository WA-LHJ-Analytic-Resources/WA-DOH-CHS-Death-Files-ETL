# 0_setup.R
# This script assumes the environment was created by uvr and that all
# required packages are already installed via `uvr::add()` + `uvr::sync()`.

# 1. Load Environment Variables (from .Renviron) -----

# If user needs to create or edit .Renviron manually:
# file.edit(".Renviron")
# Restart session after editing.

RAW_DATA_FOLDER <- Sys.getenv("RAW_DEATH_FILES_FOLDER")
OUTPUT_FOLDER <- Sys.getenv("HARMONIZED_DEATH_FILE_FOLDER")

print(RAW_DATA_FOLDER)
print(OUTPUT_FOLDER)

# 2. Load Required Packages (installed already by uvr) -----

library(arrow)
library(conflicted)
library(fs)
library(ggplot2)
library(glue)
library(here)
library(htmlwidgets)
library(lubridate)
library(plotly)
library(purrr)
library(rads) # installed via uvr::add("github::PHSKC-APDE/rads")
library(readr)
library(readxl)
library(scales)
library(stringi)
library(stringr)
library(tictoc)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(writexl)

## Resolve any package conflicts
conflicted::conflict_prefer("filter", "dplyr") # dplyr::filter() > plotly::filter()
conflicted::conflict_prefer("lag", "dplyr") # dplyr::lag() > stats::lag()

# 3. Load Custom Functions -----

list.files(
  path = here::here("Scripts", "Custom_Functions"),
  pattern = "\\.R$",
  full.names = TRUE,
  recursive = TRUE
) %>%
  lapply(source)

# 4. Define Project Parameters -----

params <- list()

params$code_sets <- list()
params$raw_data_folder <- RAW_DATA_FOLDER
params$output_folder <- OUTPUT_FOLDER

params$apply_variable_labels <- TRUE

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
params$age_5yr_levels <- seq(0, 85, by = 5)

# 5. Load Crosswalks & Harmonized Data Schema -----

params$crosswalk_filepath <- here::here(
  "Resources",
  "Admin",
  "Crosswalks and Schemas.xlsx"
)

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

params$variable_recode_cw_statistical <- prepare_crosswalk(
  cw_sheet = "recode_variables_statistical",
  cw_type = "Variable Recoding"
)

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

# 6. Load Code Sets -----

params$code_sets_folder <- here::here("Resources", "Code Sets")

for (set in list.files(params$code_sets_folder, full.names = TRUE)) {
  setname <- stringr::str_remove(basename(set), "_codes.csv")
  message(glue("Loading code sets for: {setname}"))
  params$code_sets[[setname]] <- readr::read_csv(
    file = set,
    show_col_types = FALSE
  )
}

rm(set, setname)
