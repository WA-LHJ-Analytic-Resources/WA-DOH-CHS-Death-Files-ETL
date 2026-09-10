# TEST.R
pacman::p_load(readxl)

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

#-------------------------------------------------------------
# 1. LOAD CROSSWALK (rename_variables sheet)
#-------------------------------------------------------------

crosswalk <- read_excel("Resources/Crosswalks/crosswalk.xlsx", sheet = "rename_variables")

#-------------------------------------------------------------
# 3. FUNCTION TO PROCESS ONE YEAR OF DATA
#-------------------------------------------------------------

process_year <- function(file_year, cw = crosswalk, file_path) {

  # Convert cw to long format: each row gives (year, from_name, to_name) -----
  cw_long <- cw %>%
    pivot_longer(
      cols = matches("^\\d{4}$"),     # matches columns named as "2010","2011",…
      names_to = "year",
      values_to = "from_name"
    ) %>%
    mutate(year = as.integer(year)) %>%
    select(year, from_name, to_name, notes)

  # Identify renaming instructions for this specific file_year -----

  cw_year <- cw_long %>%
    filter(year == file_year)

  # Identify source columns that are actually needed from this vintage ---- 
  source_vars <- cw_year %>% filter(!is.na(from_name)) %>% pull(from_name) %>% unique()

  # Read the CSV for this year; set all columns as character -----

  df <- read_csv(
    file = file_path,
    col_select = all_of(source_vars), # Only load source_vars
    col_types = cols(.default = col_character()), # force all vars to character
    show_col_types = FALSE   
  )

  # Normalize missing values & trim whitespace -----

  df <- df %>%
    mutate(across(everything(), ~stringi::stri_enc_toutf8(.x))) %>%
    mutate(across(everything(), ~stringi::stri_replace_all_regex(.x, pattern = "[\\p{C}]", replacement = ""))) %>% # Removes control characters and invalid (per UTF-8 string encoding) bytes
    mutate(across(everything(), ~na_if(.x, ""))) %>%   # convert "" → NA
    mutate(across(everything(), ~na_if(.x, "NA"))) %>% # convert "NA" → NA
    mutate(across(everything(), ~str_trim(.x, side = "both")))

  # Apply renaming logic -----

  # Create named vector for renaming:
  # names = original var (from_name), values = final var (to_name)
  rename_map <- cw_year %>%
    filter(!is.na(from_name)) %>%
    pull(from_name, name = to_name)

  # Perform renaming for variables that exist in the data vintage
  df <- df %>%
    rename(!!!rename_map) # !!! expands named vector so that name (new_var_name) = value (old_var_name)

  # For missing variables (in specific data vintages; where from_name = NA) create 100% NA to_name variable -----

  ## Identify mising variables for the specific data vintage (if any)
  missing_vars <- cw_year %>%
    filter(is.na(from_name)) %>%
    pull(to_name)

  ## Create to_name variables for missing variabkles with 100% NA
  for (v in missing_vars) {
    df[[v]] <- NA_character_
  }

  # Ensure data contain exactly the final schema columns (Useful for harmonization across vintages) -----

  final_schema <- cw$to_name

  df <- df %>%
    select(all_of(final_schema))   # Subset & reorder to only the to_name (final target schema) variables specified in the cw.

  return(df)
}


for (file_yr in death_stat_files$file_year) {

  tictoc::tic(glue("Processing the data vintage for: {file_yr}"))

  data_vintage <- death_stat_files %>% filter(file_year == file_yr)

  harmonized_list[[as.character(file_yr)]] <- process_year(
    file_year = file_yr,
    cw = crosswalk, 
    file_path = data_vintage$file_location
  )

  tictoc::toc()
}


harmonized_data <- bind_rows(harmonized_list, .id = "file_year")
