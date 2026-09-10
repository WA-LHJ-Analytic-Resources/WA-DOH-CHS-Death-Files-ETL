# process_year.R

process_year <- function(year, cw = var_rename_crosswalk, file_path) {

  # Identify renaming instructions for this specific file_year -----

  cw_year <- cw %>%
    filter(file_year == year)

  # Identify source columns that are actually needed from this vintage ---- 
  source_vars <- cw_year %>% filter(!is.na(from_name)) %>% pull(from_name) %>% unique()

  # Read the CSV for this year; set all columns as character -----

  df <- read_csv(
    file = file_path,
    col_select = all_of(source_vars), # Only load source_vars. all_of() will throw an error if there's a mismatch (helpful for identifying potential bugs)
    col_types = cols(.default = col_character()), # force all vars to character
    show_col_types = FALSE   
  )

  # Normalize missing values & trim whitespace -----

  df <- df %>%
    mutate(across(everything(), ~stringi::stri_enc_toutf8(.x))) %>%
    mutate(across(everything(), ~stringi::stri_replace_all_regex(.x, pattern = "[\\p{C}]", replacement = ""))) %>% # Removes control characters and invalid (per UTF-8 string encoding) bytes
    mutate(across(everything(), ~na_if(.x, ""))) %>%   # convert "" to NA
    mutate(across(everything(), ~na_if(.x, "NA"))) %>% # convert "NA" to NA
    mutate(across(everything(), ~str_trim(.x, side = "both")))

  # Apply renaming logic -----

  # Create named vector for renaming:
  ## names = original var (from_name), values = final var (to_name)
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
    pull(to_name) %>%
    unique()

  ## Create to_name variables for missing variabkles with 100% NA
  for (v in missing_vars) {
    df[[v]] <- NA_character_
  }

  # Ensure data contain exactly the final schema columns (Useful for harmonization across vintages) -----

  final_schema <- cw %>% pull(to_name) %>% unique()

  df <- df %>%
    select(all_of(final_schema))   # Subset & reorder to only the to_name (final target schema) variables specified in the cw.

  return(df)
}