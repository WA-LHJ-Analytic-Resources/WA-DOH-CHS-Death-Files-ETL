#' Process a Single-Year Death Data File Using a Crosswalk Schema
#'
#' This function ingests a single-year WA DOH death certificate statistical .csv file,
#' selects only the source variables relevant for that vintage, normalizes
#' encoding and whitespace issues, applies a year-specific renaming crosswalk,
#' inserts missing variables as 100% `NA`, and returns a harmonized data frame that
#' matches the final unified schema across all years.
#'
#' It is designed to be used inside multi-year harmonization workflows where
#' different vintages have inconsistent column names, missing fields, and
#' varying formatting conventions.
#'
#' @param year Integer year (e.g., `2014`, `2020`) associated with the death
#'   data vintage being processed.
#' @param cw A crosswalk tibble describing how each year's variables map from
#'   source names (`from_name`) to unified target names (`to_name`). Must
#'   contain at minimum:
#'   \itemize{
#'     \item `file_year` — year associated with each mapping row
#     \item `from_name` — vintage-specific variable name (may be `NA` if the
#'           variable does not exist in that year)
#     \item `to_name` — unified schema variable name
#'   }
#' @param file_path Full path to the CSV file for the given year. The function
#'   loads the file using `readr::read_csv()` with all columns treated as
#'   character.
#'
#' @return A tibble containing the year's harmonized data, with:
#'   \itemize{
#'     \item All variables renamed to the unified schema (`to_name` values)
#'     \item Missing yearly variables created and filled with `NA_character_`
#'     \item Columns ordered exactly according to the final schema
#'   }
#'
#' @details
#' The function performs the following operations:
#'
#' \enumerate{
#'   \item **Subset crosswalk for the given year.**
#'         Extract the renaming rules and identify the source variables that
#'         actually exist in the yearly file.
#'
#'   \item **Load the CSV using only required columns**, ensuring all fields are
#'         read as character, which prevents type inconsistency across vintages.
#'
#'   \item **Normalize raw text**, including:
#'         \itemize{
#'           \item Converting encoding to UTF‑8
#'           \item Removing UTF‑8 control characters
#'           \item Converting `""` and `"NA"` to actual missing values
#'           \item Trimming surrounding whitespace
#'         }
#'
#'   \item **Apply renaming logic** via a named vector (`to_name = from_name`).
#'
#'   \item **Insert missing variables** (where the crosswalk has `from_name = NA`)
#'         by creating columns of `NA_character_`.
#'
#'   \item **Reorder columns exactly to the final unified schema**.
#' }
#'
#' @export

process_year <- function(year, cw, file_path) {
  # Identify renaming instructions for this specific file_year -----
  cw_year <- cw %>%
    filter(file_year == year)

  # Identify source columns that are actually needed from this vintage ----
  source_vars <- cw_year %>%
    filter(!is.na(from_name)) %>%
    pull(from_name) %>%
    unique()

  # Read the CSV for this year; set all columns as character -----
  df <- read_csv(
    file = file_path,
    col_select = all_of(source_vars), # Only load source_vars. all_of() will throw an error if there's a mismatch (helpful for identifying potential bugs)
    col_types = cols(.default = col_character()), # force all vars to character
    show_col_types = FALSE
  )

  # Normalize missing values & trim whitespace -----
  df <- df %>%
    mutate(across(everything(), ~ stringi::stri_enc_toutf8(.x))) %>%
    mutate(across(
      everything(),
      ~ stringi::stri_replace_all_regex(
        .x,
        pattern = "[\\p{C}]",
        replacement = ""
      )
    )) %>% # Removes control characters and invalid (per UTF-8 string encoding) bytes
    mutate(across(everything(), ~ na_if(.x, ""))) %>% # convert "" to NA
    mutate(across(everything(), ~ na_if(.x, "NA"))) %>% # convert "NA" to NA
    mutate(across(everything(), ~ str_trim(.x, side = "both")))

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
    select(all_of(final_schema)) # Subset & reorder to only the to_name (final target schema) variables specified in the cw.

  # Return processed data vintage file -----
  return(df)
}
