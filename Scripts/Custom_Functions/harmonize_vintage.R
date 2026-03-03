# harmonize_vintage.R

#' Harmonize a single data vintage (schema, types, and values)
#'
#' @description
#' Reads one data **vintage** (source file) and harmonizes it in three stages:
#' 1) **Schema harmonization**: matches and renames variables to a standard schema using a
#'    variable-name crosswalk.
#' 2) **Type harmonization**: coerces all variables to character for consistent downstream joins.
#' 3) **Value harmonization**: optionally crosswalks variable **codes** (values) from the source
#'    system to the destination system (only when `file_row$system == "BEDROCK"`). WHALES vintages
#'    are not crosswalked.
#'
#' The function also attaches **provenance** columns (vintage label, source file, ingestion
#' timestamp, and system) and relocates them to the front of the output.
#'
#' @param file_row A single-row data structure (e.g., tibble row or list) that supplies metadata
#'   for the vintage to harmonize. It must include the elements:
#'   - `file_location` (character): path to the source file to import.
#'   - `file_year` (numeric or character): year for the vintage, appended to the data.
#'   - `vintage_label` (character): human-readable label for the vintage.
#'   - `system` (character): source system label, typically `"BEDROCK"` or `"WHALES"`.

#' @param variable_name_cw A data frame/tibble defining the **variable-name** crosswalk used to
#'   harmonize column names for the given `file_year`. Passed to
#'   `build_variable_name_crosswalk()`. Expected to map source column names to standardized names.
#'
#' @param variable_code_cw A data frame/tibble defining the **variable-code** (value) crosswalk
#'   used when `file_row$system == "BEDROCK"`. Passed to `apply_crosswalk()` to translate
#'   raw codes to destination codes (e.g., WHALES). Should contain fields necessary for
#'   `apply_crosswalk()` including `variable_name`, `from_system`, `from_code`, `to_system`,
#'   `to_code`, and optional labels.
#'
#' @param verbose Logical indicating whether to emit detailed messages during value crosswalk
#'   (forwarded to `apply_crosswalk()`). Defaults to the value provided by the caller.
#'
#' @details
#' **Reading and cleaning**
#' - The vintage is read via `rio::import()` using `file_row$file_location`, treating `""` and `"NA"`
#'   as missing, and trimming whitespace.
#' - Column names are standardized with `janitor::clean_names()`.
#' - Single-space character values (`" "`) are converted to `NA`.
#' - A `file_year` column is appended from `file_row$file_year`.
#'
#' **Provenance**
#' - The following columns are added and relocated to the front:
#'   - `vintage_label`, `source_file`, `ingestion_ts` (ISO timestamp string), `system`, `file_year`.
#'
#' **Schema harmonization**
#' - `build_variable_name_crosswalk()` is called to construct a rename map based on `variable_name_cw`
#'   and the `file_year`. Only variables present in that map are selected, and the data is renamed
#'   to the standardized schema.
#'
#' **Type harmonization**
#' - All columns are coerced to character (`as.character`) to ensure consistent joins and downstream processing.
#'
#' **Value harmonization**
#' - If `file_row$system == "BEDROCK"`, variable codes are crosswalked using `apply_crosswalk()` with:
#'   `year_col = "file_year"`, `keep_labels = FALSE`, and `present = "crosswalked"`. Informational
#'   messages are emitted describing the operation.
#' - If `file_row$system == "WHALES"`, the function emits a message and **skips** value crosswalking,
#'   returning the type-harmonized data.
#'
#' @return A tibble (or data frame) containing the harmonized vintage:
#' - Standardized **schema** (column names),
#' - All variables coerced to **character**,
#' - **Provenance** columns at the front,
#' - For BEDROCK vintages, **crosswalked** variable codes; for WHALES, codes are retained as-is.
#'

harmonize_vintage <- function(
  file_row,
  variable_name_cw,
  variable_code_cw,
  keep_labels = FALSE,
  present = "crosswalked",
  verbose
) {
  # Read & basic cleaning -----
  df_raw <- rio::import(
    file = file_row$file_location,
    na.strings = c("", "NA"),
    strip.white = TRUE
  ) %>%
    janitor::clean_names() %>%
    mutate(across(where(is.character), ~ dplyr::na_if(.x, " "))) %>%
    mutate(file_year = file_row$file_year)

  # Provenance (Information on the Data Vintage) Keep these through all steps -----
  provenance <- tibble(
    vintage_label = file_row$vintage_label,
    source_file = file_row$file_location,
    ingestion_ts = as.character(Sys.time()), # ISO timestamp string
    system = file_row$system
  )

  # Schema harmonization (match variable names) -----
  var_rename_map <- build_variable_name_crosswalk(
    df = df_raw,
    variable_name_cw = variable_name_cw,
    year_col = "file_year"
  )

  df_schema <- df_raw %>%
    select(any_of(c("file_year", unname(var_rename_map)))) %>% # Subset down to only original variables listed in var_rename_map
    dplyr::rename(!!!var_rename_map)

  # Attach provenance columns back in (ensure they’re kept during select) -----
  df_schema <- df_schema %>%
    bind_cols(provenance) %>%
    dplyr::relocate(vintage_label, source_file, ingestion_ts, system, file_year) # Move these variables to the front.

  # Data type harmonization (ensure all variables are character data type) -----
  df_types <- df_schema %>%
    mutate(across(everything(), ~ as.character(.x)))

  # Value harmonization (crosswalk all variable codes) -----

  if (file_row$system == "BEDROCK") {
    message(glue(
      "Crosswalking BEDROCK variable codes to WHALES variable codes for the following data vintage: {file_row$vintage_label}"
    ))

    df_values <- apply_crosswalk(
      df = df_types,
      crosswalk = variable_code_cw,
      year_col = "file_year",
      keep_labels = keep_labels, # overwrites original columns with crosswalked codes
      present = present,
      verbose = verbose
    )
  } else if (file_row$system == "WHALES") {
    message(glue(
      "The provided data vintage ({file_row$vintage_label}) is from the WHALES system and does not need its variable codes crosswalked."
    ))
    df_values <- df_types
  }
  # Return harmonized data for this vintage
  df_values
}
