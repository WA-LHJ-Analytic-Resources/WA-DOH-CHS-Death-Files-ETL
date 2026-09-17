#' Clean and Validate Date Variables in a Data Frame
#'
#' This function parses, cleans, validates, and logically constrains a set of
#' date variables within a data frame. It standardizes multiple date formats,
#' removes placeholder or impossible values, applies variable-specific logic
#' checks (e.g., DOB before DOD, realistic age constraints), and returns the
#' cleaned data with date variables replaced by their parsed versions.
#'
#' Additionally, the function produces a parsing error report containing values
#' that failed to parse and attaches this information as an attribute
#' \code{"date_parsing_errors"} on the returned data frame.
#'
#' @param df A data frame containing raw date variables and related fields such
#'   as \code{file_year}, \code{state_file_number}, etc.
#' @param vars A character vector of raw date variable names to parse. Defaults
#'   to six column names commonly used in mortality datasets.
#' @param orders A character vector of date formats passed to
#'   \code{lubridate::parse_date_time}. Defaults to standard YMD, Y-m-d,
#'   m/d/Y, and d%b%Y formats.
#' @param verbose Logical; if \code{TRUE}, prints additional diagnostic output.
#'   Currently not used internally, included for future expansion.
#'
#' @return A cleaned data frame with:
#'   \itemize{
#'     \item Raw date variables removed.
#'     \item Parsed versions renamed to the original variable names.
#'     \item Parsed values validated against dataset-specific logical constraints.
#'     \item An attribute \code{"date_parsing_errors"} containing examples of
#'           parsing failures (does not include examples of values converted to NA during date logic checks).
#'   }
#'
#' @details
#' The function performs the following major steps:
#' \enumerate{
#'   \item Verifies that all variables listed in \code{vars} exist in \code{df}.
#'   \item Parses raw text date fields into standardized date objects.
#'   \item Removes placeholder values (e.g., year 9999 or years <= 1850).
#'   \item Performs logical validity checks specific to mortality datasets:
#'     \itemize{
#'       \item DOB must occur before DOD.
#'       \item Age at death must be <120 years.
#'       \item Injury dates must lie between birth and death (with a 9‑month prenatal buffer).
#'       \item Received and disposition dates must occur after death and within 2 years of file_year.
#'     }
#'   \item Generates a structured report of unparsed date values.
#'   \item Replaces raw date variables with cleaned parsed versions.
#' }
#' @export

clean_date_variables <- function(
  df,
  vars = c(
    "date_harmonized",
    "date_of_birth",
    "date_of_death",
    "date_of_injury",
    "date_received",
    "disposition_date"
  ),
  orders = c("Ymd", "Y-m-d", "m/d/Y", "d%b%Y"),
  verbose = FALSE
) {
  # Stop if any specified vars do not exist in df
  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    stop(glue::glue(
      "The following variables listed in `vars` were not found in the provided data frame: {paste(missing_vars, collapse = ', ')}"
    ))
  }

  # Step 0: Create parsed variable names
  new_names <- paste0(vars, "_parsed")

  # Step 1: Parse String Values into Dates (via lubridate::parse_date_time())
  df <- df %>%
    mutate(
      across(
        .cols = all_of(vars),
        .fns = ~ lubridate::parse_date_time(
          na_if(str_squish(.x), ""),
          orders,
          tz = "UTC"
        ),
        .names = "{.col}_parsed"
      )
    )

  # Step 2: Remove Placeholder & Typo Values: If any date value is in the future (a lot of 9999 year values) or less than 1850, convert to NA
  df <- df %>%
    mutate(
      across(
        .cols = all_of(new_names),
        .fns = ~ if_else(.x > lubridate::today(), as.Date(NA), .x)
      ),
      across(
        .cols = all_of(new_names),
        .fns = ~ if_else(lubridate::year(.x) <= 1850, as.Date(NA), .x)
      )
    )

  # Step 3: Generate Parsing Error Report
  parsing_error_examples <- purrr::map_dfr(
    vars,
    function(v) {
      parsed_v <- paste0(v, "_parsed") # Parsed variable names

      df %>%
        filter(!is.na(.data[[v]]) & is.na(.data[[parsed_v]])) %>% # Filter to mismatches between original (non-NA) and parsed values (NA)
        distinct(.data[[v]], .keep_all = TRUE) %>% # Filter to distinct original values (avoid duplicative examples of parsing errors)
        transmute(
          state_file_number,
          file_year,
          variable = v,
          original_value = .data[[v]],
          parsed_value = .data[[parsed_v]]
        ) # Only returns the variables pre-specified here.
    }
  )

  # Step 4: Implement Variable-specific Logic Checks (these won't be captured in parsing_error_examples)
  df <- df %>%
    # Establish file_year date range (temporary calculated columns)
    mutate(
      file_year_start = lubridate::ymd(paste0(file_year, "-01-01")),
      file_year_end = lubridate::ymd(paste0(file_year, "-12-31"))
    ) %>%
    # (Y) Date of Death: Between the Start & End of the file_year date range
    mutate(
      date_of_death_parsed = if_else(
        between(date_of_death_parsed, file_year_start, file_year_end),
        date_of_death_parsed,
        as.Date(NA)
      )
    ) %>%
    # (Y) Date of Birth: Between a DOB value that makes individual's calculate age at death less than 120 - and End of the file_year date range.
    mutate(
      ## DOB 1: DOB cannot be after the date of death
      date_of_birth_parsed = if_else(
        date_of_birth_parsed <= date_of_death_parsed,
        date_of_birth_parsed,
        as.Date(NA)
      ),
      age_calculated = rads::calc_age(
        from = date_of_birth_parsed,
        to = date_of_death_parsed
      ),
      ## DOB 2: Between a DOB value that makes individual's calculate age at death less than 120 - and End of the file_year date range.
      date_of_birth_parsed = if_else(
        age_calculated < 120,
        date_of_birth_parsed,
        as.Date(NA)
      )
    ) %>%
    # (Y) Date of Injury: Between Date of Birth and Date of Death
    mutate(
      date_of_injury_parsed = case_when(
        date_of_injury_parsed < (date_of_birth_parsed - months(9)) ~ as.Date(
          NA
        ), # Conservative logic check: Captures potential life threatening injuries occurring during pregnancies (allows injuries to be valid up to 9 months before DOB)
        date_of_injury_parsed > date_of_death_parsed ~ as.Date(NA), # Removes injury dates that happen after DOD
        TRUE ~ date_of_injury_parsed
      )
    ) %>%
    # (Y) Date Received: Between Date of Death and within ~2 years of End of file_year date range. (Exploring the data for 2010-2024, the biggest date difference between date of death and date received was 592 days or 1.6 years)
    mutate(
      date_received_parsed = if_else(
        between(
          date_received_parsed,
          date_of_death_parsed,
          (file_year_end + years(2))
        ),
        date_received_parsed,
        as.Date(NA)
      )
    ) %>%
    # (Y) Disposition Date: Between date of death and within ~2 years of End of file_year date range
    mutate(
      disposition_date_parsed = if_else(
        between(
          disposition_date_parsed,
          date_of_death_parsed,
          (file_year_end + years(2))
        ),
        disposition_date_parsed,
        as.Date(NA)
      )
    ) %>%
    # Remove Calculation Columns
    select(-age_calculated, -file_year_start, -file_year_end)

  # Step 5: Drop Raw Date Variables, Rename Parsed Variables (to Raw Date Variable Names)
  df <- df %>%
    select(-all_of(vars)) %>% # Drop original raw date fields
    rename_with(
      # Rename parsed fields → original names
      ~vars,
      all_of(new_names)
    )

  # Step 6: Add parsing_error_examples as an attribute to output
  attr(df, "date_parsing_errors") <- parsing_error_examples

  return(df)
}
