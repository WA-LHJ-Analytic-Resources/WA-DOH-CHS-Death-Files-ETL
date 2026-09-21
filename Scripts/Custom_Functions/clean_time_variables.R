#' Clean and Normalize Time Variables in a Mortality Data Frame
#'
#' This function standardizes, validates, reconstructs, and parses time-related
#' variables commonly found in mortality datasets. It supports harmonizing
#' time-of-death and time-of-injury variables that may be inconsistently encoded
#' as character strings, hour/minute components, or placeholder codes (e.g.,
#' "2400", "9999").
#'
#' The function performs several cleaning steps including punctuation removal,
#' validation of hour/minute ranges, zero-padding, reconstruction of full times
#' from component fields, placeholder normalization, and conversion into
#' proper `hms` time objects using `readr::parse_time()`.
#'
#' A structured parsing error report is attached to the output as an attribute
#' named `"time_parsing_errors"`.
#'
#' @param df A data frame containing time-of-death and time-of-injury variables,
#'   including full-format time strings and hour/minute component fields.
#' @param verbose Logical indicating whether warnings from `parse_time()` should
#'   be shown (`TRUE`) or suppressed (`FALSE`). Defaults to `FALSE`.
#'
#' @return
#' A data frame with fully cleaned and parsed time variables. Raw component
#' fields (`*_hour`, `*_minutes`) and intermediate fields (e.g.,
#' `*_primary`) are removed, and final parsed times are returned under:
#'
#' * `time_of_death`
#' * `time_of_injury`
#'
#' The returned data frame also includes:
#'
#' * `time_parsing_errors`: a tibble listing rows where original values failed
#'   to parse into valid time objects.
#'
#' @details
#' The function executes the following operations sequentially:
#'
#' \enumerate{
#'   \item Remove punctuation from all time-related fields.
#'   \item Validate hour (00–23) and minute (00–59) components.
#'   \item Zero-pad hours and minutes to width = 2.
#'   \item Construct unified time fields (`*_primary`) from hour/minute components.
#'   \item Coalesce primary times with existing full-format times.
#'   \item Normalize placeholder timestamps:
#'       \itemize{
#'         \item `"2400"` → `"0000"`
#'         \item `"9999"` → `NA`
#'       }
#'   \item Parse times with `readr::parse_time()`, conditionally suppressing warnings.
#'   \item Construct a parsing error report that includes original values,
#'         primary values, hour/minute components, and failed parsed results.
#'   \item Remove intermediate fields and rename final parsed variables to
#'         `time_of_death` and `time_of_injury`.
#' }
#'
#' @export

clean_time_variables <- function(df, verbose = FALSE) {
  # Helper function to conditionally suppress warnings during time parsing
  parse_time_wrap <- function(x, fmt = "%H%M") {
    if (verbose == TRUE) {
      readr::parse_time(x, format = fmt)
    } else if (verbose == FALSE) {
      suppressWarnings(readr::parse_time(x, format = fmt))
    }
  }

  # Step 0: Remove All Punctuation in All Time Variables
  df <- df %>%
    mutate(across(
      .cols = c(
        time_of_death,
        time_of_death_hour,
        time_of_death_minutes,
        time_of_injury,
        time_of_injury_hour,
        time_of_injury_minutes
      ),
      ~ str_remove(.x, "[:punct:]")
    ))

  # Step 1: Normalize Hour & Minute Component Variables
  df <- df %>%
    ## Hour Validation: Ensure hour values are 01-23
    mutate(
      across(
        c(
          time_of_death_hour,
          time_of_injury_hour
        ),
        ~ if_else(as.numeric(.x) >= 24, NA_character_, .x)
      ),
      ## Minute Validation: Ensure minute values are 00-59
      across(
        c(
          time_of_death_minutes,
          time_of_injury_minutes
        ),
        ~ if_else(as.numeric(.x) >= 60, NA_character_, .x)
      )
    )

  # Step 2: 0 Pad Hour & Minute Component Variables
  df <- df %>%
    mutate(
      across(
        c(
          time_of_death_hour,
          time_of_death_minutes,
          time_of_injury_hour,
          time_of_injury_minutes
        ),
        ~ str_pad(.x, width = 2, side = 'left', pad = "0")
      )
    )

  # Step 3: Create Time Variables from Hour & Minute Component Variables
  df <- df %>%
    mutate(
      time_of_death_primary = case_when(
        ## Hour & Minutes available
        !is.na(time_of_death_hour) & !is.na(time_of_death_minutes) ~ paste0(
          time_of_death_hour,
          time_of_death_minutes
        ),
        ## Hour only available
        !is.na(time_of_death_hour) & is.na(time_of_death_minutes) ~ paste0(
          time_of_death_hour,
          "00"
        ),
        TRUE ~ NA_character_
      ),
      time_of_injury_primary = case_when(
        ## Hour & Minutes available
        !is.na(time_of_injury_hour) & !is.na(time_of_injury_minutes) ~ paste0(
          time_of_injury_hour,
          time_of_injury_minutes
        ),
        ## Hour only available
        !is.na(time_of_injury_hour) & is.na(time_of_injury_minutes) ~ paste0(
          time_of_injury_hour,
          "00"
        ),
        TRUE ~ NA_character_
      )
    )

  # Step 4: Coalesce Time variables
  ## Note: Using _primary versions first and backfilling with regular time variables. Rationale: A large majority of death data vintages only provide Hour & Minute component variables (it is the preferred format)
  df <- df %>%
    mutate(
      time_of_death = coalesce(time_of_death_primary, time_of_death),
      time_of_injury = coalesce(time_of_injury_primary, time_of_injury)
    )

  # Step 5: Normalize "2400" timestamp to "0000"; Convert "9999" (common placeholder) to NA
  df <- df %>%
    mutate(
      across(
        .cols = c(time_of_death, time_of_injury),
        ~ if_else(.x == "2400", "0000", .x)
      ),
      across(
        .cols = c(time_of_death, time_of_injury),
        ~ if_else(.x == "9999", NA, .x)
      )
    )

  # Step 6: Parse Time Variables (to time data type)
  df <- df %>%
    mutate(
      time_of_death_final = parse_time_wrap(time_of_death, fmt = "%H%M"),
      time_of_injury_final = parse_time_wrap(time_of_injury, fmt = "%H%M")
    )

  # Step 7: Generate Parsing Error Report

  parsing_error_examples <- purrr::map_dfr(
    c("time_of_death", "time_of_injury"),

    function(v) {
      primary_v <- paste0(v, "_primary")
      final_v <- paste0(v, "_final")
      hour_v <- paste0(v, "_hour")
      min_v <- paste0(v, "_minutes")

      df %>%
        filter(
          (!is.na(.data[[v]]) | !is.na(.data[[primary_v]])) &
            is.na(.data[[final_v]])
        ) %>%
        distinct(.data[[v]], .data[[primary_v]], .keep_all = TRUE) %>%
        transmute(
          state_file_number,
          file_year,
          variable = v,
          original_value = .data[[v]],
          primary_value = .data[[primary_v]],
          hour_component = .data[[hour_v]],
          minute_component = .data[[min_v]],
          parsed_value = .data[[final_v]]
        )
    }
  )

  # Step 8: Remove Unecessary Variables & Rename Variables
  df <- df %>%
    select(
      -time_of_death_primary,
      -time_of_injury_primary,
      -time_of_death,
      -time_of_death_hour,
      -time_of_death_minutes,
      -time_of_injury,
      -time_of_injury_hour,
      -time_of_injury_minutes,
    ) %>%
    rename(
      time_of_death = time_of_death_final,
      time_of_injury = time_of_injury_final
    )

  # Step 9: Add parsing_error_examples as an attribute to output
  attr(df, "time_parsing_errors") <- parsing_error_examples

  return(df)
}
