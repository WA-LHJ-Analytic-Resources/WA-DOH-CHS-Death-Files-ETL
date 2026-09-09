# clean_time_variables.R

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
        ~ ifelse(as.numeric(.x) >= 24, NA_character_, .x)
      ),
      ## Minute Validation: Ensure minute values are 00-59
      across(
        c(
          time_of_death_minutes,
          time_of_injury_minutes
        ),
        ~ ifelse(as.numeric(.x) >= 60, NA_character_, .x)
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
        ~ ifelse(.x == "2400", "0000", .x)
      ),
      across(
        .cols = c(time_of_death, time_of_injury),
        ~ ifelse(.x == "9999", NA, .x)
      )
    )

  # Step 6: Parse Time Variables (to time data type)
  df <- df %>%
    mutate(
      time_of_death_final = parse_time_wrap(time_of_death, fmt = "%H%M"),
      time_of_injury_final = parse_time_wrap(time_of_injury, fmt = "%H%M")
    )

  # Step 7: Generate Parsing Error Report

  parsing_errors_examples <- purrr::map_dfr(
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

  # Step 9: Return df & parsing_errors
  return(list(df_clean = df, parsing_errors = parsing_error_examples))
}
