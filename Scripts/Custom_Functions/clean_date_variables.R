# clean_date_variables.R

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
  tz = "UTC", # Does not matter as all input variables are dates only (no times)
  verbose = FALSE
) {
  # Step 0: Create parsed variable names
  new_names <- paste0(vars, "_parsed")

  # Step 1: Parse String Values into Dates (via lubridate::parse_date_time())
  df <- df %>%
    mutate(
      across(
        all_of(vars),
        ~ lubridate::parse_date_time(na_if(str_squish(.x), ""), orders, tz),
        .names = "{.col}_parsed"
      )
    )

  # Step 2: Implement Date-specific Logic Checks
  df <- df %>%
    mutate(
      ## date_of_death_parsed: IF calculated age is less than 120 THEN keep ELSE convert to DOB to NA (value not plausible)
      age_calc = rads::calc_age(
        from = date_of_birth_parsed,
        to = date_of_death_parsed
      ),
      date_of_birth_parsed = if_else(
        age_calc < 120,
        date_of_birth_parsed,
        NA_Date_
      ),

      ## date_of_death_parsed: IF DOD year is greater than or equal to file year THEN keep ELSE convert DOD to NA (value not plausible)
      date_of_death_parsed = if_else(
        lubridate::year(date_of_death_parsed) >= as.integer(file_year),
        date_of_death_parsed,
        NA_Date_
      ),

      ## date_received_parsed: IF DR year is greater than or equal to file year THEN keep ELSE convert DR to NA (value not plausible)
      date_received_parsed = if_else(
        lubridate::year(date_received_parsed) >= as.integer(file_year),
        date_received_parsed,
        NA_Date_
      )
    ) %>%
    select(-age_calc) # remove temporary column

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

  # Step 4: Drop Raw Date Variables, Rename Parsed Variables (to Raw Date Variable Names)
  df <- df %>%
    select(-all_of(vars)) %>% # Drop original raw date fields
    rename_with(
      # Rename parsed fields → original names
      ~vars,
      all_of(new_names)
    )

  # Step 5: Return df & parsing_errors
  return(list(df_clean = df, parsing_errors = parsing_error_examples))
}
