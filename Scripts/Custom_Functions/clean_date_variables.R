# clean_date_variables.R

clean_date_variables <- function(
  df,
  vars,
  orders = c("Ymd", "Y-m-d", "m/d/Y", "d%b%Y"), # lubridate token formats
  tz = "UTC",
  verbose = FALSE
) {
  # Helper function to conditionally suppress warnings during date parsing
  parse_date_wrapper <- function(x) {
    if (verbose == TRUE) {
      lubridate::parse_date_time(x, orders = orders, tz = tz)
    } else if (verbose == FALSE) {
      suppressWarnings(lubridate::parse_date_time(x, orders = orders, tz = tz))
    }
  }

  # Iterate Date Parsing for all provided variables
  for (var in vars) {
    var_sym <- rlang::sym(var)

    df <- df %>%
      mutate(
        !!var_sym := str_squish(as.character(!!var_sym)),
        !!var_sym := na_if(!!var_sym, ""), # blank -> NA
        !!var_sym := parse_date_wrapper(!!var_sym),

        # Convert improbable/impossible years to NA
        !!var_sym := dplyr::case_when(
          is.na(!!var_sym) ~ NA_Date_, # already NA
          lubridate::year(!!var_sym) < 1900 ~ NA_Date_, # Meant to capture dates before 1900's (typos)
          lubridate::year(!!var_sym) >
            lubridate::year(lubridate::today()) + 1 ~ NA_Date_, # Meant to capture 9999's and dates that are more than 1 year into the future.
          TRUE ~ as.Date(!!var_sym)
        )
      )
  }

  return(df)
}
