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
        !!var_sym := parse_date_wrapper(!!var_sym)
      )
  }

  return(df)
}
