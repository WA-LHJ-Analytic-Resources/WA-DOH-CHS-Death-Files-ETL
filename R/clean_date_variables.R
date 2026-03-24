# clean_date_variables.R

clean_date_variables <- function(
  df,
  vars,
  orders = c("Ymd", "Y-m-d", "m/d/Y", "d%b%Y"), # lubridate token formats
  tz = "UTC"
) {
  # Iterate Date Parsing for all provided variables
  for (var in vars) {
    var_sym <- rlang::sym(var)

    df <- df %>%
      mutate(
        !!var_sym := str_squish(as.character(!!var_sym)),
        !!var_sym := na_if(!!var_sym, ""), # blank -> NA
        !!var_sym := lubridate::parse_date_time(
          !!var_sym,
          orders = orders,
          tz = tz
        )
      )
  }

  return(df)
}
