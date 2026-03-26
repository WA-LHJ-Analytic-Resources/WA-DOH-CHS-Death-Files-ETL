# load_data_vintage.R

load_data_vintage <- function(file_location, available_vars, missing_vars) {
  df <- readr::read_csv(
    file = file_location,
    col_select = all_of(available_vars), # Pull all variables listed in variable_rename_YYYY.csv listed as missing == FALSE in the data set
    col_types = cols(.default = col_character()), # Data type harmonization: Reading in all variables as character data types. Prevents unintended data type coercions (useful for ID variables with leading zeros)
    na = c("", "NA"), # Treat "",  and "NA" as missing
    trim_ws = TRUE, # Trim leading/trailing whitespace in character fields
    show_col_types = FALSE # Optional: suppress column type message
  ) %>%
    ## Adding missing_vars as 100% NA (ensure they are character data types)
    mutate(
      !!!set_names(rep(list(NA_character_), length(missing_vars)), missing_vars)
    )

  return(df)
}
