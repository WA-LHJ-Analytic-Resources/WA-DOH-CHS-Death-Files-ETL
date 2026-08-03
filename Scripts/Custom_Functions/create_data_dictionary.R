# create_data_dictionary.R

create_data_dictionary <- function(
  df,
  vars_no_val = "source_file",
  vars_no_val_limit = 30
) {
  # Create Data Dictionary
  data_dictionary <- tibble(
    variable = names(df),
    class = map_chr(df, ~ paste(class(.x), collapse = ", ")),
    n_missing = map_int(df, ~ sum(is.na(.x))),
    pct_missing = map_dbl(df, ~ mean(is.na(.x))),
    n_distinct = map_int(df, ~ n_distinct(.x, na.rm = TRUE)),
    values = map_chr(
      df,
      ~ {
        vals <- unique(.x)
        vals <- vals[!is.na(vals)]

        ## Sort example values. For factor (by levels), for a character (alphabetical)
        vals <- sort(vals)

        ## Concatenate all example values into a string
        paste(vals, collapse = ", ")
      }
    )
  )

  # Refine Data Dictionary
  data_dictionary <- data_dictionary %>%
    # Remove Example Values for: 1) Date and Date-Time Variables & 2) Select Variables (High n_distinct values)
    mutate(
      values = case_when(
        # Remove example values for variables of date or datetime data classes
        map_lgl(
          df,
          ~ inherits(.x, c("Date", "POSIXct", "POSIXt", "difftime", "hms"))
        ) ~ NA_character_,

        # Remove example values for variables with high cardinality (if n_distinct >= vars_no_val_limit0)
        n_distinct > vars_no_val_limit ~ NA_character_,

        # Remove example values for user-provided variables
        variable %in% vars_no_val ~ NA_character_,

        # Pattern-based removal (variables starting with "record_axis_code")
        str_detect(variable, "^record_axis_code") ~ NA_character_,

        TRUE ~ values
      )
    ) %>%
    # Clean up Percent Missing Label
    mutate(pct_missing = glue("{round(pct_missing * 100, 1)}%")) %>%
    # Arrange the variables alphabetically
    mutate(
      sort_var = if_else(
        str_detect(variable, "^record_axis_code_\\d+$"),
        sprintf(
          "record_axis_code_%03d",
          as.integer(str_extract(variable, "\\d+$"))
        ),
        variable
      )
    ) %>%
    arrange(sort_var) %>%
    select(-sort_var)

  return(data_dictionary)
}
