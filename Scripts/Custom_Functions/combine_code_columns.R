# combine_code_columns.R

combine_code_columns <- function(
  df,
  input_vars,
  output_var,
  delimiter = ";",
  remove_inputs = FALSE
) {
  # Convert the output_var string to a symbol
  out_sym <- rlang::sym(output_var)

  # Capture the tidyselect expression for input_vars
  input_quo <- rlang::enquo(input_vars)

  # Evaluate selection against df to get the *names* of the columns
  selected_idx <- tidyselect::eval_select(input_quo, data = df)
  selected_names <- names(selected_idx)

  # Splice the selected column names as symbols into tidyr::unite()
  df_combined <- df %>%
    tidyr::unite(
      col = !!out_sym, # name of the new column (string is allowed)
      !!!rlang::syms(selected_names), # columns to concatenate (spliced as symbols)
      sep = delimiter, # delimiter (e.g., ";")
      na.rm = TRUE, # drop NAs when concatenating
      remove = remove_inputs # optionally remove the input columns
    ) %>%
    # Remove redundant record_axis_code_1 variable (same as underlying_cod_code)
    select(-record_axis_code_1)

  # Return the modified data frame/tibble
  df_combined
}

# Parking Lot -------

## DuckDB Appropriate Version

# combine_code_columns <- function(df, input_vars, output_var) {
#   # Capture tidyselect expression
#   input_vars_quo <- enquo(input_vars)

#   # Resolve selected column names
#   selected_cols <- names(
#     tidyselect::eval_select(
#       expr = input_vars_quo,
#       data = df
#     )
#   )

#   # Build SQL concat_ws expression (DuckDB skips NULL automatically)
#   sql_expr <- paste0(
#     "regexp_replace(",
#     "concat_ws(';', ",
#     paste(selected_cols, collapse = ", "),
#     "), ",
#     "'(^;+|;+$)', ''",
#     ")"
#   )

#   # Create the output_var
#   df %>%
#     mutate(
#       !!output_var := sql(sql_expr)
#     )
# }
