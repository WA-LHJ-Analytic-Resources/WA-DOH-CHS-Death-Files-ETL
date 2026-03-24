# combine_code_columns.R

combine_code_columns <- function(data, input_vars, output_var) {
  # Capture tidyselect expression
  input_vars_quo <- enquo(input_vars)

  # Resolve selected column names
  selected_cols <- names(
    tidyselect::eval_select(
      expr = input_vars_quo,
      data = data
    )
  )

  # Build SQL concat_ws expression (DuckDB skips NULL automatically)
  sql_expr <- paste0(
    "regexp_replace(",
    "concat_ws(';', ",
    paste(selected_cols, collapse = ", "),
    "), ",
    "'(^;+|;+$)', ''",
    ")"
  )

  # Create the output_var
  data %>%
    mutate(
      !!output_var := sql(sql_expr)
    )
}
