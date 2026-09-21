#' Combine Multiple Code Columns into a Single Delimited Output Column
#'
#' This function concatenates multiple input columns into a single output
#' variable using `tidyr::unite()`. It supports tidyselect input (e.g.,
#' `starts_with()`, `matches()`, `c(var1, var2)`) and allows optional removal
#' of the original input columns after combination.
#'
#' The function is useful when harmonizing coding systems or collapsing multiple
#' related indicator/code columns into a single semicolon-separated string.
#'
#' @param df A data frame containing the variables to be combined.
#' @param input_vars A tidyselect expression identifying the input columns to
#'   concatenate. Supports selections such as `starts_with("code_")`,
#'   `c(code1, code2)`, or `matches("_code$")`.
#' @param output_var A character string specifying the name of the resulting
#'   combined column.
#' @param delimiter A single character used to separate combined values.
#'   Defaults to `";"`.
#' @param remove_inputs Logical; if `TRUE`, the original input variables are
#'   removed from the resulting data frame. Defaults to `FALSE`.
#'
#' @return
#' A data frame with an additional column named `output_var` containing the
#' concatenated values from `input_vars`. If `remove_inputs = TRUE`, the input
#' columns are dropped. The function also removes `record_axis_code_1` when
#' `remove_inputs = TRUE`, as this field is redundant with the combined output.
#'
#' @details
#' Internally, the function:
#'
#' \enumerate{
#'   \item Evaluates the tidyselect expression to identify input columns.
#'   \item Splices selected column names into `tidyr::unite()` using rlang
#'         quasiquotation.
#'   \item Concatenates all selected columns into a single string column while
#'         removing `NA` values.
#'   \item Optionally removes the input columns and `record_axis_code_1`.
#' }
#'
#' @export

combine_code_columns <- function(
  df,
  input_vars,
  output_var,
  delimiter = ";",
  remove_inputs = FALSE
) {
  # Step 0: Convert the output_var string to a symbol
  out_sym <- rlang::sym(output_var)

  # Step 1: Capture the tidyselect expression for input_vars
  input_quo <- rlang::enquo(input_vars)

  # Step 2: Evaluate selection against df to get the *names* of the columns
  selected_idx <- tidyselect::eval_select(input_quo, data = df)
  selected_names <- names(selected_idx)

  # Step 3: Splice the selected column names as symbols into tidyr::unite()
  df_combined <- df %>%
    tidyr::unite(
      col = !!out_sym, # name of the new column (string is allowed)
      !!!rlang::syms(selected_names), # columns to concatenate (spliced as symbols)
      sep = delimiter, # delimiter (e.g., ";")
      na.rm = TRUE, # drop NAs when concatenating
      remove = remove_inputs # optionally remove the input columns
    )

  # Step 4 (Optional): Remove Input Variables
  if (remove_inputs == TRUE) {
    df_combined <- df_combined %>%
      # Remove redundant record_axis_code_1 variable (same as underlying_cod_code)
      select(-record_axis_code_1)
  }

  # Step 5: Return the modified data frame/tibble
  return(df_combined)
}
