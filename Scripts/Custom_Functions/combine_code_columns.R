# combine_code_columns.R

#' Combine multiple columns into a single delimited string column
#'
#' @param df A data frame (or tibble).
#' @param new_var Name of the output column to create (a string).
#' @param input_cols Tidyselect specifying the input columns to combine.
#'   e.g., c(col1, matches("^pattern_\\d+$"))
#' @param sep Delimiter to use between values. Default ";"
#' @param keep_source Logical: keep the source columns? Default TRUE.
#' @param na_rm Logical: drop NA values before concatenation? Default TRUE.
#'
#' @return A data frame with the new combined column.

combine_code_columns <- function(data, input_cols, new_var, sep = ";") {
  data %>%
    mutate(
      across(
        {{ input_cols }},
        ~ na_if(str_trim(.), "")
      )
    ) %>%
    tidyr::unite(
      {{ new_var }},
      {{ input_cols }},
      sep = sep,
      na.rm = TRUE,
      remove = FALSE
    )
}
