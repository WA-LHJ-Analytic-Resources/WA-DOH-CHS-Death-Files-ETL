#' Clean and Standardize Data Types in a Data Frame Based on a Schema
#'
#' This function audits, validates, and coerces the data types of variables in a
#' data frame using a schema data frame (`df_schema`). It checks for missing and
#' extra variables, prints warnings, converts each variable to its designated
#' type, and stores a before/after audit of data types as an attribute on the
#' returned data frame.
#'
#' @description
#' The function enforces consistency between a dataset (`df`) and a harmonized
#' schema (`df_schema`), ensuring that each variable is coerced to the correct
#' data type. Any mismatches in variable presence (missing or extra columns) are
#' flagged via warnings. Type coercion is handled through a controlled set of
#' rules, including numeric, integer, logical, factor, ordered, and character
#' types.
#'
#' @param df A data frame whose variable types will be validated and coerced.
#' @param df_schema A data frame containing two required columns:
#'   \describe{
#'     \item{variable}{Character name of the variable in `df`}
#'     \item{data_type}{Expected data type (e.g., "character", "numeric",
#'       "integer", "logical", "factor", "ordered")}
#'   }
#'
#' @return
#' A data frame identical to `df` but with variables converted to types defined
#' in `df_schema`. The returned data frame includes an attribute:
#'
#' * `data_type_conversions`: A tibble documenting the original and final class
#'   of each variable.
#'
#' @details
#' The function performs the following steps:
#'
#' \enumerate{
#'   \item Check for missing variables (in schema but not in `df`) and extra
#'         variables (in `df` but not in schema). Issues are reported via
#'         `warning()`.
#'
#'   \item Audit the original data types for all variables in `df`.
#'
#'   \item Coerce each variable listed in `df_schema` to the specified type using:
#'         \itemize{
#'           \item Numeric and integer parsing via `readr::parse_double()` and
#'                 `readr::parse_integer()`.
#'           \item Logical coercion via `coerce_logical()` (user-defined).
#'           \item Factor and ordered factor creation via `factor()`.
#'           \item Character coercion via `as.character()`.
#'           \item Date & time variables are not coereced in this function and are handled separately in clean_date_variables() and clean_time_variables().
#'         }
#'
#'   \item Re‑audit all variables and attach the audit to the output.
#'
#'   \item Return the updated data frame.
#' }
#'
#' @export

clean_data_types <- function(
  df,
  df_schema
) {
  # Step 1a: Check for missing variables in df ----
  schema_vars <- df_schema$variable
  df_missing_vars <- setdiff(schema_vars, names(df))

  # Step 1b: Check for extra variables in df (not referenced in df_schema) -----
  df_extra_vars <- setdiff(names(df), schema_vars)

  # Step 1c: Print warning messages indicating missing/extra variables in df

  ## Missing Variables Message
  if (length(df_missing_vars) > 0) {
    missing_vars_message <- paste(
      "The provided df has missing variables that were specified in the harmonized_data_schema:",
      paste(df_missing_vars, collapse = ", ")
    )

    warning(missing_vars_message)
  }

  ## Missing Variables Message
  if (length(df_extra_vars) > 0) {
    extra_vars_message <- paste(
      "The provided df has extra variables that WERE NOT specified in the harmonized_data_schema:",
      paste(df_extra_vars, collapse = ", ")
    )

    warning(extra_vars_message)
  }

  # Step 2: Audit Original Data Types -----
  audit <- tibble(
    variable = names(df),
    class = sapply(df, function(z) paste(class(z), collapse = "/"))
  )

  # Step 3: Convert Data Types (using df_schema), column by column -----
  purrr::pwalk(
    df_schema %>% select(variable, data_type),
    function(variable, data_type) {
      ## Exract Variable Being Converted & Convert to Character
      x <- df[[variable]]
      x_chr <- as.character(x)

      ## Convert Variable's data type based on df_schema$type value for the Variable
      df[[variable]] <<- switch(
        data_type,
        "character" = as.character(x),
        "numeric" = suppressWarnings(readr::parse_double(
          x_chr
        )),
        "integer" = suppressWarnings(readr::parse_integer(
          x_chr
        )),
        "logical" = coerce_logical(x),
        # "date" = lubridate::as_date(x_chr), # date variables already properly processed via clean_date_variables()
        # "time" = lubridate::hm(x_chr),  # time variables already properly processed via clean_time_variables()
        "factor" = {
          # Step 1: only coerce to factor; do not expand levels/labels yet
          factor(x_chr)
        },
        "ordered" = {
          # Step 1: coerce to ordered factor with observed order of appearance
          factor(x_chr, ordered = TRUE)
        },
        {
          msg <- sprintf(
            "Unknown type '%s' for variable '%s'; leaving as-is.",
            data_type,
            variable
          )
          x
        }
      )
    }
  )

  # Step 4: Audit Updated Data Types -----
  audit <- audit %>%
    mutate(new_class = sapply(df, function(z) paste(class(z), collapse = "/")))

  ## Add schema_audit as an attribute to output
  attr(df, "data_type_conversions") <- audit

  # Step 5: Return df -----
  return(df)
}
