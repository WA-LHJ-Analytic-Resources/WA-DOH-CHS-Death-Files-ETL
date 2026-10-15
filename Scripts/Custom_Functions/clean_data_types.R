#' Clean and Standardize Data Types in a Data Frame Based on a Schema
#'
#' This function audits, validates, and coerces the data types of variables in a
#' data frame (`df`) using a harmonized schema data frame (`df_schema`). It checks
#' for missing or extra variables, converts each variable to its designated type,
#' and records a before/after audit of all data types.
#'
#' @description
#' The function enforces consistency between a dataset and an external schema by:
#' (1) validating variable presence, (2) coercing variables to their specified
#' data types, and (3) applying factor levels, labels, and ordering rules defined
#' in the schema.
#'
#' The schema supports *long-form factor specifications*, where variables with
#' multiple factor levels (e.g., age groups, sex, race) appear in multiple rows.
#' Factor ordering is not controlled by the `data_type` column; instead,
#' factor ordering is determined by `factor_is_ordered` and (optionally)
#' `factor_order`.
#'
#' @param df A data frame whose variable types will be validated and coerced.
#'
#' @param df_schema A data frame defining the expected structure of `df`. At
#' minimum, it must include:
#' \describe{
#'   \item{variable}{Name of the variable in `df`.}
#'   \item{data_type}{Target data type ("character", "numeric", "integer",
#'                    "logical", or "factor").}
#' }
#'
#' For variables with `data_type == "factor"`, `df_schema` must contain *one row
#' per factor level*, including:
#' \describe{
#'   \item{factor_value}{The raw value in `df` to be mapped to a factor level.}
#'   \item{factor_label}{A human‑readable label assigned to that level.}
#'   \item{factor_is_ordered}{Logical flag indicating whether the factor should
#'                            be constructed as ordered.}
#'   \item{factor_order}{Integer specifying the level order (only required when
#'                       `factor_is_ordered == TRUE`).}
#' }
#'
#' @return
#' A data frame identical to `df` but with variables converted to the types
#' declared in `df_schema`. An attribute `"data_type_conversions"` is attached,
#' containing a tibble summarizing old and new classes for each variable.
#'
#' @details
#' The function executes the following steps:
#'
#' \enumerate{
#'   \item Validate that all schema variables appear in `df` and warn about
#'         missing or extra variables.
#'
#'   \item Audit original data types for all variables in `df`.
#'
#'   \item Coerce each variable to its schema-defined type:
#'     \itemize{
#'       \item `numeric` / `integer`: Parsed via `readr::parse_double()` or
#'             `readr::parse_integer()`.
#'       \item `logical`: Coerced using a user-defined `coerce_logical()` helper.
#'       \item `factor`: Levels, labels, and ordering are applied based on the
#'             long-form factor specification in `df_schema`.
#'             Ordered factors are sorted using `factor_order`; unordered
#'             factors preserve the row order of `df_schema`.
#'       \item `character`: Converted with `as.character()`.
#'       \item Date/time fields are not processed here and should be handled
#'             through separate functions (e.g., `clean_date_variables()`).
#'     }
#'
#'   \item Re-audit all variable classes and attach the audit to the output.
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
    df_schema %>% select(variable, data_type) %>% distinct(),
    function(variable, data_type) {
      ## Extract Variable Being Converted & Convert to Character
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
          ## Step 1: Pull the schema rows for this variable
          schema_rows <- df_schema %>%
            dplyr::filter(.data$variable == !!variable)

          ## Step 2: Determine if the factor is ordered
          fct_ordered <- unique(schema_rows$factor_is_ordered)[1]

          ## Step 3: If factor is ordered, sort rows by schema_row$factor_order
          if (isTRUE(fct_ordered)) {
            schema_rows <- schema_rows %>% arrange(factor_order)
          }

          ## Step 4: Extract factor levels and labels
          fct_levels <- schema_rows$factor_value
          fct_labels <- schema_rows$factor_label

          ## Step 5: Build factor variable
          factor(
            x_chr,
            levels = fct_levels,
            labels = fct_labels,
            ordered = fct_ordered
          )
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
    mutate(
      new_class = sapply(df, function(z) paste(class(z), collapse = "/"))
    ) %>%
    arrange(variable)

  ## Add schema_audit as an attribute to output
  attr(df, "data_type_conversions") <- audit

  # Step 5: Return df -----
  return(df)
}
