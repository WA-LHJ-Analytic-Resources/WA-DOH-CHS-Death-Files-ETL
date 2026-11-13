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
#'             through separate functions (e.g., `clean_date_variables()`, `clean_time_variables()`).
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

  # Step 1c: (Data Quality Check) Print error messages indicating missing/extra variables in df

  ## Missing Variables Message
  if (length(df_missing_vars) > 0) {
    rlang::abort(
      glue::glue(
        "The provided df is missing variable(s) required by df_schema: {paste(df_missing_vars, collapse = ', ')}."
      )
    )
  }

  ## Extra Variables Message
  if (length(df_extra_vars) > 0) {
    rlang::abort(
      glue::glue(
        "The provided df has extra variables that WERE NOT specified in df_schema: {paste(df_extra_vars, collapse = ', ')}."
      )
    )
  }

  # Step 2: Audit Original Data Types -----
  audit <- tibble::tibble(
    variable = names(df),
    class = purrr::map_chr(
      df,
      ~ paste(class(.x), collapse = "/")
    )
  )

  # Step 3: Convert Data Types (using df_schema), iterating over each variable -----

  ## 3a: Create iteration data frame
  schema_types <- df_schema %>% distinct(variable, data_type)

  ## 3b: Iterate data conversion over each variable
  for (var in unique(schema_types$variable)) {
    ### Extract Variable Specific Names & Data Types
    variable <- schema_types %>% filter(variable == var) %>% pull(variable)
    data_type <- schema_types %>% filter(variable == var) %>% pull(data_type)

    ### Extract Variable Values (As-Is & As a Character)
    var_values_raw <- df %>% pull(!!var)
    var_values_chr <- as.character(var_values_raw)

    ### Implement Conversion (based on target data type; data_type)
    df[[variable]] <- switch(
      data_type,
      "character" = as.character(var_values_raw),
      "numeric" = suppressWarnings(readr::parse_double(
        var_values_chr
      )),
      "integer" = suppressWarnings(readr::parse_integer(
        var_values_chr
      )),
      "logical" = coerce_logical(var_values_raw),
      "date" = var_values_raw, # date variables already properly processed via clean_date_variables()
      "time" = var_values_raw, # time variables already properly processed via clean_time_variables()
      "factor" = {
        ### Step F.1: Subset rows in data frame schema to create variable-specific schema
        var_schema <- df_schema %>%
          filter(variable == !!variable)

        ### Step F.2a: Determine if the factor is ordered
        fct_ordered <- unique(var_schema$factor_is_ordered)[1]

        ### Step F.2b: If factor is ordered, sort rows by schema_row$factor_order
        if (isTRUE(fct_ordered)) {
          #### (Data Quality Check) Are any factor_order values for the variable NA?
          if (any(is.na(var_schema$factor_order))) {
            rlang::abort(
              glue::glue(
                "Variable '{variable}' is an ordered factor but contains missing factor_order values."
              )
            )
          }

          #### (Data Quality Check) Are any factor_order values duplicated?
          duplicate_orders <- var_schema %>%
            count(factor_order) %>%
            filter(n > 1)

          if (nrow(duplicate_orders) > 0) {
            rlang::abort(
              glue::glue(
                "Variable '{variable}' contains duplicate factor_order values: {paste(duplicate_orders$factor_order, collapse = ', ')}."
              )
            )
          }

          #### Sort variable-specific schema (var_schema) by factor_order
          var_schema <- var_schema %>% arrange(factor_order)
        }

        ### Step F.3: (Data Quality Check) Ensure no duplicate entries (in final, harmonized data schema; df_schema) for factor levels-labels pairing. If more than 2+ levels mapped to 1 label, it should be addressed in recode_values()
        duplicate_fct_mappings <- var_schema %>%
          distinct(factor_value, factor_label) %>%
          count(factor_value) %>%
          filter(n > 1) # A single factor_value should never map to multiple factor_label's

        if (nrow(duplicate_fct_mappings) > 0) {
          rlang::abort(
            glue::glue(
              "Variable '{variable}' contains factor_value(s) mapped to multiple factor_label values: {paste(duplicate_fct_mappings$factor_value, collapse = ', ')}."
            )
          )
        }

        ### Step F.4: Extract unique, non-NA observed values/levels (from data frame)
        observed_levels <- unique(stats::na.omit(var_values_chr))

        ### Step F.5: Extract expected values/levels & corresponding labels (from the final, harmonized data schema; df_schema)
        expected_levels <- var_schema$factor_value # Should be unique (no duplicates) if pass Step #3 DQ Check
        expected_labels <- var_schema$factor_label

        ### Step F.6a: (Data Quality Check) Identify values present in the data but absent from the final, harmonized data schema (df_schema)
        unexpected_values <- setdiff(
          observed_levels,
          expected_levels
        )

        ### Step F.6b: Stop/Error Message for Unexpected Values
        if (length(unexpected_values) > 0) {
          rlang::abort(
            glue::glue(
              "Factor validation failed for variable '{variable}'. Found value(s) not defined in df_schema$factor_value: {paste(sort(unexpected_values), collapse = ', ')}."
            )
          )
        }

        ## Step F.7: Build factor variable
        factor(
          var_values_chr,
          levels = expected_levels,
          labels = expected_labels,
          ordered = fct_ordered
        )
      },

      ## Step F.8: (Data Quality Check) Stop the function if an unknown data-type was specified in df_schema
      {
        rlang::abort(
          glue::glue(
            "Unknown data_type '{data_type}' specified in df_schema for variable '{variable}'."
          )
        )
      }
    )
  }

  # Step 4: Audit Updated Data Types -----
  audit <- audit %>%
    mutate(
      new_class = purrr::map_chr(
        df,
        ~ paste(class(.x), collapse = "/")
      )
    ) %>%
    arrange(variable)

  # Step 5: Add audit as an attribute to output -----
  attr(df, "data_type_conversions") <- audit

  # Step 6: Return df -----
  return(df)
}
