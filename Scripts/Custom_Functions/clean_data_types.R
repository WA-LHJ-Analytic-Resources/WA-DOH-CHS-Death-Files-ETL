# clean_data_types.R

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
        # "date" = lubridate::as_date(x_chr), # date variables already properly processed via clean_date_variables() in 1_harmonize_death_files.R
        # "time" = lubridate::hm(x_chr),  # time variables already properly processed via clean_time_variables() in 1_harmonize_death_files.R
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
  df
}
