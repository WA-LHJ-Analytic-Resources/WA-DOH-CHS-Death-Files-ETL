# apply_variable_labels.R

#' Apply factor/ordered levels & labels using a dictionary (no df_schema needed)
#'
#' @param df      A data.frame/tibble with data
#' @param df_schema A tibble with: variable, level, label [, order] [, ordered]
#' @return df with factored/ordered columns; audit stored in attr(df, "factor_audit")

df <- harmonized_data
df_schema <- params$harmonized_data_schema
# var = "armed_forces"
# var <- "education"

apply_variable_labels <- function(df, df_schema) {
  # Step 0: Initialize Audits list & Factor Variables (from df_schema)
  audits <- list()
  factor_vars <- df_schema %>%
    filter(data_type == "factor") %>%
    pull(variable) %>%
    unique()

  # Step 1: Loop through each factor variable
  for (var in factor_vars) {
    ## 1a) Skip factor variable (in df_schema) if it's not present in the df
    if (!var %in% names(df)) {
      warning(sprintf("Variable '%s' not found in df; skipping.", var))
      next
    }

    ## 1b) Create a variable-specific dictionary data frame (dict_var). Detailing all factor conversions (values & labels) for the specific factor variable
    dict_var <- df_schema %>%
      filter(variable == var) %>%
      mutate(
        factor_order = dplyr::coalesce(factor_order, dplyr::row_number())
      ) %>%
      arrange(factor_order)

    ## 1c) Pull factor values & labels (from df_schema/dict_var) into string vectors
    factor_values <- dict_var %>% pull(factor_value) %>% as.character()
    factor_labels <- dict_var %>% pull(factor_label) %>% as.character()

    ## 1d) Pull all unique variable values from df
    x_raw <- as.character(df[[var]])

    ## 1f) Flag any df$var values that were not specified in df_schema/dict_var (unknown)
    unknown <- setdiff(unique(x_raw[!is.na(x_raw)]), factor_values)

    if (length(unknown) > 0) {
      warning(sprintf(
        "Variable '%s': %d values not in dictionary (will become NA): %s",
        var,
        length(unknown),
        paste(unknown, collapse = ", ")
      ))
    }

    ## 1g) Construct Finalized Factor Variables
    df[[var]] <- factor(
      x_raw,
      levels = factor_values,
      labels = factor_labels,
      ordered = any(dict_var$factor_is_ordered) # Will be TRUE/FALSE for a given factor variable being processed.
    )

    ## 1g) Construct an Audit Tibble Summary of the Factor Conversion
    audits[[var]] <- tibble::tibble(
      variable = var,
      ordered = any(dict_var$factor_is_ordered), # Will be TRUE/FALSE for a given factor variable being processed.
      n = length(x_raw),
      n_unmatched = sum(is.na(df[[var]])),
      unmatched_values = paste(
        unknown,
        collapse = ", "
      ),
      levels_codes = paste(factor_values, collapse = " | "),
      levels_labels = paste(factor_labels, collapse = " | ")
    )
  }

  # Step 2: Bind the Audits into 1 Data Frame
  factor_audit <- dplyr::bind_rows(audits)

  # Step 3: Package Audits (factor_audit) as an attribute
  attr(df, "factor_audit") <- factor_audit

  # Step 4: Return df (with factor variables)
  df
}
