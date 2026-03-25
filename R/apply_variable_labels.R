# apply_variable_labels.R

#' Apply factor/ordered levels & labels using a dictionary (no df_schema needed)
#'
#' @param df      A data.frame/tibble with data
#' @param dict_df A tibble with: variable, level, label [, order] [, ordered]
#' @return df with factored/ordered columns; audit stored in attr(df, "factor_audit")

apply_variable_labels <- function(df, dict_df) {
  audits <- list()
  vars <- dict_df %>% pull(variable) %>% unique()

  for (var in vars) {
    # Skip if var not present
    if (!var %in% names(df)) {
      warning(sprintf("Variable '%s' not found in df; skipping.", var))
      next
    }

    # Dictionary rows for the variable; order by 'order' if present
    dict_var <- dict_df %>%
      filter(variable == var) %>%
      mutate(order = dplyr::coalesce(order, dplyr::row_number())) %>%
      arrange(order)

    lvl_codes <- dict_var %>% dplyr::pull(level) %>% as.character()
    lvl_labels <- dict_var %>% dplyr::pull(label) %>% as.character()

    # Ordered flag: explicit only; default FALSE if missing
    ord <- if ("ordered" %in% names(dict_var)) {
      dict_var %>%
        dplyr::summarise(ord = any(ordered, na.rm = TRUE)) %>%
        dplyr::pull(ord)
    } else {
      FALSE
    }

    # Raw values (assumed to be canonical codes)
    x_raw <- as.character(df[[var]])

    # Optional safety: warn if any codes in df aren't in the dictionary
    unknown <- setdiff(unique(x_raw), lvl_codes)
    if (length(unknown) > 0) {
      warning(sprintf(
        "Variable '%s': %d values not in dictionary (will become NA): %s",
        var,
        length(unknown),
        paste(unknown, collapse = ", ")
      ))
    }

    # Construct factor (levels = codes, labels = display)
    df[[var]] <- factor(
      x_raw,
      levels = lvl_codes,
      labels = lvl_labels,
      ordered = ord
    )

    # Audit
    audits[[var]] <- tibble::tibble(
      variable = var,
      ordered = ord,
      n = length(x_raw),
      n_unmatched = sum(is.na(df[[var]])),
      unmatched_values = paste(
        setdiff(unique(x_raw), lvl_codes),
        collapse = ", "
      ),
      levels_codes = paste(lvl_codes, collapse = " | "),
      levels_labels = paste(lvl_labels, collapse = " | ")
    )
  }

  factor_audit <- dplyr::bind_rows(audits)
  attr(df, "factor_audit") <- factor_audit
  df
}

# dict_df = readr::read_csv(
#   file = here("Resources", "schema_factors.csv"),
#   show_col_types = FALSE
# ) %>%
#   mutate(order = as.integer(order)) %>%
#   select(variable, level, label, order, ordered, data_type)

# TEST <- apply_variable_labels(df= harmonized_data, dict_df = dict_df)

# for(variable in unique(dict_df$variable)){

#   print(glue("Showing distributions for {variable}...."))
#   print(table(harmonized_data[[variable]]))
#   print(table(TEST[[variable]]))
# }
