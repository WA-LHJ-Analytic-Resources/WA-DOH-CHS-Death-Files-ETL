# visualize_completeness.R

visualize_completeness <- function(
  df,
  completeness_threshold = 1,
  plotly = TRUE
) {
  # Step 0: Define Variable Completeness Threshold Helper Function -----
  apply_completeness_threshold <- function(df = complete_by_year, threshold) {
    df_threshold <- df %>%
      group_by(variable) %>%
      summarise(
        over_threshold = all(pct_complete >= threshold) &
          !any(is.na(pct_complete)),
        .groups = "drop"
      ) %>%
      filter(over_threshold)

    vars_exceeding_threshold <- df_threshold %>%
      pull(variable)

    return(vars_exceeding_threshold)
  }

  # Step 1: Calculate Variable Percent Completeness by File Year and Variable -----
  var_completeness_by_year <- df %>%
    group_by(file_year) %>%
    summarise(
      across(
        everything(),
        ~ mean(!is.na(.x)), # TRUE in !is.na() is 1 (complete), FALSE is 0—mean gives percent complete.
        .names = "pcomplete_{.col}"
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      -file_year,
      names_to = "variable",
      values_to = "pct_complete"
    ) %>%
    mutate(variable = str_remove(variable, "^pcomplete_")) # Strip the "pcomplete_" prefix to keep clean variable names.

  # Step 2: Define & assign the desired variable order (in the visualization) -----
  levels_order <- var_completeness_by_year %>%
    distinct(variable) %>%
    mutate(
      sort_key = case_when(
        str_detect(variable, "^record_axis_code_\\d+$") ~
          paste0(
            "record_axis_code_",
            sprintf("%02d", as.integer(str_extract(variable, "\\d+$")))
          ),
        TRUE ~ variable
      )
    ) %>%
    arrange(sort_key) %>%
    pull(variable)

  ## Assign Proper Ordering of Variables
  var_completeness_by_year <- var_completeness_by_year %>%
    mutate(
      variable = factor(variable, levels = levels_order, ordered = TRUE)
    ) %>% # To ensure record_axis_code_# variables are properly ordered
    arrange(file_year, variable)

  # Step 3: Apply Completeness Thresholds -----
  ## Note: If a completeness threshold is applied, all variables that meet or exceed that completeness threshold for ALL file_years
  # will NOT be included in the visualization. This is meant to help focus in on incomplete variables.

  vars_to_keep <- apply_completeness_threshold(
    df = var_completeness_by_year,
    threshold = completeness_threshold
  )

  var_completeness_by_year_final <- var_completeness_by_year %>%
    filter(!variable %in% vars_to_keep) %>% # Remove these variables --> want to look at variables with lower completeness (or changing completeness over time)
    filter(!str_detect(variable, "record_axis_code")) # keep all record_axis_code variables...

  # Step 4: Create ggplot2 Heatmap -----
  heatmap <- ggplot(
    var_completeness_by_year_final %>%
      mutate(
        variable = factor(variable, levels = rev(sort(unique(variable)))),
        pct_label = percent(pct_complete, accuracy = 0.1) # 0.711111 → "71.1%"
      ),
    aes(
      x = file_year,
      y = variable,
      fill = pct_complete,
      text = paste0(
        "Variable: ",
        variable,
        "<br>",
        "Year: ",
        file_year,
        "<br>",
        "Complete: ",
        pct_label
      )
    )
  ) +
    geom_tile() +
    scale_fill_viridis_c(
      limits = c(0, 1),
      labels = percent_format(accuracy = 1),
      name = "% complete"
    ) +
    labs(
      title = "Percent Completeness by Year & Variable",
      x = "Year",
      y = "Variable"
    ) +
    theme_minimal()

  # Step 5: Convert ggplot2 Heatmap to Plotly (via ggplotly()) -----
  if (plotly == TRUE) {
    heatmap <- plotly::ggplotly(heatmap, tooltip = "text")
  }

  return(heatmap)
}
