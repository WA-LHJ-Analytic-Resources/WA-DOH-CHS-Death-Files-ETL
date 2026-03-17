# investigate_missingness.R
## Note: This script is meant to examine the output: harmonized_data to see if any abnormal patterns in missingness emerge that may be an artifact of our ETL process.
pacman::p_load(scales)

# Step 1: Calculate Percent Completeness by File Year and Variable ----

complete_by_year <- harmonized_data %>%
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

# Step 2: Identify Variables that meets or exceeds a defined completeness threshold (for every file_year) -----

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

complete_vars <- apply_completeness_threshold(
  df = complete_by_year,
  threshold = 1
)
mostly_complete_vars <- apply_completeness_threshold(
  df = complete_by_year,
  threshold = 0.90 # Will be used to filter out variables that have consistently >= 90% completeness across years
)

# Step 3: Filter complete_by_year using threshold variables -----
complete_by_year_filtered <- complete_by_year %>%
  filter(!variable %in% mostly_complete_vars) %>% # Remove these variables --> want to look at variables with lower completeness (or changing completeness over time)
  filter(!str_detect(variable, "record_axis_code"))


# Step 4: Heatmap Visualization -----

heatmap <- ggplot(
  complete_by_year_filtered %>%
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

plotly::ggplotly(heatmap, tooltip = "text")
