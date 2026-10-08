# crosswalk_review.R

# Missing Variables (via Variable Rename Crosswalks) -----

statistical_missing <- params$variable_rename_cw_statistical %>%
  select(file_year, from_name, to_name) %>%
  mutate(missing = is.na(from_name), file_type = "Death Statistical")

literals_missing <- params$variable_rename_cw_literals %>%
  select(file_year, from_name, to_name) %>%
  mutate(missing = is.na(from_name), file_type = "Cause of Death Literals")

names_missing <- params$variable_rename_cw_names %>%
  select(file_year, from_name, to_name) %>%
  mutate(missing = is.na(from_name), file_type = "Death Names")

## Bind All Missing Variable Summaries together
combined_missing <- bind_rows(
  statistical_missing,
  literals_missing,
  names_missing
)

## Create Missing Summary by Variable
missing_variable_summary <- combined_missing %>%
  filter(missing == TRUE) %>%
  arrange(to_name, file_year) %>%
  group_by(file_type, to_name) %>%
  mutate(missing_years = paste0(file_year, collapse = ",")) %>%
  ungroup() %>%
  distinct(file_type, to_name, missing_years)


# Flagged Recoing Variables (via Variable Recode Crosswalk) -----

## Load in Variable Recode Crosswalk
var_recode_crosswalk <- params$variable_recode_cw_statistical %>%
  mutate(file_type = "Death Statistical")

## Create Missing Summary by Variable
flagged_variable_recode_summary <- var_recode_crosswalk %>%
  filter(review_flag == TRUE) %>%
  group_by(file_type, variable, from_code, to_code) %>%
  mutate(applicable_years = paste0(file_year, collapse = ",")) %>%
  ungroup() %>%
  distinct(
    file_type,
    variable,
    from_code,
    from_label,
    to_code,
    to_label,
    applicable_years
  )

# Save Review Summaries -----

# Name the sheets by naming the list elements
writexl::write_xlsx(
  list(
    "1_Missing_Variable_Summary" = missing_variable_summary,
    "2_Recoding_Variable_Review" = flagged_variable_recode_summary
  ),
  path = here(
    "Resources",
    "Admin",
    "Admin_Review.xlsx"
  )
)
