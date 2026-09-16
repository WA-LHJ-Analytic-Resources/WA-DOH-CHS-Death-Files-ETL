# crosswalk_review.R

# Missing Variables (via Variable Rename Crosswalk) -----

## Load in Variable Rename Crosswalk
var_rename_crosswalk <- params$variable_rename_cw %>%
  pivot_longer(
    cols = matches("^\\d{4}$"), # matches columns named as "2010","2011",…
    names_to = "file_year",
    values_to = "from_name"
  ) %>%
  mutate(file_year = as.integer(file_year)) %>%
  select(file_year, from_name, to_name)

## Identify Missing Variables
var_rename_crosswalk <- var_rename_crosswalk %>%
  mutate(missing = is.na(from_name))

## Create Missing Summary by Variable
missing_variable_summary <- var_rename_crosswalk %>%
  filter(missing == TRUE) %>%
  arrange(to_name, file_year) %>%
  group_by(to_name) %>%
  mutate(missing_years = paste0(file_year, collapse = ",")) %>%
  ungroup() %>%
  distinct(to_name, missing_years)


# Flagged Recoing Variables (via Variable Recode Crosswalk) -----

## Load in Variable Recode Crosswalk
var_recode_crosswalk <- params$variable_recode_cw

## Create Missing Summary by Variable
flagged_variable_recode_summary <- var_recode_crosswalk %>%
  filter(review_flag == TRUE) %>%
  group_by(variable, from_code, to_code) %>%
  mutate(applicable_years = paste0(file_year, collapse = ",")) %>%
  ungroup() %>%
  distinct(
    variable,
    from_code,
    from_label,
    to_code,
    to_label,
    applicable_years
  )

# Save Summaries -----

## Missing Variables
writexl::write_xlsx(
  missing_variable_summary,
  path = here(
    "Resources",
    "Review",
    "Missing Variables Referenced in Rename Crosswalk.xlsx"
  )
)

## Flagged Recoding
writexl::write_xlsx(
  flagged_variable_recode_summary,
  path = here(
    "Resources",
    "Review",
    "Flagged Variable Recoding Operations.xlsx"
  )
)
