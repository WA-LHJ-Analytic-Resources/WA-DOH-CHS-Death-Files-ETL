# Temporary Script to help facilitate investigation of missingness harmonized_data as described in https://github.com/WA-LHJ-Analytic-Resources/WA-DOH-CHS-Death-Files-ETL/issues/3
# Run after script 1_crosswalk_death_files.R with harmonized_data in R's memory
pacman::p_load(dplyr, naniar)

harmonized_data |>
  group_by(file_year) |>
  summarise(
    percent_mising_date_of_death = (sum(is.na(date_of_death)) / n()) * 100
  )


cols_to_check <- names(harmonized_data)[26:ncol(harmonized_data)] # starts at age column

missingness_summary <- harmonized_data |>
  group_by(file_year) |>
  summarise(across(
    all_of(cols_to_check),
    ~ (sum(is.na(.x)) / n()) * 100,
    .names = "perc_missing_{.col}"
  ))

View(missingness_summary)

harmonized_data |>
  select(file_year, age:last_col()) |>
  gg_miss_fct(fct = file_year)
