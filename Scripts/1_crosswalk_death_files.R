# 1_crosswalk_death_files.R

# Identify All Death Statistical File Vintages -----

files <- identify_death_files(folder = params$root_folder)

death_stat_files <- files %>%
  filter(
    file_type == "Stat",
    file_status == "F",
    file_year %in% c(params$years_bedrock, params$years_whales)
  ) %>%
  mutate(vintage_label = glue("{system}_{file_year}")) %>%
  relocate(vintage_label, .before = everything())

# Harmonize Data Vintages -----

harmonized_output_list <- harmonize_all_vintages(
  files = death_stat_files,
  variable_name_cw = params$variable_name_cw,
  variable_code_cw = params$variable_code_cw,
  keep_labels = FALSE, # TRUE: adds a {var}_label that provides the coded value descriptions; FALSE: {var}_label not created.
  present = "crosswalked", # "crosswalked": only BEDROCK-->WHALES converted codes provided; "both" original BEDROCK and BEDROCK-->WHALES codes provided.
  verbose = TRUE,
  timed = TRUE
)


## Extract Harmomized Data & QA Report
harmonized_data <- purrr::map_dfr(harmonized_output_list, "data") # Pull out and append all harmonized data vintages
qa_unmapped_codes_report <- purrr::map_dfr(harmonized_output_list, "qa") # Pull out and append all QA reports for each data vintage

# Save Raw Harmonized Data -----

## TBD - Still being developed

## CSV
# Pros: Familiar file format
# Cons: Excel opening limit is ~1 million rows, not optimized for large data
# harmonized_data %>% write_csv(x = ., file = "Data/harmonized_data.csv")

## Parquet
# Pros: Optimized for large data, coding language agnostic, can easily join/paritition data by year
# Cons: A new file format, saves to whole directories, optimized for large data = lazy evaluation (maybe a new paradigm for folks)
# Source: https://r4ds.hadley.nz/arrow.html

# arrow::write_dataset(
#   dataset = harmonized_data,
#   path = here::here("Data/Harmonized_Data"),
#   format = "parquet",
#   partitioning = c("system", "file_year"), # c("system", "file_year")
#   basename_template = "death_statistical_{i}.parquet", # filename pattern
#   hive_style = TRUE, # key=value/ subdirs (year=2020/…)
#   existing_data_behavior = "overwrite" # "error" or "delete_matching"
# )

## DuckDB
# Pros: 1 file...
# Cons: Database connections may be new for folks...
