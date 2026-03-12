# 1_crosswalk_death_files.R

# Identify All Death Statistical File Vintages -----

files <- identify_death_files(folder = params$raw_data_folder)

death_stat_files <- files %>%
  # Filter to Finalized Death Statistical Files
  filter(
    file_type == "Stat",
    file_ext == "csv", # avoid including .xlsx or other documents (PDFs)
    file_status == "F" # Filter data vintages only (for now)
  ) %>%
  # Add Vintage Label tag
  mutate(vintage_label = glue("{system}_{file_year}")) %>%
  # Move Vintage Label to 1st position
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
harmonized_data <- purrr::map_dfr(harmonized_output_list, "data") %>% # Pull out and append all harmonized data vintages
  mutate(across(where(is.character), ~ stringi::stri_encode(., to = "UTF-8"))) # Ensure all character variables are UTF-8 encoded

qa_unmapped_codes_report <- purrr::map_dfr(harmonized_output_list, "qa") # Pull out and append all QA reports for each data vintage

# Save Raw Harmonized Data -----

## Connect to DuckDB database
con <- dbConnect(duckdb::duckdb(), dbdir = params$duckdb_filepath)

## Save Raw Harmonized Data to Table
dbWriteTable(con, "harmonized_data_raw", harmonized_data)

## Disconnect from DuckDB
dbDisconnect(con) # Close database connection after finishing run all of R script

# Clean Up -----
rm(harmonized_data, harmonized_output_list)
