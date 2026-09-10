# 1_harmonize_death_files

# Identify All Death Statistical File Vintages -----
death_files <- identify_death_files(folder = params$raw_data_folder)

death_stat_files <- death_files %>%
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

# Harmonization Process ----

tictoc::tic("Harmonize all death data vintages")

## Step 0: Initiate Data Storage Lists
harmonized_list <- list()

## Step 1: Load Variable Rename & Recode Crosswalks
var_rename_crosswalk <- read_excel("Resources/Crosswalks and Schemas.xlsx", sheet = "rename_variables") %>%
   pivot_longer(
      cols = matches("^\\d{4}$"),     # matches columns named as "2010","2011",…
      names_to = "file_year",
      values_to = "from_name"
    ) %>%
    mutate(file_year = as.integer(file_year)) %>%
    select(file_year, from_name, to_name, notes)

var_recode_crosswalk <- read_excel("Resources/Crosswalks and Schemas.xlsx", sheet = "recode_variables") %>%
  select(file_year, variable, from_code, from_label, to_code, to_label)

## Step 2: Load, Rename, and Recode Each Data Vintage

for (file_yr in death_stat_files$file_year) {

  tictoc::tic(glue("Processing the data vintage for {file_yr}"))

  ### Step 2a: Identify death statistical file vintage to be loaded
  data_vintage <- death_stat_files %>% filter(file_year == file_yr)

  ### Step 2b: Extact vintage metadata
  provenance <- tibble(
    vintage_label = data_vintage$vintage_label,
    file_year = data_vintage$file_year,
    source_file = data_vintage$file_location,
    source_system = data_vintage$system,
    date_harmonized = as.character(lubridate::today())
  )

  ### Step 2c: Load in Data Vintage (Perform 1-Data Type & 2-Schema Harmonization)
  harmonized_list[[as.character(file_yr)]] <- process_year(
    year = file_yr,
    cw = var_rename_crosswalk, 
    file_path = data_vintage$file_location
  ) %>%
    bind_cols(provenance) %>%
    relocate(
      vintage_label,
      source_file,
      date_harmonized,
      source_system,
      file_year,
      .before = everything()
    ) # Move these variables to the front.
  
  ### Step 2d: Recode Variables
  harmonized_list[[as.character(file_yr)]] <- harmonized_list[[as.character(file_yr)]] %>%
    recode_variables(df = ., year = file_yr, cw = var_recode_crosswalk, verbose = FALSE) # Change verbose to TRUE (if you want to see variable recoding implemented per data vintage)
  
  tictoc::toc()
}

## Step 3: Append all data vintages together
harmonized_data <- bind_rows(harmonized_list)

tictoc::toc()

# Clean up -----

rm(
  file_yr,
  harmonized_list,
  data_vintage,
  provenance,
  var_recode_crosswalk,
  var_rename_crosswalk
)
