# identify_death_files.R

identify_death_files <- function(folder, data_only = TRUE) {
  # Step 0: Fix 2012 Death Statistical File Naming Convention
  if (file_exists(here::here(folder, "DeathStat2012.csv"))) {
    fs::file_move(
      path = here::here(folder, "DeathStat2012.csv"), # WA DOH Forgot the "F" in the file name for Finalized
      new_path = here::here(folder, "DeathStatF2012.csv") # Add the "F" to the file name for Finalized
    )
  }

  # Step 1: List files recursively under the folder
  paths <- list.files(folder, recursive = FALSE, full.names = TRUE)

  # Step 2: Build the 'files' tibble
  files <- tibble(
    file_name = path_file(paths), # just the file name
    file_type = str_extract(file_name, pattern = "Lit|Names|Stat|Geo"), # Extract the Death Certificate File Type from the file_name (options: Lit, Names, Stat, Geo)
    file_location = paths, # full path
    stringsAsFactors = FALSE
  ) %>%
    ## Add file name w/o extension, and extension column
    mutate(
      file_name_no_ext = path_ext_remove(file_name),
      file_ext = path_ext(file_name)
    )

  # (Optional) Step 3: Remove Documentation-related files
  if (data_only == TRUE) {
    files <- files %>%
      filter(file_ext %in% c("csv", "xlsx")) %>% # Add more death data file formats here (if there are more in the future)
      filter(
        !str_detect(file_name, "Death Statistical Dictionary and Crosswalk")
      )
  }

  # Step 4: Add File Year & File Status to "files" tibble
  files <- files_orig %>%
    mutate(
      ## Create file_year via detecting 4 digit strings within the file_name (that start with "20"), convert it to numeric
      file_year = str_extract(file_name, pattern = "20[0-9]{2}"),
      file_year = as.integer(file_year),
      ## File Status
      file_name_no_ext_stem = str_remove(
        file_name_no_ext,
        pattern = "DeathLit|DeathNames|DeathStat"
      ),
      file_status = ifelse(
        str_detect(file_name_no_ext_stem, "^F"),
        "Final",
        "Preliminary"
      ),
    ) %>%
    # Add WA DOH System Tag
    mutate(system = ifelse(file_year <= 2015, "BEDROCK", 'WHALES')) %>%
    # Subset & Order Variables
    select(
      file_type,
      file_name = file_name_no_ext,
      file_ext,
      file_year,
      system,
      file_status,
      file_location
    )

  # Step 4: Return files tibble
  return(files)
}
