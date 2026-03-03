# identify_death_files.R

identify_death_files <- function(folder) {
  # Step 0: Fix 2012 Death Statistical File Naming Convention
  if (file_exists(here::here(folder, "raw", "stat", "DeathStat2012.csv"))) {
    fs::file_move(
      path = here::here(folder, "raw", "stat", "DeathStat2012.csv"), # WA DOH Forgot the "F" in the file name for Finalized
      new_path = here::here(folder, "raw", "stat", "DeathStatF2012.csv") # Add the "F" to the file name for Finalized
    )
  }

  # Step 1: List files recursively under the folder
  paths <- list.files(folder, recursive = TRUE, full.names = TRUE)

  # Step 2: Build the 'files' tibble
  files <- tibble(
    file_name = path_file(paths), # just the file name
    file_type = str_to_lower(path_file(path_dir(paths))), # name of the parent subfolder
    file_location = paths, # full path
    stringsAsFactors = FALSE
  ) %>%
    ## Add file name w/o extension, and extension column
    mutate(
      file_name_no_ext = path_ext_remove(file_name),
      file_ext = path_ext(file_name)
    )

  # Step 3: Add File Year & File Status to "files" tibble
  files <- files %>%
    mutate(
      ## Create file_year via detecting 4 digit strings within the file_name, convert it to numeric
      file_year = str_extract(file_name, pattern = "[:digit:]{4}"),
      file_year = as.numeric(file_year),
      ## Extract Last Few Characters from file_name_no_ext
      second_last_char = str_sub(file_name_no_ext, -2, -2),
      third_last_char = str_sub(file_name_no_ext, -3, -3),
      fourth_last_char = str_sub(file_name_no_ext, -4, -4),
      ## Identify file_status using last few characters of file_name_no_ext
      file_status = case_when(
        second_last_char == "Q" ~ str_sub(file_name_no_ext, -2, -1), # last 2 chars
        third_last_char == "D" ~ str_sub(file_name_no_ext, -4, -1), # last 4 chars
        fourth_last_char == "P" ~ str_sub(file_name_no_ext, -4, -4), # the 'P' itself
        TRUE ~ "F"
      )
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
