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
    file_type = case_when(
      # Extract the Death Certificate File Type from the file_name (options: Lit, Names, Stat, Geo)
      str_detect(file_name, "Lit") ~ "Cause of Death Literals",
      str_detect(file_name, "Names") ~ "Death Names",
      str_detect(file_name, "Stat") ~ "Death Statistical",
      str_detect(file_name, "Stat") ~ "Death Geographic",
      TRUE ~ NA
    ),
    file_location = paths, # full path,
    file_modified_date_time = file_info(paths)$modification_time,
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
      # Remove Death Statistical Data Dictionary
      filter(
        !str_detect(file_name, "Death Statistical Dictionary and Crosswalk")
      )
  }

  # Step 4: Add File Year & File Status to "files" tibble
  files <- files %>%
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
      file_location,
      file_modified_date_time
    ) %>%
    # Arrange by File Type & File Year
    arrange(file_type, file_year)

  ## Step 5: Logic Check - Ensure 1 File Per Year
  tryCatch(
    expr = {
      ### Identify potential errors (multiple files per data vintage)
      multiple_files_per_year <- files %>%
        count(file_type, file_year) %>%
        filter(n > 1)

      if (nrow(multiple_files_per_year) > 0) {
        ### Extract the problematic files
        bad_files <- files %>%
          semi_join(
            multiple_files_per_year,
            by = c("file_type", "file_year")
          ) %>%
          group_by(file_type, file_year) %>%
          mutate(
            # Rule 1: If both Preliminary and Final exist:
            has_prelim = any(file_status == "Preliminary"),
            has_final = any(file_status == "Final"),

            remove = case_when(
              # Rule 1: Remove Preliminary when Final exists
              has_prelim & has_final & file_status == "Preliminary" ~ TRUE,

              # Rule 2: Multiple Preliminary files, remove older ones
              has_prelim & !has_final & file_status == "Preliminary" ~
                file_modified_date_time != max(file_modified_date_time),

              # Otherwise: keep
              TRUE ~ FALSE
            )
          ) %>%
          ungroup() %>%
          select(
            file_year,
            file_status,
            file_location,
            file_modified_date_time,
            remove
          )

        ### Create a clean, printable tibble for error message
        tibble_error_string <- paste(
          capture.output(print(bad_files)),
          collapse = "\n"
        )

        stop(
          paste(
            "More than 1 file found per data vintage. See 'remove' column for recommendation(s) on files to remove.",
            tibble_error_string
          ),
          call. = FALSE
        )
      }

      # Normal return from your function goes here
    },

    error = function(e) {
      message("Error: ", e$message)
      NA
    }
  )

  # Step 6: Return files tibble
  return(files)
}
