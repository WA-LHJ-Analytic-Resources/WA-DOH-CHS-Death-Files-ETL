#' Identify Washington State Death Certificate Files In A Folder
#'
#' This function scans a folder containing WA Department of Health death
#' certificate data files, identifies all file types (Statistical, Literals,
#' Names, Geographic), normalizes known naming issues, extracts metadata (year,
#' status, system version), and performs quality checks to ensure required
#' `.csv` files exist for all available death data vintages (this data pipeline only works with .csv files).
#'
#' A tibble describing all detected files is returned. If any data vintages are
#' missing `.csv` files, the function stops with a detailed error message.
#'
#' @param folder Character path to the folder containing death certificate
#'   files. This should typically correspond to your project's raw data folder.
#' @param death_file_type Character string specifying which type of death files
#'   to return. Options include:
#'   \itemize{
#'     \item `"Death Statistical"` (default)
#'     \item `"Death Names"`
#'     \item `"Cause of Death Literals"`
#'     \item `"Death Geographic"`
#'   }
#'
#' @return
#' A tibble containing metadata for all files matching the requested
#' `death_file_type`, with columns:
#' \describe{
#'   \item{file_type}{Identified category of death file.}
#'   \item{file_name}{File name without extension.}
#'   \item{file_ext}{File extension (`csv`, `xlsx`, etc.).}
#'   \item{file_year}{Extracted 4‑digit year within the file name.}
#'   \item{system}{`"BEDROCK"` for years ≤ 2015, otherwise `"WHALES"`.}
#'   \item{file_status}{`"Final"` or `"Preliminary"` based on file name prefix.}
#'   \item{file_location}{Full file path.}
#'   \item{file_modified_date_time}{Last modified datetime.}
#' }
#'
#' @details
#' The function performs the following operations:
#'
#' \enumerate{
#'   \item Fix the known 2012 naming issue (`DeathStat2012.csv` → `DeathStatF2012.csv`)
#'         introduced by WA DOH.
#'
#'   \item List files in the folder (non-recursive), extract file names,
#'         extensions, and classify each file type using patterns such as
#'         `"Lit"`, `"Names"`, `"Stat"`.
#'
#'   \item Filter to the requested `death_file_type` (default = Statistical).
#'
#'   \item Extract the file year using a 4‑digit pattern (`"20[0-9]{2}"`).
#'
#'   \item Infer the file status:
#'         \itemize{
#'           \item `"Final"` if file name stem begins with `"F"`
#'           \item otherwise `"Preliminary"`
#'         }
#'
#'   \item Determine system source (`BEDROCK` ≤ 2015, `WHALES` > 2015).
#'
#'   \item Ensure each data vintage includes a `.csv` version. If any vintages
#'         are missing `.csv` files, the function throws a detailed `stop()`
#'         message including a bullet list of missing years.
#' }
#'
#' @export

identify_death_files <- function(
  folder,
  death_file_type = "Death Statistical"
) {
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
      str_detect(file_name, "Geo") ~ "Death Geographic",
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

  # Step 3: Subset to Specific Type of Death Files (default = Death Statistical)
  files <- files %>%
    filter(file_type == death_file_type)

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
      file_status = if_else(
        str_detect(file_name_no_ext_stem, "^F"),
        "Final",
        "Preliminary"
      ),
    ) %>%
    # Add WA DOH System Tag
    mutate(system = if_else(file_year <= 2015, "BEDROCK", 'WHALES')) %>%
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

  ## Step 5 (Error Check): Identify if any Data Vintages do not have a .csv version in the data folder
  years_missing_csv <- files %>%
    group_by(file_type, file_year) %>%
    summarise(has_csv = any(file_ext == "csv"), .groups = "drop") %>%
    filter(has_csv == FALSE)

  if (nrow(years_missing_csv) > 0) {
    # Create a bulleted list of missing vintages
    missing_msg <- years_missing_csv %>%
      mutate(
        combo = glue("- {file_type} ({file_year})")
      ) %>%
      pull(combo) %>%
      glue::glue_collapse(sep = "\n")

    stop(
      glue::glue(
        "The harmonization workflow requires .csv files (it is not designed for processing .xlsx files).\n\n",
        "The following data vintages in your data folder currently do NOT have .csv versions:\n",
        "{missing_msg}\n\n",
        "Please download the .csv versions from Secure Access Washington and add them to: {params$raw_data_folder}"
      ),
      call. = FALSE
    )
  }

  # Step 6: Return files tibble
  return(files)
}
