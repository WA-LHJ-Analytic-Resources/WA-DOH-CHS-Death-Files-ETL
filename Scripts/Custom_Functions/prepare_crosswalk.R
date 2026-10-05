# prepare_crosswalk.R

prepare_crosswalk <- function(
  cw_path = params$crosswalk_filepath,
  cw_sheet,
  cw_type
) {
  # Step 1: Load Crosswalk
  cw <- readxl::read_excel(cw_path, cw_sheet)

  # Step 2: Format Crosswalk
  if (cw_type == "Variable Renaming") {
    cw_formatted <- cw %>%
      pivot_longer(
        cols = matches("^\\d{4}$"), # matches columns named as "2010","2011",…
        names_to = "file_year",
        values_to = "from_name"
      ) %>%
      mutate(file_year = as.integer(file_year)) %>%
      select(file_year, from_name, to_name, notes)
  } else if (cw_type == "Variable Recoding") {
    cw_formatted <- cw %>%
      select(file_year, variable, from_code, from_label, to_code, to_label)
  }

  return(cw_formatted)
}
