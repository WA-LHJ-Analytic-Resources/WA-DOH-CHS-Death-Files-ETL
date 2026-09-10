# load_crosswalk.R

load_crosswalk <- function(file_year, type) {
  ## Construct Filename
  if (type == "rename") {
    filename <- paste0("rename_variables_", file_year, ".csv")
  }

  if (type == "recode") {
    filename <- paste0("recode_variables_", file_year, ".csv")
  }

  ## Load Crosswalk
  cw <- readr::read_csv(
    file = here(
      "Resources",
      "Crosswalks",
      file_year,
      filename
    ),
    show_col_types = FALSE # Optional: suppress column type message
  )
  return(cw)
}
