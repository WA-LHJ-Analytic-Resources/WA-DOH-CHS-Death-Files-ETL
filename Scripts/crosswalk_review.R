# crosswalk_review.R

## Identify All Crosswalk Files ----

# Step 1: Identify all files (recursively)
all_files <- fs::dir_ls(
  path = params$cw_filepath,
  recurse = TRUE,
  type = "file"
)

## Step 2: Build the 'files' tibble
cw_files <- tibble(
  file_name = fs::path_file(all_files), # final path segment (filename)
  file_location = all_files, # full path to the file
  parent_dir = fs::path_file(fs::path_dir(all_files)) # just the parent directory name
) %>%
  ## Filter to only annual data vintage crosswalks
  filter(str_detect(parent_dir, "[:digit:]{4}")) %>%
  mutate(
    rename_cw = ifelse(str_detect(file_name, "rename_variables"), TRUE, FALSE),
    recode_cw = ifelse(str_detect(file_name, "recode_variables"), TRUE, FALSE)
  )

rm(all_files)

## Load All Crosswalk Files -----

## Step 3: Initiate Storage Lists
rename_cw_list <- list()
recode_cw_list <- list()

for (file in 1:nrow(cw_files)) {
  cw_file <- cw_files %>% slice(file)

  message(glue("Processing {cw_file$file_name}"))

  if (cw_file$rename_cw == TRUE) {
    rename_cw_list[[cw_file$parent_dir]] <- readr::read_csv(
      file = cw_file$file_location,
      show_col_types = FALSE
    )
  }

  if (cw_file$recode_cw == TRUE) {
    recode_cw_list[[cw_file$parent_dir]] <- readr::read_csv(
      file = cw_file$file_location,
      show_col_types = FALSE
    )
  }
}

## Bind All Crosswalk Files Together -----

all_rename_cw <- bind_rows(rename_cw_list, .id = "file_year")
all_recode_cw <- bind_rows(recode_cw_list, .id = "file_year")

## Export Multi-Year Missing/Review Flag Variables ----

all_rename_cw %>%
  writexl::write_xlsx(
    .,
    path = here(params$cw_filepath, "all_rename_variables.xlsx")
  )

all_recode_cw %>%
  writexl::write_xlsx(
    .,
    path = here(params$cw_filepath, "all_recode_variables.xlsx")
  )

# Clean up -----
rm(all_rename_cw, all_recode_cw)
