# build_variable_name_crosswalk().R

## Note: This function takes variable_name_crosswalk.csv, takes the from_name column, splits it by the | delimiter (as there may be multiple slight variations of the from_name)
## and prepares the variable rename crosswalk.

build_variable_name_crosswalk <- function(
  df,
  variable_name_cw,
  year_col = "file_year"
) {
  yr <- unique(df[[year_col]])
  variable_name_cw %>%
    tidyr::separate_rows(from_name, sep = "\\|") %>% # explode aliases
    dplyr::filter(from_name %in% names(df)) %>%
    dplyr::group_by(to_name) %>%
    dplyr::summarise(from_name = dplyr::first(from_name), .groups = "drop") %>%
    tibble::deframe()
}

# TEST2 -----

# # Rename variables in a data frame using a crosswalk table
# rename_with_crosswalk <- function(df, variable_name_cw) {
#   # Filter to only crosswalk rows whose from_name exists in df
#   cw_matches <- variable_name_cw %>%
#     dplyr::filter(from_name %in% names(df))

#   # Identify renames that would collide with existing df column names
#   collisions <- cw_matches %>%
#     dplyr::filter(to_name %in% names(df) & to_name != from_name)

#   if (nrow(collisions)) {
#     message("Skipping renames that would create duplicates: ",
#             paste0(collisions$from_name, "->", collisions$to_name, collapse = ", "))
#   }

#   # Build mapping: unique to_name -> first matching from_name (by row order)
#   name_map <- cw_matches %>%
#     dplyr::anti_join(collisions, by = c("from_name", "to_name")) %>%
#     dplyr::distinct(to_name, .keep_all = TRUE) %>%     # first occurrence per to_name
#     dplyr::select(to_name, from_name) %>%
#     tibble::deframe()                                  # named vector: names = to_name, values = from_name

#   # Apply renaming
#   dplyr::rename(df, !!!name_map)
# }
