# unify_variables.R

unify_variables <- function(df, vars, code_set) {
  ## Format Code Set
  code_set_formatted <- code_set %>%
    mutate(
      code = as.integer(code),
      label = stringr::str_to_upper(label)
    ) %>%
    distinct(code, .keep_all = TRUE) %>%
    select(code, label)

  if (vars == "funeral home") {
    df_unified <- df %>%
      # Convert funeral_home_code_new to integer (for joining)
      mutate(funeral_home_code_new = as.integer(funeral_home_code)) %>%
      # Left join to funeral_home_codes
      left_join(
        .,
        code_set_formatted,
        by = join_by(funeral_home_code_new == code)
      ) %>%
      # Fill in funeral_home_name with joined labels
      mutate(
        funeral_home_name = coalesce(label),
        funeral_home_name = ifelse(
          is.na(funeral_home_name) &
            str_detect(funeral_home_code, "OR|EGON|DAHO"),
          "OUT OF STATE",
          funeral_home_name
        ) # A few codes were pieces of state names --> convert funeral_home_names for these pieces as "OUT OF STATE"
      ) %>%
      # Subset Unneeded Variables
      select(-funeral_home_code, -funeral_home_code_new, -label)
  }

  if (vars == "disposition facility") {
    df_unified <- df %>%
      mutate(
        disposition_facility_code = as.integer(disposition_facility_code)
      ) %>%
      left_join(
        .,
        code_set,
        by = join_by(disposition_facility_code == code),
        relationship = "many-to-many"
      ) %>%
      mutate(disposition_facility_name = coalesce(label)) %>%
      select(-disposition_facility_code, -label)
  }

  return(df_unified)
}
