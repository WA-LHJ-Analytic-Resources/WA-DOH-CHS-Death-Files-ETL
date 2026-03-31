# unify_variables.R

unify_variables <- function(df, vars, code_set) {
  ## Format Code Set
  code_set_formatted <- code_set %>%
    select(code, label) %>%
    mutate(
      code = as.integer(code),
      label = stringr::str_to_upper(label)
    ) %>%
    distinct(code, .keep_all = TRUE)

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
        funeral_home_name = coalesce(funeral_home_name, label), # Fill in Funeral Home Name in following order (1st: Funeral Home Name --> (if NA) --> 2nd: Funeral Home Code Labels)

        funeral_home_name = ifelse(
          is.na(funeral_home_name) &
            str_detect(funeral_home_code, "OR|EGON|DAHO"),
          "OUT OF STATE",
          funeral_home_name
        ) # A few codes were pieces of state names --> convert funeral_home_names for these pieces as "OUT OF STATE"
      ) %>%
      # Subset Unneeded Variables
      select(
        -funeral_home_code_new,
        -funeral_home_code,
        -label
      )
  }

  if (vars == "disposition facility") {
    df_unified <- df %>%
      # Convert Disposition Facility Code to Integer (for joining)
      mutate(
        disposition_facility_code = as.integer(disposition_facility_code)
      ) %>%
      # Left Join Disposition Facility Code to Labels (Cemetery Code Set)
      left_join(
        .,
        code_set_formatted,
        by = join_by(disposition_facility_code == code)
      ) %>%
      # Fill in Disposition Facility Name in following order (1st: Disposition Facility Name --> (if NA) --> 2nd: Disposition Facility Code Labels)
      mutate(
        disposition_facility_name = coalesce(
          disposition_facility_name,
          label
        )
      ) %>%
      select(
        -label,
        -disposition_facility_code
      )
  }

  return(df_unified)
}

# Implementation in 2_clean_harmonized.R (DEPRECATED) -----
## Implemented after Audit List

# Unify Variable Versions -----

## Disposition Facility Codes & Names
# harmonized_data <- unify_variables(
#   df = harmonized_data,
#   vars = "disposition facility",
#   code_set = params$code_sets$cemetery
# )

## Funeral Home Codes & Names
# harmonized_data <- unify_variables(
#   df = harmonized_data,
#   vars = "funeral home",
#   code_set = params$code_sets$funeral_home
# )
