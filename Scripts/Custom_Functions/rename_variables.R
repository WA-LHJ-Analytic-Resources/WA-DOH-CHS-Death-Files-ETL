# rename_variables.R

rename_variables <- function(df, var_rename_cw) {
  df_renamed <- df %>%
    ## 2-Schema-Harmonization: Variable Renaming
    rename(
      !!!setNames(var_rename_cw$from_name, var_rename_cw$to_name)
    ) %>%
    ## Reorder Variables (State_File_Number then Alphabetical Order)
    select(all_of(c("state_file_number", sort(names(.)))))

  return(df_renamed)
}
