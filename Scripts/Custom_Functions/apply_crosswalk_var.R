# apply_crosswalk_var.R

#' Apply a crosswalk for a single variable
#'
#' @description
#' Applies a code crosswalk for one variable in a data frame, joining on the
#' source coding system and the variable's raw code, and optionally controlling
#' presentation of outputs and keeping/removing label columns.
#'
#' This helper:
#' - Verifies the target variable exists in `df`; if not, it warns and returns `df` unchanged.
#' - Subsets the crosswalk `cw` to the variable of interest (`variable_name == var`) and
#'   deduplicates rows.
#' - Joins `df` to the variable-specific crosswalk using `df$system` -> `from_system`
#'   and the variable's raw value -> `{var}_from_code`.
#' - Optionally presents only the crosswalked value/label or retains both lineage and crosswalked columns.
#' - Optionally drops label columns for the variable of interest.
#'
#' @param df A data frame (or tibble) containing at least:
#'   - a column named `"system"` indicating the source coding system for each row, and
#'   - the variable to crosswalk given by `var`.
#'
#' @param cw A data frame (or tibble) representing the crosswalk. It must contain the columns:
#'   - `variable_name` (character): name of the variable this row applies to.
#'   - `from_system` (character): source coding system name.
#'   - `from_code` (character or coercible to character): raw/source code value.
#'   - `from_label` (character): human-readable label for the source code.
#'   - `to_system` (character): destination coding system name.
#'   - `to_code` (character or coercible to character): mapped/destination code value.
#'   - `to_label` (character): human-readable label for the destination code.
#'
#' @param var A single string naming the variable in `df` to crosswalk.
#'
#' @param keep_labels Logical; if `FALSE` (default) removes any label columns for the
#'   variable of interest (e.g., `{var}_from_label`, `{var}_to_label` or `{var}_label`
#'   depending on `present`). If `TRUE`, label columns are retained.
#'
#' @param present One of `c("crosswalked", "both")`. When `"crosswalked"`, the function
#'   renames `{var}_to_code` -> `{var}` and `{var}_to_label` -> `{var}_label`, and drops
#'   `{var}_from_code` and `{var}_from_label`. When `"both"`, lineage columns are preserved,
#'   including `{var}_from_code`, `{var}_from_label`, `{var}_to_code`, `{var}_to_label`.
#'
#' @return A data frame (tibble) with the crosswalk applied for the variable `var`. Depending on
#'   `present` and `keep_labels`, the output will include:
#'   - When `present = "both"`: `{var}_from_code`, `{var}_from_label`, `{var}_to_code`, `{var}_to_label`
#'     (and original `{var}` removed).
#'   - When `present = "crosswalked"`: `{var}` (renamed from `{var}_to_code`) and `{var}_label`
#'     (renamed from `{var}_to_label`), with lineage columns dropped.

apply_crosswalk_var <- function(
  df,
  cw,
  var,
  keep_labels = FALSE,
  present = c("crosswalked", "both")
) {
  present <- match.arg(present)

  # Confirm the variable to be crosswalked is in the data frame -----
  if (!var %in% names(df)) {
    warning(glue::glue("Variable '{var}' not found; skipping."))
    return(df)
  }

  # Subset crosswalk just for the variable of interest -----
  cw_var <- cw %>%
    dplyr::filter(variable_name == !!var) %>% # Filter this crosswalk to the codes for the variable of interest.
    dplyr::select(
      from_system,
      from_code,
      from_label,
      to_system,
      to_code,
      to_label
    ) %>% # Remove unneeded columns from crosswalk
    dplyr::distinct() %>% # deduplicate any Crosswalk rows
    # Make variable-specific helper variable names (ex: from_code --> marital_status_from_code)
    dplyr::rename(
      !!paste0(var, "_from_code") := from_code,
      !!paste0(var, "_from_label") := from_label,
      !!paste0(var, "_to_code") := to_code,
      !!paste0(var, "_to_label") := to_label
    )

  # Construct helper column names for this variable -----
  raw_col <- rlang::sym(var)
  raw_name <- paste0(var, "_from_code")

  # Perform Crosswalk -----
  out <- df %>%
    # Preserve raw for lineage; cast to character for safe joining
    dplyr::mutate("{raw_name}" := as.character(!!raw_col)) %>% # Adds _from_code

    # Join on system + raw value
    dplyr::left_join(
      cw_var,
      by = c(
        # Map df$system -> cw_var$from_system
        rlang::set_names("from_system", "system"),
        # Map df[[raw_name]] -> cw_var[[paste0(var, "_from_code")]]
        rlang::set_names(paste0(var, "_from_code"), raw_name)
      )
    ) %>% # Adds _from_label, to_code, _to_label
    dplyr::select(
      -!!raw_col,
      -any_of(c("from_system", "to_system")) # Removes from_system and to_system (variables from crosswalk) to prevent column name collision when apply_crosswalk_var() is iterated over (ex --> from_system.y.y.y.y.y.y)
    ) # <-- add this line

  # Toggle Presentation Mode ----
  if (present == "crosswalked") {
    out <- out %>%
      # var_to_code and var_to_label --> (var & var_label)
      rename_with(~var, .cols = paste0(var, "_to_code")) %>%
      rename_with(~ paste0(var, "_label"), .cols = paste0(var, "_to_label")) %>%
      # Remove var_from_code &  ar_from_label columns
      select(-any_of(c(paste0(var, "_from_code"), paste0(var, "_from_label"))))
  }

  # Keep Labels -----
  if (keep_labels == FALSE) {
    # Identify Label variables
    label_vars <- names(
      tidyselect::eval_select(
        tidyselect::matches(paste0("^", var, ".*_label$")),
        out
      )
    )

    # Remove Label (for the Variable of Interest)
    out <- out %>%
      dplyr::select(-dplyr::all_of(label_vars))
  }

  out
}
