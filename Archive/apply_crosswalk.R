# apply_crosswalk.R

#' Apply value crosswalks across many variables
#'
#' This function performs two passes:
#' 1) A QA-friendly pass on a temporary copy with `present = "both"` and `keep_labels = TRUE`
#'    so helper columns `{var}_from_code` and `{var}_to_code` are available for QA.
#' 2) A final pass on the original `df` using the caller's requested `present` + `keep_labels`.
#'
#' @param df A data frame that has already had schema harmonization applied.
#' @param crosswalk A tibble/data frame with columns including:
#'   variable_name, vintage_label, from_code, from_label, to_code, to_label, notes.
#' @param year_col Optional character; name of the column used to derive `vintage_label` if missing.
#' @param keep_labels Logical; if FALSE, removes all `{var}.*_label` columns produced by `apply_crosswalk_var()`
#'                    in the final pass. Use TRUE to keep labels for reporting.
#' @param present Character; one of `"crosswalked"` (overwrite `{var}` with mapped code)
#'                or `"both"` (retain original helper columns + `{var}` is removed by `apply_crosswalk_var()`).
#'                NOTE: For QA, a temporary pass always uses `"both"`.
#' @param verbose Logical; if TRUE, prints progress and QA messages.
#' @param qa_print_n Integer; number of rows to print from the QA tibble (default 20).
#'
#' @return The processed data frame with standardized codes according to `present` + `keep_labels`.
#'         The QA tibble is attached as attribute `"qa_unmapped"` for programmatic use.
#'

apply_crosswalk <- function(
  df,
  crosswalk,
  year_col = "file_year",
  keep_labels = FALSE,
  present = c("crosswalked", "both"),
  verbose = TRUE,
  qa_print_n = 20
) {
  present <- match.arg(present)

  # Ensure vintage_label exists in data frame----
  if (!"vintage_label" %in% names(df)) {
    stop(
      "Data is missing 'vintage_label' variable"
    )
  }

  # Identify variables to crosswalk ----
  vars_to_map <- crosswalk %>%
    dplyr::distinct(variable_name) %>%
    dplyr::pull(variable_name)

  # QA Check 1: QA-friendly application on a temporary copy ----
  qa_df <- df
  processed_vars <- character()
  skipped_vars <- character()

  for (var in vars_to_map) {
    if (var %in% names(qa_df)) {
      # For QA we always use 'both' + keep_labels = TRUE so helper columns exist
      qa_df <- apply_crosswalk_var(
        df = qa_df,
        cw = crosswalk,
        var = var,
        keep_labels = TRUE,
        present = "both"
      )
      processed_vars <- c(processed_vars, var)
    } else {
      skipped_vars <- c(skipped_vars, var)
      if (verbose == TRUE) {
        message(glue::glue("Skipped: '{var}' not found in data."))
      }
    }
  }

  # Messages: Which variables were successfully crosswalked vs skipped ----
  if (verbose == TRUE) {
    if (length(processed_vars) > 0) {
      message(glue::glue(
        "***Variables crosswalked ({length(processed_vars)}): {paste(processed_vars, collapse = ', ')}"
      ))
    } else {
      message("***No variables were crosswalked (none found in data).")
    }

    if (length(skipped_vars) > 0) {
      message(glue::glue(
        "***Variables in crosswalk but missing from data ({length(skipped_vars)}): {paste(skipped_vars, collapse = ', ')}"
      ))
    }
  }

  # QA Check 2: Report unmapped codes per processed variable ----
  ## Note: By "unmapped" we mean scenarios where var_from_code is not NA, but the join didn't find a var_to_code.
  qa_unmapped <- purrr::map_dfr(processed_vars, function(var) {
    raw_name <- paste0(var, "_from_code")
    to_code_tmp <- paste0(var, "_to_code")

    if (!all(c(raw_name, to_code_tmp, "vintage_label") %in% names(qa_df))) {
      return(tibble::tibble())
    }

    qa_df %>%
      dplyr::filter(!is.na(.data[[raw_name]]), is.na(.data[[to_code_tmp]])) %>%
      dplyr::count(
        variable = var,
        vintage_label,
        raw = .data[[raw_name]],
        name = "n"
      ) %>%
      dplyr::arrange(dplyr::desc(n))
  }) %>%
    tibble::as_tibble() # Force tibble printing (robust to options like na.print)

  if (verbose == TRUE) {
    if (nrow(qa_unmapped) > 0) {
      message(
        "***Quality Assurance: Unmapped codes detected in the supplied crosswalk:"
      )
      print(qa_unmapped, n = qa_print_n)
    } else {
      message(
        "***Quality Assurance: No unmapped codes detected for the supplied crosswalk."
      )
    }
  }

  # Perform Crosswalking for All Processed Variables (using user-provided present + keep_labels parameters) ----
  reduced <- df
  for (var in processed_vars) {
    reduced <- apply_crosswalk_var(
      df = reduced,
      cw = crosswalk,
      var = var,
      keep_labels = keep_labels,
      present = present
    )
  }

  # Attach QA Unmapped Report to the Output Data Frame as Metadata
  ## Note: This can be used to identify if there are any var_from_codes (raw) that do not have an assigned crosswalk --> allows us to edit crosswalks
  ### To access qa_unmapped: use this R code: qa <- attr(reduced, "qa_unmapped"); print(qa, n = 20)

  attr(reduced, "qa_unmapped") <- qa_unmapped

  reduced
}
