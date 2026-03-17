# harmonize_all_vintages.R

#' Harmonize a collection of data vintages
#'
#' @description
#' Iterates over a table of input file metadata (`files`), harmonizing each vintage
#' via `harmonize_vintage()` and collecting QA outputs via `collect_unmapped()`.
#' Provides optional verbose messaging and timing for the entire run and for each
#' vintage.
#'
#' @param files A data frame/tibble with one row per vintage to harmonize. Must contain
#'   at least the columns required by `harmonize_vintage()` for `file_row`, including:
#'   - `file_location` (character): path to the source file,
#'   - `file_year` (numeric/character): the vintage year,
#'   - `vintage_label` (character): label for the vintage,
#'   - `system` (character): source system (e.g., `"BEDROCK"`, `"WHALES"`).
#'
#' @param variable_name_cw A data frame/tibble with the **variable-name** crosswalk,
#'   forwarded to `harmonize_vintage()` (and used by `build_variable_name_crosswalk()`).
#'
#' @param variable_code_cw A data frame/tibble with the **variable-code** (value) crosswalk,
#'   forwarded to `harmonize_vintage()` (and used by `apply_crosswalk()` when needed).
#'
#' @param verbose Logical (default `TRUE`). When `TRUE`, prints messages indicating
#'   which vintage is being harmonized and any messages emitted within `harmonize_vintage()`.
#'
#' @param timed Logical (default `TRUE`). When `TRUE`, uses `tictoc` timers to report
#'   elapsed time for the overall run and for each vintage.
#'
#' @details
#' **Workflow**
#' - Optionally starts an overall timer: `tictoc::tic("Harmonizing all <n> vintages")`.
#' - Loops over row indices of `files` with `purrr::map()`. For each row:
#'   - Pulls a single `file_row` (kept as a one-row tibble with `drop = FALSE`).
#'   - Optionally starts a per-vintage timer with the vintage label.
#'   - Calls `harmonize_vintage()` to perform schema, type, and (if needed) value harmonization.
#'   - Calls `collect_unmapped()` to produce QA diagnostics for unmapped codes/value issues.
#'   - Optionally stops the per-vintage timer and prints the elapsed time.
#'   - Returns a list element containing both `data` (the harmonized tibble) and `qa`.
#' - Names the resulting list with `files$vintage_label`.
#' - Optionally stops and prints the overall timer.
#'
#' **Output structure**
#' - A named list, where each element name corresponds to a `vintage_label`.
#' - Each element is a list with:
#'   - `data`: the harmonized vintage (tibble/data frame),
#'   - `qa`: the QA output returned by `collect_unmapped()`.
#'
#' **Messaging & timing**
#' - When `verbose = TRUE`, prints a message before harmonizing each vintage.
#' - When `timed = TRUE`, prints elapsed time for each vintage and the total time.
#'
#' @return A named list of length `nrow(files)`. Each element is a list with components
#'   `data` (harmonized vintage) and `qa` (QA diagnostics).

harmonize_all_vintages <- function(
  files,
  variable_name_cw,
  variable_code_cw,
  keep_labels = FALSE,
  present = "crosswalked",
  verbose = TRUE,
  timed = TRUE
) {
  # Start overall timer
  if (timed == TRUE) {
    tictoc::tic(sprintf("Harmonizing all %d vintages", nrow(files)))
  }

  # Harmonize each vintage
  harmonized_list <- purrr::map(seq_len(nrow(files)), function(i) {
    file_row <- files[i, , drop = FALSE]
    label <- as.character(file_row$vintage_label)

    if (verbose == TRUE) {
      message(sprintf("→ Harmonizing vintage: %s", label))
    }

    if (timed == TRUE) {
      tictoc::tic(sprintf("Vintage %s", label))
    }

    df_values <- harmonize_vintage(
      file_row,
      variable_name_cw = variable_name_cw,
      variable_code_cw = variable_code_cw,
      keep_labels = keep_labels,
      present = present,
      verbose = verbose
    )
    qa <- collect_unmapped(df_values, file_row)

    if (timed == TRUE) {
      # Print elapsed time for this vintage
      tictoc::toc(quiet = FALSE)
    }

    list(data = df_values, qa = qa)
  })

  # Name each list element with vintage_label
  out_names <- as.character(files$vintage_label)
  names(harmonized_list) <- out_names

  # Stop and print overall time
  if (timed == TRUE) {
    tictoc::toc(quiet = FALSE)
  }

  harmonized_list
}
