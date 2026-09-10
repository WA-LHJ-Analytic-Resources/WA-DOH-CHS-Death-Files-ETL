# recode_vars.R

#' Recodes multiple variables in a data frame using a long-format crosswalk
#'
#' @description
#' Recodes values across one or more variables in \code{df} based on a
#' long-format crosswalk \code{var_recode_crosswalk} that must exist in the calling
#' environment. The crosswalk should have columns:
#' \itemize{
#'   \item \code{variable} — the target variable name in \code{df}
#'   \item \code{from_code} — the source/raw code found in \code{df[[variable]]}
#'   \item \code{to_code} — the canonical code to recode to
#' }
#'
#' The function:
#' \enumerate{
#'   \item Identifies variables to recode from \code{unique(var_recode_crosswalk$variable)}.
#'   \item For each variable, performs a join-based recode using \code{dplyr::join_by()}.
#'   \item Optionally emits messages (with \code{glue}) listing all \code{from -> to} pairs.
#'   \item Optionally times each variable's recode and the total runtime.
#' }
#'
#' @details
#' - This implementation uses \code{dplyr >= 1.1.0} (for \code{join_by()}).
#' - Values not listed in the crosswalk are left unchanged (i.e., partial recoding is supported).
#' - If a variable listed in the crosswalk is not present in \code{df}, the function will message (if \code{verbose = TRUE}) and skip it.
#' - If no pairs exist for a variable (empty mapping), the function will message (if \code{verbose = TRUE}) and skip it.
#'
#' @param df A data frame or tibble containing the variables to be recoded.
#' @param year  An integer specifying the data vintage being recoded.
#' @param cw  A data frame or tibble containing the variable recode crosswalk (including from_code and to_code columns).
#' @param verbose Logical; if \code{TRUE}, prints informative messages about which variables and code pairs are being recoded. Default: \code{TRUE}.
#' @param timed Logical; if \code{TRUE}, prints timing information per variable and total runtime. Default: \code{FALSE}.
#'
#' @return The input \code{df} with specified variables recoded according to \code{var_recode_crosswalk}. Unlisted variables or codes remain untouched.

recode_variables <- function(
  df,
  year, 
  cw = var_recode_crosswalk,
  verbose = TRUE,
  timed = FALSE
) {
  # Optional: Timer
  if (timed == TRUE) {
    tictoc::tic("Variable Recoding Process")
  }

  # Step 0: Initialize a df_recoded output data frame
  df_recoded <- df

  # Step 1: Identify variables to recode from the crosswalk
  recode_vars <- cw %>% filter(file_year == year) %>% pull(variable) %>% unique()

  # Step 2: Loop through the provided data frame and recode each variable (based on the provided variable_recode_cw)
  for (var in recode_vars) {
    # 2.1: Check if variable present in df. If not, message and continue to next variable
    if (!var %in% names(df)) {
      message(glue::glue("'{var}' not present in data frame; skipping."))
      next
    }

    # 2.2: Unique mapping look-up pairs (lu-pairs) for this variable
    lu_pairs <- cw %>%
      dplyr::filter(variable == var) %>%
      dplyr::distinct(from_code, to_code)

    # 2.3: If no pairs exist, message and continue
    if (nrow(lu_pairs) == 0) {
      message(glue::glue(
        "No recodes defined for '{var}' in this vintage; passing through as-is."
      ))
      next
    }

    # 2.4: Build a readable message of the pairs (from_code --> to_code) being recoded for each variable in the var_recode_cw
    if (verbose == TRUE) {
      pairs_txt <- lu_pairs %>%
        dplyr::mutate(
          from_code = as.character(.data$from_code),
          to_code = as.character(.data$to_code)
        ) %>%
        dplyr::transmute(pair = glue::glue("{from_code} -> {to_code}")) %>%
        dplyr::pull(pair) %>%
        paste(collapse = "; ")

      message(glue::glue("Recoding {var}: {pairs_txt}"))
    }

    # 2.5: Perform recoding of variable codes (using tidy-eval with join_by())
    df_recoded <- df_recoded %>%
      dplyr::left_join(
        lu_pairs,
        by = dplyr::join_by(!!rlang::sym(var) == from_code)
      ) %>%
      dplyr::mutate(!!rlang::sym(var) := coalesce(to_code, .data[[var]])) %>%
      dplyr::select(-to_code)
  }

  if (timed == TRUE) {
    tictoc::toc()
  }

  return(df_recoded)
}
