#' Create ICD-10 Condition Flag Variables
#'
#' This function applies a predefined set of ICD-10 regular-expression patterns
#' to underlying and contributing cause-of-death fields in order to generate
#' binary indicator (flag) variables for major mortality condition categories
#' (drug overdose, suicide, firearm injury, chronic diseases, etc.).
#'
#' The ICD-10 pattern definitions used by this function are sourced from the
#' Washington State Department of Health Statistical Death Dictionary and
#' Crosswalks resource:
#' https://doh.wa.gov/sites/default/files/2024-10/422-162-DeathStatisticalDictionaryAndCrosswalks.xlsx
#'
#' @param df A data frame containing, at minimum, the variables
#'   `underlying_cod_code` and `all_contributing_cod_code`, which hold the
#'   underlying and contributing ICD-10 cause-of-death codes respectively.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Creates an internal named list of ICD-10 regular-expression patterns
#'         used to detect cause-of-death groupings.
#'   \item Generates a flag for all drug overdose deaths based on the underlying
#'         ICD-10 code.
#'   \item Generates drug-specific overdose indicators by evaluating contributing
#'         ICD-10 codes only for cases identified as overdoses.
#'   \item Adds suicide-related, firearm-related, and chronic disease indicators
#'         based on underlying cause-of-death ICD-10 codes.
#'   \item Returns the input data frame with additional binary (0/1) indicator
#'         variables.
#' }
#'
#' @return
#' A data frame containing the original columns plus newly created condition flag
#' variables, each coded as 1 (condition present) or 0 (condition not present).
#'
#'
#' @export

create_condition_flags <- function(df) {
  # Create ICD-10 Condition Flag Definitions -----
  ## Source: https://doh.wa.gov/sites/default/files/2024-10/422-162-DeathStatisticalDictionaryAndCrosswalks.xlsx

  icd10_patterns <- list(
    # Drug overdose (all)
    all_drug = "^(X4[0-4]|X6[0-4]|X85|Y1[0-4]).*",

    # Drug-specific
    opioids = "T40[0-4]",
    heroin = "T401",
    natural_semisynthetic = "T402",
    methadone = "T403",
    synthetic_opioid = "T404",
    cocaine = "T405",
    prescription_opioid = "T40[2-4]",
    psychostimulant = "T436",

    # Suicide
    suicide_all = "^(U03|X6[0-9]|X7[0-9]|X8[0-4]|Y870)",
    suicide_firearm = "^(X7[2-4])",
    suicide_asphyxia = "^(X70)",
    suicide_poisoning = "^(X6[0-9])",
    suicide_other = "^(U03|X6[0-9]|X71|X7[5-9]|X8[0-4]|Y870)",

    # Firearm
    firearm_all = "^(W3[2-4]|X7[2-4]|X9[3-5]|Y2[2-4]|Y350)",
    firearm_unintentional = "^(W3[2-4])",
    firearm_homicide = "^(X9[3-5])",
    firearm_undetermined = "^(Y2[2-4])",
    firearm_suicide = "^(X7[2-4])",
    firearm_legal = "^(Y350)",

    # Other indicators
    malignant_neoplasm = "^C[0-9].*",
    heart_disease = "^(I0[0-9]|I11|I13|I2[0-9]|I3[0-9]|I4[0-9]|I5[0-1])",
    alzheimers = "^(G30)",
    unintentional_injury = "^(V0[1-9]|V[1-9][0-9]|X[0-5][0-9]|Y8[5-6])",
    chronic_lower_resp = "^J4.*",
    cerebrovascular = "^(I6[0-9])",
    diabetes = "^(E1[0-4])",
    chronic_liver = "^(K70|K7[3-4])",
    influenza_pneumonia = "^(J09|J1[0-8])",
    parkinsons = "^(G2[0-1])",
    essential_hypertension = "^(I10|I12|I15)",
    pneumonitis = "^(J69)",
    homicide = "^(U0[1-2]|X8[5-9]|X9[0-9]|Y0[0-9]|Y871)"
  )

  # Create ICD-10 Condition Flag Variables ------

  df %>%
    # Add All Drug Overdose Flag (Includes Underlying Cause of Death ICD-10 Only)
    mutate(
      flag_all_drug = if_else(
        str_detect(underlying_cod_code, icd10_patterns$all_drug),
        1,
        0
      )
    ) %>%

    # Add Drug-Specific Overdose Flags (Includes Underlying & Contributing Cause of Death ICD-10) -----
    mutate(
      flag_opioids = if_else(
        flag_all_drug == 1 &
          str_detect(all_contributing_cod_code, icd10_patterns$opioids),
        1,
        0
      ),
      flag_heroin = if_else(
        flag_all_drug == 1 &
          str_detect(all_contributing_cod_code, icd10_patterns$heroin),
        1,
        0
      ),
      flag_natural_semisynthetic_opioid = if_else(
        flag_all_drug == 1 &
          str_detect(
            all_contributing_cod_code,
            icd10_patterns$natural_semisynthetic
          ),
        1,
        0
      ),
      flag_methadone = if_else(
        flag_all_drug == 1 &
          str_detect(all_contributing_cod_code, icd10_patterns$methadone),
        1,
        0
      ),
      flag_synthetic_opioid = if_else(
        flag_all_drug == 1 &
          str_detect(
            all_contributing_cod_code,
            icd10_patterns$synthetic_opioid
          ),
        1,
        0
      ),
      flag_cocaine = if_else(
        flag_all_drug == 1 &
          str_detect(all_contributing_cod_code, icd10_patterns$cocaine),
        1,
        0
      ),
      flag_prescription_opioid = if_else(
        flag_all_drug == 1 &
          str_detect(
            all_contributing_cod_code,
            icd10_patterns$prescription_opioid
          ),
        1,
        0
      ),
      flag_psychostimulant = if_else(
        flag_all_drug == 1 &
          str_detect(all_contributing_cod_code, icd10_patterns$psychostimulant),
        1,
        0
      )
    ) %>%

    # Add Suicide Indicator Flags (Includes Underlying Cause of Death ICD-10 Only) -----
    mutate(
      flag_suicide_all = if_else(
        str_detect(underlying_cod_code, icd10_patterns$suicide_all),
        1,
        0
      ),
      flag_suicide_firearm = if_else(
        str_detect(underlying_cod_code, icd10_patterns$suicide_firearm),
        1,
        0
      ),
      flag_suicide_asphyxia = if_else(
        str_detect(underlying_cod_code, icd10_patterns$suicide_asphyxia),
        1,
        0
      ),
      flag_suicide_poisoning = if_else(
        str_detect(underlying_cod_code, icd10_patterns$suicide_poisoning),
        1,
        0
      ),
      flag_suicide_other = if_else(
        str_detect(underlying_cod_code, icd10_patterns$suicide_other),
        1,
        0
      )
    ) %>%

    # Add Firearm Indicator Flags (Includes Underlying Cause of Death ICD-10 Only) -----
    mutate(
      flag_firearm_all = if_else(
        str_detect(underlying_cod_code, icd10_patterns$firearm_all),
        1,
        0
      ),
      flag_firearm_unintentional = if_else(
        str_detect(underlying_cod_code, icd10_patterns$firearm_unintentional),
        1,
        0
      ),
      flag_firearm_homicide = if_else(
        str_detect(underlying_cod_code, icd10_patterns$firearm_homicide),
        1,
        0
      ),
      flag_firearm_undetermined = if_else(
        str_detect(underlying_cod_code, icd10_patterns$firearm_undetermined),
        1,
        0
      ),
      flag_firearm_suicide = if_else(
        str_detect(underlying_cod_code, icd10_patterns$firearm_suicide),
        1,
        0
      ),
      flag_firearm_legal = if_else(
        str_detect(underlying_cod_code, icd10_patterns$firearm_legal),
        1,
        0
      )
    ) %>%

    # Add Other Indicators (Includes Underlying Cause of Death ICD-10 Only) -----
    mutate(
      flag_malignant_neoplasm = if_else(
        str_detect(underlying_cod_code, icd10_patterns$malignant_neoplasm),
        1,
        0
      ),
      flag_heart_disease = if_else(
        str_detect(underlying_cod_code, icd10_patterns$heart_disease),
        1,
        0
      ),
      flag_alzheimers = if_else(
        str_detect(underlying_cod_code, icd10_patterns$alzheimers),
        1,
        0
      ),
      flag_unintentional_injury = if_else(
        str_detect(underlying_cod_code, icd10_patterns$unintentional_injury),
        1,
        0
      ),
      flag_chronic_lower_respiratory = if_else(
        str_detect(underlying_cod_code, icd10_patterns$chronic_lower_resp),
        1,
        0
      ),
      flag_cerebrovascular_disease = if_else(
        str_detect(underlying_cod_code, icd10_patterns$cerebrovascular),
        1,
        0
      ),
      flag_diabetes = if_else(
        str_detect(underlying_cod_code, icd10_patterns$diabetes),
        1,
        0
      ),
      flag_chronic_liver_disease = if_else(
        str_detect(underlying_cod_code, icd10_patterns$chronic_liver),
        1,
        0
      ),
      flag_influenza_pneumonia = if_else(
        str_detect(underlying_cod_code, icd10_patterns$influenza_pneumonia),
        1,
        0
      ),
      flag_parkinsons = if_else(
        str_detect(underlying_cod_code, icd10_patterns$parkinsons),
        1,
        0
      ),
      flag_essential_hypertension = if_else(
        str_detect(underlying_cod_code, icd10_patterns$essential_hypertension),
        1,
        0
      ),
      flag_pneumonitis = if_else(
        str_detect(underlying_cod_code, icd10_patterns$pneumonitis),
        1,
        0
      ),
      flag_homicide = if_else(
        str_detect(underlying_cod_code, icd10_patterns$homicide),
        1,
        0
      )
    )
}
