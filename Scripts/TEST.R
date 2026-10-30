# TEST.R

# Prepare Test Data -----
TEST <- harmonized_data %>%
  select(
    state_file_number,
    file_year,
    underlying_cod_code,
    all_contributing_cod_code,
    starts_with("flag_")
  )


# Custom Function -----

sample_by_group <- function(df, vars, n = 1) {
  # data: dataframe to sample from
  # vars: character vector of column names used for grouping
  # n: number of samples per group

  df %>%
    select(all_of(vars)) %>%
    slice_sample(n = n, by = all_of(vars)) %>%
    arrange(across(all_of(vars)))
}

# Review -----
# review_list <- list()

# ## All Drugs
# review_list$all_drug <- sample_by_group(
#   df = TEST,
#   vars = c("flag_all_drug", "underlying_cod_code")
# )

# ## Opioids
# review_list$opioids <- sample_by_group(
#   df = TEST,
#   vars = c(
#     "flag_all_drug",
#     "flag_opioids",
#     "underlying_cod_code",
#     "all_contributing_cod_code"
#   )
# )

# View(review_list$opioids)

# ## Heroin
# review_list$heroin <- sample_by_group(
#   df = TEST,
#   vars = c(
#     "flag_all_drug",
#     "flag_heroin",
#     "underlying_cod_code",
#     "all_contributing_cod_code"
#   )
# )

# View(review_list$heroin)

# ## Natural Semisynthetic Opioid
# review_list$natural_semisynthetic_opioid <- sample_by_group(
#   df = TEST,
#   vars = c(
#     "flag_all_drug",
#     "flag_natural_semisynthetic_opioid",
#     "underlying_cod_code",
#     "all_contributing_cod_code"
#   )
# )

# View(review_list$natural_semisynthetic_opioid)

# ## Methadone
# review_list$methadone <- sample_by_group(
#   df = TEST,
#   vars = c(
#     "flag_all_drug",
#     "flag_methadone",
#     "underlying_cod_code",
#     "all_contributing_cod_code"
#   )
# )

# View(review_list$methadone)

# ## Synthetic Opioid
# review_list$synthetic_opioid <- sample_by_group(
#   df = TEST,
#   vars = c(
#     "flag_all_drug",
#     "flag_synthetic_opioid",
#     "underlying_cod_code",
#     "all_contributing_cod_code"
#   )
# )

# View(review_list$synthetic_opioid)

# ## Cocaine
# review_list$cocaine <- sample_by_group(
#   df = TEST,
#   vars = c(
#     "flag_all_drug",
#     "flag_cocaine",
#     "underlying_cod_code",
#     "all_contributing_cod_code"
#   )
# )

# View(review_list$cocaine)

# ## Prescription Opioid
# review_list$prescription_opioid <- sample_by_group(
#   df = TEST,
#   vars = c(
#     "flag_all_drug",
#     "flag_prescription_opioid",
#     "underlying_cod_code",
#     "all_contributing_cod_code"
#   )
# )

# View(review_list$prescription_opioid)

# ## Psychostimulant
# review_list$psychostimulant <- sample_by_group(
#   df = TEST,
#   vars = c(
#     "flag_all_drug",
#     "flag_psychostimulant",
#     "underlying_cod_code",
#     "all_contributing_cod_code"
#   )
# )

# View(review_list$psychostimulant)

# ## Malignant Neoplasm
# review_list$malignant_neoplasm <- sample_by_group(
#   df = TEST,
#   vars = c(
#     "flag_malignant_neoplasm",
#     "underlying_cod_code"
#   )
# )

# View(review_list$malignant_neoplasm)

# ## Chronic Lower Respiratory
# review_list$chronic_lower_respiratory <- sample_by_group(
#   df = TEST,
#   vars = c(
#     "flag_chronic_lower_respiratory",
#     "underlying_cod_code"
#   )
# )

# View(review_list$chronic_lower_respiratory)

# Secondary Review -----
## Note: This secondary review will compare the 0/1 indicator labels for each condition within the harmonized data file vs WA DOH's Annual Data Vintage

## Filter Test Harmonized Data
TEST_year <- TEST %>%
  filter(file_year == 2024)

## Load in 2024 Death Statistical File Data
dat_year <- read_csv(
  file = "S:/Prevention Services/CH/Assessment/CHS_BirthsDeaths/Deaths/Data/DeathStatF2024.csv",
  col_select = c(
    `State File Number`,
    `Underlying COD Code`,
    `All Drugs`,
    `Opioids`,
    `Heroin`,
    `Natural Semisynthetic Opioid`,
    `Methadone`,
    `Synthetic Opioid`,
    `Cocaine`,
    `Prescription Opioid`,
    `Psychostimulant`,
    `Suicide All`,
    `Suicide Firearm`,
    `Suicide Asphyxia`,
    `Suicide Poisoning`,
    `Suicide Other`,
    `Firearm All`,
    `Firearm Unintentional`,
    `Firearm Homicide`,
    `Firearm Undetermined`,
    `Firearm Legal`,
    `Malignant Neoplasm`,
    `Heart Disease`,
    `Alzheimers`,
    `Unintentional Injury`,
    `Chronic Lower Respiratory`,
    `Cerebrovascular Disease`,
    `Diabetes`,
    `Chronic Liver Disease`,
    `Influenza Pneumonia`,
    `Parkinsons`,
    `Essential Hypertension`,
    `Pneumonitis`,
    `Homicide`
  )
) %>%
  janitor::clean_names() %>%
  mutate(state_file_number = as.character(state_file_number))

## Join Test Harmonized Data & WA DOH Annual Data Vintage
combined <- TEST_year %>%
  left_join(
    .,
    dat_year %>% select(-underlying_cod_code),
    by = join_by(state_file_number)
  )


## Compare 0/1 Indicator Outputs

### Identify all flag_ variables
flag_vars <- names(combined)[str_detect(names(combined), "^flag_")]

### Compute partner variable names (remove 'flag_')
partner_vars <- str_remove(flag_vars, "^flag_")

### Keep only pairs where the non-flag version actually exists in the data frame
valid_pairs <- tibble(
  flag_var = flag_vars,
  partner_var = partner_vars
) %>%
  filter(partner_var %in% names(combined))

### Create TRUE/FALSE _match variables for each indicator variable
comparison_df <- combined %>%
  mutate(
    !!!set_names(
      map2(
        valid_pairs$flag_var,
        valid_pairs$partner_var,
        ~ combined[[.x]] == combined[[.y]] # <-- FIXED
      ),
      paste0(valid_pairs$partner_var, "_match")
    )
  )

## Results of  0/1 Indicator Comparisons
# If not explicitly mentioned, 100% agreement between data sources

## 2024 (n = 67,141)
# 1) flag_opioids --> 5 record mismatch (Appear to be false positive flagging on T40.6/T406 ICD-10 code; "2024005223" "2024018781" "2024031932" "2024037361" "2024062492")
AAA <- comparison_df %>%
  filter(opioids_match == FALSE) %>%
  select(
    state_file_number,
    opioids,
    flag_opioids,
    underlying_cod_code,
    all_contributing_cod_code
  )
# 2) flag_suicide_other --> 18 record mismatch (Appear to be false negatives related to X62.0 and Y87.0; "2024003129" "2024006110" "2024007805" "2024016224" "2024017447" "2024017851" "2024019197" "2024023607" "2024024402", "2024024627" "2024027551" "2024034510" "2024035328" "2024036588" "2024044548" "2024046443" "2024052758" "2024060155")
AAA <- comparison_df %>%
  filter(suicide_other_match == FALSE) %>%
  select(
    state_file_number,
    suicide_other,
    flag_suicide_other,
    underlying_cod_code
  )

# 3) flag_unintentional_injury --> 1654 record mismatch --> RESOLVED; was missing W00-W99 in regex logic
# 4) flag_cerebrovascular_disease --> 4,640 record mismatch --> RESOLVED; was accidentally using chronic liver disease regex definition
# 5) flag_homicide --> 13 record mismatch (Appear to be false negatives related to Y00.0, Y02.0, Y04.0, Y07.9, Y08.0; "2024009412" "2024012751" "2024018664" "2024028566" "2024032717" "2024035053" "2024039682" "2024048295" "2024050453", "2024052815" "2024061249" "2024064033" "2024091337")
AAA <- comparison_df %>%
  filter(homicide_match == FALSE) %>%
  select(
    state_file_number,
    homicide,
    flag_homicide,
    underlying_cod_code
  )


## 2023 (n = 67,513)
# 1) flag_opioids --> 7 record mismatch (False positives related to T40.6 - Other and unspecified narcotics: "2023002304" "2023003070" "2023013772" "2023014363" "2023022854" "2023031346" "2023032647")
AAA <- comparison_df %>%
  filter(opioids_match == FALSE) %>%
  select(
    state_file_number,
    opioids,
    flag_opioids,
    underlying_cod_code,
    all_contributing_cod_code
  )
# 2) flag_suicide_other --> 25 record mismatch (False Negatives related to soley X62.0; "2023001754" "2023003969" "2023005561" "2023011415" "2023013947" "2023022017" "2023022496" "2023022673" "2023031048")
AAA <- comparison_df %>%
  filter(suicide_other_match == FALSE) %>%
  select(
    state_file_number,
    suicide_other,
    flag_suicide_other,
    underlying_cod_code
  )

# 3) flag_homicide --> 10 record mismatch (False Negatives related to Y00.0, Y03.0, Y04.0, Y06.9, Y08.0)
AAA <- comparison_df %>%
  filter(homicide_match == FALSE) %>%
  select(
    state_file_number,
    homicide,
    flag_homicide,
    underlying_cod_code
  )


## 2022 (n = 70,704)

## 1) flag_opioids --> 11 record mismatch (False Positives T40.6; "2022002115" "2022010043" "2022012667" "2022014437" "2022017876" "2022022411" "2022032452" "2022032547" "2022034237", "2022054144" "2022069234")
AAA <- comparison_df %>%
  filter(opioids_match == FALSE) %>%
  select(
    state_file_number,
    opioids,
    flag_opioids,
    underlying_cod_code,
    all_contributing_cod_code
  )
## 2) flag_suicide_other --> 15 record mismatch (False Negatives X62.0; "2022000605" "2022005338" "2022007506" "2022012063" "2022013346" "2022018427" "2022019537" "2022027224" "2022030981","2022032452" "2022035806" "2022053668" "2022060591" "2022061282" "2022064970")
AAA <- comparison_df %>%
  filter(suicide_other_match == FALSE) %>%
  select(
    state_file_number,
    suicide_other,
    flag_suicide_other,
    underlying_cod_code
  )
## 3) flag_homicide --> 22 record mismatch
AAA <- comparison_df %>%
  filter(homicide_match == FALSE) %>%
  select(
    state_file_number,
    homicide,
    flag_homicide,
    underlying_cod_code
  )
