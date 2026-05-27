# 2_process_geography_variables.R

# 1) Clean WA Codes (Zero Pad) ----

harmonized_data <- harmonized_data %>%
  mutate(across(
    c(residence_county_wa_code, death_county_wa_code, injury_county_wa_code),
    ~ str_pad(
      .,
      width = 2,
      side = "left",
      pad = "0"
    )
  ))


# 2) Translate WA Codes to FIPS Codes (to fill in missing FIPS) -----

## 2a) county_code_pairs specifies each WA Code & FIPS code variable pairing, along with the year_threshold (that indicates what years to use WA Codes to fill in missing FIPS codes)
county_code_pairs <- list(
  list(
    wa_col = "death_county_wa_code",
    fips_col = "death_county_fips",
    year_threshold = 2022
  ),
  list(
    wa_col = "residence_county_wa_code",
    fips_col = "residence_county_fips",
    year_threshold = 2016
  ),
  list(
    wa_col = "injury_county_wa_code",
    fips_col = "injury_county_fips",
    year_threshold = 2022
  )
)

## 2b) Use county_code_pairs & params$code_sets$wa_county_code_to_fips to translate WA Codes (before year_threshold) t
## to fill in missing FIPS codes (for death_county_fips, residence_county_fips, and injury_county_fips)

harmonized_data <- county_code_pairs %>%
  reduce(
    function(data, pair) {
      county_wa_code_to_fips(
        data,
        pair$wa_col,
        pair$fips_col,
        pair$year_threshold
      )
    },
    .init = harmonized_data
  )

## 2c) Convert translated WA Codes --> FIPS Codes with a value of "00" to NA
## Additional details: '00' WA Code values = "Out of State or Unknown" (impossible to distinguish) --> Convert to NA

harmonized_data <- harmonized_data %>%
  ### For death and injury county columns
  mutate(
    across(
      .cols = c(death_county_fips, injury_county_fips),
      .fns = ~ case_when(
        file_year < 2022 & .x == "00" ~ NA_character_,
        TRUE ~ as.character(.x)
      )
    ),
    ### For residence county column
    residence_county_fips = case_when(
      file_year < 2016 & residence_county_fips == "00" ~ NA_character_,
      TRUE ~ as.character(residence_county_fips)
    )
  )

# 3) Translate FIPS Codes to Literals -----

## 3a) county_label_pairs specifies each FIPS code & Literal variable pairing, along with the year_threshold (that indicates what years to use WA Codes to fill in missing Literal values)
## Additional details: Backfill residence_county & injury_county columns (for 2010-2015, which are missing) using FIPS codes.

county_label_pairs <- list(
  list(
    fips_col = "residence_county_fips",
    literal_col = "residence_county",
    year_threshold = 2016
  ),
  list(
    fips_col = "injury_county_fips",
    literal_col = "injury_county",
    year_threshold = 2016
  )
)

## 3b) Use county_label_pairs & params$code_sets$wa_county_code_to_fips to translate FIPS Codes (before year_threshold)
## to fill in missing Literal Values (for residence_county & injury_county)

harmonized_data <- county_label_pairs %>%
  reduce(
    function(data, pair) {
      county_fips_to_literals(
        data,
        pair$fips_col,
        pair$literal_col,
        pair$year_threshold
      )
    },
    .init = harmonized_data
  )

# 4) Clean Death FIPS Variables -----

## 4a) Adjust death_county_fips number padding
## Additional details: In 2022-2023 this column had many different number sequences padding the front of the 3-character county FIPS code. A
## Additionally, county FIPS code were sometimes entered with no number padding but with missing zeros. Examples: "009" is input as "9" or "019" as "19"

harmonized_data <- harmonized_data %>%
  mutate(
    death_county_fips = case_when(
      file_year %in% 2022:2023 ~ str_pad(
        str_sub(death_county_fips, -3, -1), # extract last 3 characters
        width = 3, # make sure length is 3 by padding with zero
        side = "left",
        pad = "0"
      ),
      TRUE ~ as.character(death_county_fips)
    )
  )

## 4b) Harmonize death_state coding
# Additional details: During the 2010-2015 "48" (used for Washington), 2016-onwards "WASHINGTON" is used. Convert "48" --> "WASHINGTON"

harmonized_data <- harmonized_data %>%
  mutate(
    death_state = case_when(
      file_year %in% 2010:2015 & death_state == "48" ~ "WASHINGTON",
      TRUE ~ as.character(death_state)
    )
  )

# 5) Clean Residence FIPS Variables -----

## 5a) Set Out of State values for FIPS Residence County to "00" for years 2016 onwards
## Additional details: Pre 2016 Crosswalked FIPS Residence County codes do not include Out of State codes.
## For 2016-2023, the data did include OOS codes, but they used 5-character FIPS codes with leading state 2 digit prefixes.
## For 2024 and onwards, FIPS codes were reduced to 3-character codes. These are not unique when Out of State values are present.

## Note: Information about Out of State residences geography can still be accessed with literal columns (via residence_county & residence_state_fips_code combined logic)

harmonized_data <- harmonized_data %>%
  mutate(
    residence_county_fips = case_when(
      file_year >= 2016 &
        residence_state_fips_code != "WA" ~ "00", # For 2016-onwards, set OOS residence_county_fips to '00'
      TRUE ~ as.character(residence_county_fips)
    )
  )


## 5b) Make sure residence county fips has padding "0"s to reach length = 3
## Additional details: after setting out of State values to "00", there are many two charcter values that need a leading 0 and some single digit values that need two leading 00s.

harmonized_data <- harmonized_data %>%
  mutate(
    residence_county_fips = case_when(
      file_year >= 2016 & residence_county_fips != "00" ~ str_pad(
        #ignore out of State "00" coding
        residence_county_fips,
        width = 3,
        side = "left",
        pad = "0"
      ),
      TRUE ~ as.character(residence_county_fips)
    )
  )

## 5c) Drop leading '53' (WA prefix) from FIPS residence_county_fips values for file years: 2016-2023
## Additional details: Some inputted values have a leading "53". Crosswalked FIPS codes do not have leading 53 and 2024 and onwards will not have leading 53.
## Project team decided to drop the leading prefix to infer a strict 3 digit residence_county_fips format (with OOS values being '00')

harmonized_data <- harmonized_data %>%
  mutate(
    across(
      .cols = residence_county_fips,
      .fns = ~ case_when(
        file_year %in% 2016:2023 ~ str_remove(.x, "^53"),
        TRUE ~ as.character(.x)
      )
    )
  )

## 5d) Fix Residence State FIPS code for file years: 2010-2015
## Additional details: During the 2010-2015 "48" was used. During 2016 onwards, "WA" used for residence_state_fips_code. Crosswalk "48" --> "WA".

harmonized_data <- harmonized_data %>%
  mutate(
    residence_state_fips_code = case_when(
      file_year %in% 2010:2015 & residence_state_fips_code == "48" ~ "WA",
      TRUE ~ as.character(residence_state_fips_code)
    )
  )


# 6) Clean Injury FIPS Variables -----

## 6a) Reformat Injury County FIPS column padding and missingness
## Additional details: Out of state injuries will be assigned value "00" because 3-character FIPS codes are not unique with Out of State values present in column.
## Additionally, random number padding will be removed and replaced with padding using "0"s to ensure length = 3 for FIPS codes entered as two or single character codes.
## Lastly, we will be assigning NA values to column based on literal column (injury_county & injury_state) values.

## Note: Information about Out of State injuries can still be accessed using literal columns (via injury_county & injury_state combined logic)

harmonized_data <- harmonized_data %>%
  mutate(
    injury_county_fips = case_when(
      file_year %in%
        2022:2023 &
        is.na(injury_state) &
        is.na(injury_county) ~ NA_character_,
      file_year %in%
        2022:2023 &
        injury_state == "WASHINGTON" &
        is.na(injury_county) ~ NA_character_,
      file_year %in% 2022:2023 & injury_state != "WASHINGTON" ~ "00",
      TRUE ~ as.character(injury_county_fips)
    ),
    injury_county_fips = case_when(
      file_year %in% 2022:2023 & injury_county_fips != "00" ~ str_pad(
        # ignore out of state special coding
        str_sub(injury_county_fips, -3, -1), # extract last 3 characters
        width = 3, # make sure length is 3 by padding with zeros for values entered as single or two length strings
        side = "left",
        pad = "0"
      ),
      TRUE ~ as.character(injury_county_fips)
    )
  )

## 6b) Reformat 2024 and onward Injury County FIPS Column ----
## Additional details: Since 3-character FIPS codes are not unique we will code out of State counties to "00"

harmonized_data <- harmonized_data %>%
  mutate(
    injury_county_fips = case_when(
      file_year > 2023 & injury_state != "WASHINGTON" ~ "00",
      TRUE ~ as.character(injury_county_fips)
    )
  )
