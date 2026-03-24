# clean_time_variables.R

clean_time_variables <- function(df) {
  df_formatted <- df %>%
    mutate(
      # 0) Normalize Hour/Minute Variables (ensure they are in proper ranges)
      ## Hour Validation
      across(
        c(
          time_of_death_hour,
          time_of_injury_hour
        ),
        ~ ifelse(as.numeric(.x) >= 24, NA_character_, .x)
      ),
      ## Minute Validation
      across(
        c(
          time_of_death_minutes,
          time_of_injury_minutes
        ),
        ~ ifelse(as.numeric(.x) >= 60, NA_character_, .x)
      ),

      # 1) Pad Hour/Minute Variables with "0"
      across(
        c(
          time_of_death_hour,
          time_of_death_minutes,
          time_of_injury_hour,
          time_of_injury_minutes
        ),
        ~ str_pad(.x, width = 2, side = 'left', pad = "0")
      ),

      # 2) Create Alternative Time of Death & Time of Injury variables (time_of_death_alt, time_of_injury_alt), will be used as a replacement if time_of_death or time_of_injury respectively are NA
      time_of_death_alt = ifelse(
        !is.na(time_of_death_hour) & !is.na(time_of_death_minutes),
        paste0(time_of_death_hour, time_of_death_minutes),
        NA_character_
      ),
      time_of_injury_alt = ifelse(
        !is.na(time_of_injury_hour) & !is.na(time_of_injury_minutes),
        paste0(time_of_injury_hour, time_of_injury_minutes),
        NA_character_
      ),

      # 3) Coalesce Time of Death & Time of Injury variables with alternates
      time_of_death = coalesce(time_of_death_alt),
      time_of_injury = coalesce(time_of_injury_alt),

      # 4) Normalize inputs: treat "NANA", "9999" and blanks as NA
      across(
        c(
          time_of_death,
          time_of_injury
        ),
        ~ ifelse(.x %in% c("NANA", "9999", ""), NA_character_, .x)
      ),

      # 5) Normalize "2400" timestamp to "0000"
      across(
        c(
          time_of_death,
          time_of_injury
        ),
        ~ ifelse(.x == "2400", "0000", .x)
      ),

      # 6) Parse Time of Death & Time of Injury variables to time data type
      time_of_death_final = readr::parse_time(time_of_death, format = "%H%M"),
      time_of_injury_final = readr::parse_time(time_of_injury, format = "%H%M")
    ) %>%
    # 7) Remove unnecessary variables
    select(
      -time_of_death_alt,
      -time_of_injury_alt,
      -time_of_death,
      -time_of_death_hour,
      -time_of_death_minutes,
      -time_of_injury,
      -time_of_injury_hour,
      -time_of_injury_minutes,
    ) %>%
    # 8) Rename variables
    rename(
      time_of_death = time_of_death_final,
      time_of_injury = time_of_injury_final
    )

  return(df_formatted)
}
