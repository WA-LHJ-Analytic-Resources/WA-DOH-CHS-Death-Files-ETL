# investigate_record_axis_code.R
## Note: Investigates any discrepancies between record_axis_code_1 and underlying_cod_code

# harmonized_data -----

TEST <- harmonized_data %>%
  select(
    vintage_label,
    state_file_number,
    local_file_number,
    underlying_cod_code,
    record_axis_code_1
  ) %>%
  filter(underlying_cod_code != record_axis_code_1)

# Raw BEDROCK FILES ----

## 2010
br10 <- readr::read_csv(
  file = death_stat_files %>%
    filter(vintage_label == "BEDROCK_2010") %>%
    pull(file_location),
  col_select = c(certno, cntyfile, underly, mltcse1)
) %>%
  mutate(certno = as.character(certno), file_year = 2010) %>%
  filter(underly != mltcse1)

## 2011
br11 <- readr::read_csv(
  file = death_stat_files %>%
    filter(vintage_label == "BEDROCK_2011") %>%
    pull(file_location),
  col_select = c(CertNo, CntyFile, Underly, mltcse1)
) %>%
  mutate(CertNo = as.character(CertNo), file_year = 2011) %>%
  filter(Underly != mltcse1)

names(br11) <- str_to_lower(names(br11)) # Need to convert variable names to lowercase

## 2012
br12 <- readr::read_csv(
  file = death_stat_files %>%
    filter(vintage_label == "BEDROCK_2012") %>%
    pull(file_location),
  col_select = c(CertNo, CntyFile, Underly, mltcse1)
) %>%
  mutate(CertNo = as.character(CertNo), file_year = 2012) %>%
  filter(Underly != mltcse1)

names(br12) <- str_to_lower(names(br12)) # Need to convert variable names to lowercase


## 2013
br13 <- readr::read_csv(
  file = death_stat_files %>%
    filter(vintage_label == "BEDROCK_2013") %>%
    pull(file_location),
  col_select = c(certno, cntyfile, underly, mltcse1)
) %>%
  mutate(certno = as.character(certno), file_year = 2013) %>%
  filter(underly != mltcse1)


## 2014
br14 <- readxl::read_excel(
  path = death_stat_files %>%
    filter(vintage_label == "BEDROCK_2014") %>%
    pull(file_location)
) %>%
  select(certno, cntyfile, underly, mltcse1) %>%
  mutate(certno = as.character(certno), file_year = 2014) %>%
  filter(underly != mltcse1)


## 2015
br15 <- readxl::read_excel(
  path = death_stat_files %>%
    filter(vintage_label == "BEDROCK_2015") %>%
    pull(file_location)
) %>%
  select(certno, cntyfile, underly, mltcse1) %>%
  mutate(certno = as.character(certno), file_year = 2015) %>%
  filter(underly != mltcse1)

# Combine All ----

br <- bind_rows(br10, br11, br12, br13, br14, br15)
rm(br10, br11, br12, br13, br14, br15)
