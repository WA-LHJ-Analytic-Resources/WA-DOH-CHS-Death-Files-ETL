# Overview
WA local health jurisdictions (LHJs( are provided death certificate data by the WA DOH Center for Health Statistics (CHS) in the Secure Access Washington platform. This repository covers the preparation and use of person level microdata which can be used for in-depth population health analyses from 2010 to present.

## Author(s) & Contributor(s)
- [Tyler Bonnell](mailto:Tyler.Bonnell@co.snohomish.wa.us) (Snohomish County Health Department - Informatics & Data Management Epidemiologist)
- [Jacob Armitage](mailto:jacob.armitage@co.thurston.wa.us) (Thurston County Public Health & Social Services Deparmtent - Assessment & Evaluation Epidemiologist)
- [Neil Panlasigui](mailto:neilp@co.skagit.wa.us) (Skagit County Public Health - Epidemiologist)
- Danny Colombara (Public Health-Seattle King County)
- Jeremy Whitehurst (Public Health-Seattle King County)

## Motivation
The Washington Department of Health (WA DOH) Center for Health Statistics (CHS) provides two sets of death certificate statistical files which have distinct schemas and value-code sets, making it difficult to derive granular insights over extended time periods. This repository aims to address this problem by harmonizing multiple annual data vintages into a single, multi year data set.

| **Year(s)** | **Source**                                        | **Notes**                                                                                                                           |
| ----------- | ------------------------------------------------- |  ----------------------------------------------------------------------------------------------------------------------------------- |
| 1980-2015   | BEDROCK                                  | Only 2010 to 2015 data vintages are available in Secure Access Washington                                                                                                                                    |
| 2016+       | [Washington Health and Life Events System (WHALES)](https://doh.wa.gov/licenses-permits-and-certificates/vital-records/whales) | DOH         | Bedrock to WHALES migration means that BEDROCK (2010-2015) data vintages must have variable names and coded values converted to align with WHALES (2016-Present) schema. |

Currently, the multi-year data set (`harmonized_data`) focuses only on harmonizing multiple years of **finalized** death certificate **statistical** files. This code and workflow could be adapted to incorporate additional types of death certificate files as well as preliminary data if it would be valuable to end users. 

# Workflow - New Users
This workflow details how users can leverage the pre-existing code to generate a new `harmonized_data` file for their teams. 

## Pre-Requisites
1. **All raw WA DOH CHS Death Certificate statistical data files are downloaded from Secure Access Washington and placed in a single folder location**. Preferably, this folder location will only store death data files and not include any documentation-related files. 

## How to Run the Code
0. Open the `.Renviron` file and specify:
    - `RAW_DEATH_FILES_FOLDER` = Where your team stores WA DOH CHS Death Certificate Statistical Files
    - `HARMONIZED_DEATH_FILE_FOLDER` = Where you want `harmonized_data` to be saved (can be the same as `RAW_DEATH_FILES_FOLDER`.)
2. Run `Scripts/0_setup.R`. This script loads all packages and custom functions, defines workflow parameters, and loads critical crosswalks (`Resources/Crosswalks and Schemas`) and code sets (ex: cemetery, country, facility, fips, and more).
3. Run `Scripts/1_harmonize_death_files.R`. This script loads each data vintage and performs the operations (specified below) before returning the single, multiple year data set: `harmonized_data`.
    - **Data Type Harmonization** (all variables as character data types)
    - **Schema Harmonization** (all variables to standardized naming convention)
    - **Value Harmonization (recodes data vintage variable values to a set of standardized code options)**
4. Run `Scripts/2_process_geography_variables.R` This script unifies the disparate death, residence, and injury county and state code variables used across data vintages (most notable being WA codes vs FIPS codes).
5. Run `Scripts/3_clean_harmonized_data.R`. This script:
    - Cleans all ICD-10 code variables
    - Cleans all date and time variables
    - Ensures all variables are converted to their final, desired data type (such as characters to factors with labels)
    - String variables are set to title case
    - `harmonized_data` saved as a [parquet](https://www.r-bloggers.com/2023/11/folks-cmon-use-parquet/) file (optimized for working with large dat asets).

## Updating the Harmonized Data Set
The admins of this repository  will seek to update this repository annual to ensure `harmonized_data` includes the most recently published annual death statistical files released. Instructions to add new data vintages or new variables to `harmonized_data` is available in the `README` excel sheet of `Resources/Crosswalks and Schemas.xlsx`
