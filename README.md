# Overview
WA LHJs are provided death certificate data by the WA DOH Center for Health Statistics (CHS) in the Secure Access Washington platform. This repository covers the preparation and use of person level microdata which can be used for in-depth population health analyses from 2010 to present.

## Motivation
WA DOH CHS provides two sets of death certificate data vintages: 1) **2010-2015** (sourced from the Bedrock system) and 2) **2016-present** (sourced from the WHALES or the Washington Health and Life Event system). These sets of data vintages have distinct schemas and value-code sets, making it difficult to derive granular insights over extended time periods. This repository aims to address this problem by generating a data pipeline that integrates Bedrock and Whales data vintages into a single harmonized data set.

## Author(s) & Contributor(s)
- [Tyler Bonnell](mailto:Tyler.Bonnell@co.snohomish.wa.us) (Snohomish County Health Department - Informatics & Data Management Epidemiologist)
- [Jacob Armitage](mailto:jacob.armitage@co.thurston.wa.us) (Thurston County Public Health & Social Services Deparmtent - Assessment & Evaluation Epidemiologist(

# Data Processing / ETL

## Data inputs

| **Year(s)** | **Source**                                        | **Contact** | **Notes**                                                                                                                           |
| ----------- | ------------------------------------------------- | ----------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| 1980-2015   | BEDROCK                                  | DOH         | Only 2010 to 2015 data vintages are available in Secure Access Washington                                                                                                                                    |
| 2016+       | Washington Health and Life Events System (WHALES) | DOH         | Bedrock --> WHALES migration means that BEDROCK data vintages must have variable names and coded values converted to align with WHALES schema. |

- STAT - This data contains nearly all of the demographic information about the decedent, dates, codes for causes of death, and other intent and mechanism information about the death.
- GEO - This data contains lattitude and longitude information that is then geocoded to blocks, school districts, zip codes, and other geographic identifiers.
- NAMES - This data contains full names, SSNs, and full address of the decedent.
- LITERAL - This data contains full text descriptions for causes of death.

## Data Avalabiity

Multiple versions of the data sets are sent throughout the year. There are preliminary and final versions of the data. Preliminary data will come in the form of quarterly (Q1, Q2, Q3, Q4) and then several less descriptive versions (Q5, Q6, P). Q5 and Q6 are typically not adding new rows but filling in or updating columns in already existing rows. P is usually the last preliminary and most complete file before the final file (F) is released.

## Data Inputs

## Data inputs

| **Data** | **Location** | **Last Update** | **Notes** |
|----------|--------------|-----------------|-----------|
| Raw Death Statistical Files | **User Determined** in `.Renviron` file | 2026-03-02 | Users provide the filepath to folder containing all of your organization's raw death statistical files (downloaded from Secure Access Washington) |
| Variable Name Crosswalk | `Resources/Crosswalks/1_Schema_Harmonization/variable_name_crosswalk.csv` | 2026-03-02 | Crosswalks the BEDROCK variable names to WHALES variable names. All variable names are cleaned and standardized using [`janitor::clean_names()`](https://cloud.r-project.org/web/packages/janitor/vignettes/janitor.html#clean-dataframe-names-with-clean_names) which converts variable names to lowercase, replaces spaces with "_", and more. |
| Variable Code Crosswalk | `Resources/Crosswalks/3_Value_Harmonization/variable_code_crosswalk.csv` | 2026-03-02 | (Applied after `Variable Name Crosswalk`) Crosswalks the value-code sets used by BEDROCK variables and converts them to/aligns them with WHALES value-code sets.|
| Standardized Code Sets | `Resources/Crosswalks/4_Code_Set_Expansion` | 2026-03-02 | (Applied after `Variable Code Crosswalk`) Contains the standardized value-code sets for geographies and facilities mentioned in the death files. This includes: NCHS state & county codes, WA county & city-county codes, FIPs codes, country codes, facility codes, funeral home codes, cemetery codes, and occupation codes. These code sets were extracted from WA DOH Death Statistical Dictionary & Crosswalks.xlsx and put into a machine-readable format.|
| WA DOH Death Statistical Dictionary & Crosswalks.xlsx | **Download from Secure Access Washington** | 2026-03-02 | Documents variable name and variable code crosswalks between BEDROCK and WHALES as well as provides standarized code sets. Not all variable code crosswalks are explictly mentioned in this document (some additional crosswalks are mentioned in the WA DOH provided STATA .do file). |
| DthStatFile_ConvertToOldVarNames_import.do | **Download from Secure Access Washington** | 2026-03-02 | This STATA (.do) script details how WHALES variable names and variable codes can be crosswalked backed to BEDROCK format (WHALES --> BEDROCK). It includes some crosswalks that are NOT explictly mentioned in the WA DOH Death Statstical Dictionary & Crosswalks.xlsx. This script provides some crosswalk information, albeit in the opposite direction intended for this project.|
| [WA DOH Death Data User Guide](https://doh.wa.gov/sites/default/files/2024-10/422-155-WADeathFileDataUsersGuide2023_1.pdf) | Publicly Available (see link to the left) | 2026-03-02 | Provides description on how to use the WA DOH CHS death microdata |
