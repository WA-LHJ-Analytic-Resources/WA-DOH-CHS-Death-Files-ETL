# Overview
WA LHJs are provided death certificate data by the WA DOH Center for Health Statistics (CHS) in the Secure Access Washington platform. This repository covers the preparation and use of person level microdata which can be used for in-depth population health analyses from 2010 to present.

## Author(s) & Contributor(s)
- [Tyler Bonnell](mailto:Tyler.Bonnell@co.snohomish.wa.us) (Snohomish County Health Department - Informatics & Data Management Epidemiologist)
- [Jacob Armitage](mailto:jacob.armitage@co.thurston.wa.us) (Thurston County Public Health & Social Services Deparmtent - Assessment & Evaluation Epidemiologist)
- [Neil Panlasigui](mailto:neilp@co.skagit.wa.us) (Skagit County Public Health - Epidemiologist)

## Motivation
WA DOH CHS provides two sets of death certificate data vintages which have distinct schemas and value-code sets, making it difficult to derive granular insights over extended time periods. This repository aims to address this problem by generating a data pipeline that integrates the various data vintages into a single harmonized data set.

| **Year(s)** | **Source**                                        | **Contact** | **Notes**                                                                                                                           |
| ----------- | ------------------------------------------------- | ----------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| 1980-2015   | BEDROCK                                  | DOH         | Only 2010 to 2015 data vintages are available in Secure Access Washington                                                                                                                                    |
| 2016+       | Washington Health and Life Events System (WHALES) | DOH         | Bedrock --> WHALES migration means that BEDROCK data vintages must have variable names and coded values converted to align with WHALES schema. |

- **STAT** - This data contains nearly all of the demographic information about the decedent, dates, codes for causes of death, and other intent and mechanism information about the death.
- **GEO** - This data contains lattitude and longitude information that is then geocoded to blocks, school districts, zip codes, and other geographic identifiers.
- **NAMES** - This data contains full names, SSNs, and full address of the decedent.
- **LITERAL** - This data contains full text descriptions for causes of death.

Multiple versions of the data sets are sent throughout the year. There are preliminary and final versions of the data. Preliminary data will come in the form of quarterly (Q1, Q2, Q3, Q4) and then several less descriptive versions (Q5, Q6, P). Q5 and Q6 are typically not adding new rows but filling in or updating columns in already existing rows. P is usually the last preliminary and most complete file before the final file (F) is released.

# Data Processing Workflow

## Pre-Requisites
1. **All raw WA DOH CHS Death Certificate data files be downloaded from Secure Access Washington and placed in a single folder location**. Preferrably, this folder location will only store death data files, and not include any documentation-related files. 

## How to Run the Code
1. Download all WA DOH CHS Death Certificate Statistical Files from Secure Access Washington.
2. Run `Scripts/0_setup.R`. This script prompts users to edit `.Renviron` and specify where downloaded files from Step #1 are stored.
3. Run `Scripts/1_crosswalk_death_files.R`. This script performs the 1_Schema Harmonization, 2_Data Type Harmonization, 3) Value Harmonization steps, and takes approximately 10 minutes to complete for all available death statistical files.
4. Run `Scripts/2_clean_harmonized_data.R`. This script cleans the harmonized death data set (i.e. converts to proper data types, performs joins with code set descriptions, etc.)

### Workflow Diagram
![WA DOH CHS Death Files ETL Workflow Diagram](Resources/WA-DOH-CHS_Death-Files-ETL-Workflow-Diagram.png)

## Workflow Inputs

| **Data** | **Location** | **Last Update** | **Notes** |
|----------|--------------|-----------------|-----------|
| Raw Death Statistical Files | **User Determined** in `.Renviron` file | 2026-03-02 | Users provide the filepath to folder containing all of your organization's raw death statistical files (downloaded from Secure Access Washington) |
| Variable Name Crosswalk | `Resources/Crosswalks/1_Schema_Harmonization/variable_name_crosswalk.csv` | 2026-03-02 | Crosswalks the BEDROCK variable names to WHALES variable names. All variable names are cleaned and standardized using [`janitor::clean_names()`](https://cloud.r-project.org/web/packages/janitor/vignettes/janitor.html#clean-dataframe-names-with-clean_names) which converts variable names to lowercase, replaces spaces with "_", and more. |
| Variable Code Crosswalk | `Resources/Crosswalks/3_Value_Harmonization/variable_code_crosswalk.csv` | 2026-03-02 | (Applied after `Variable Name Crosswalk`) Crosswalks the value-code sets used by BEDROCK variables and converts them to/aligns them with WHALES value-code sets.|
| Standardized Code Sets | `Resources/Crosswalks/4_Code_Set_Expansion` | 2026-03-02 | (Applied after `Variable Code Crosswalk`) Contains the standardized value-code sets for geographies and facilities mentioned in the death files. This includes: NCHS state & county codes, WA county & city-county codes, FIPs codes, country codes, facility codes, funeral home codes, cemetery codes, and occupation codes. These code sets were extracted from WA DOH Death Statistical Dictionary & Crosswalks.xlsx and put into a machine-readable format.|
| Custom R Functions | `Scripts/Custom_Functions` | 2026-03-02 | Custom R functions were developed to streamline and increase the legibility this data pipeline. |
| [WA DOH Death Data User Guide](https://doh.wa.gov/sites/default/files/2024-10/422-155-WADeathFileDataUsersGuide2023_1.pdf) | Publicly Available (see link to the left) | 2026-03-02 | Provides description on how to use the WA DOH CHS death microdata |

### Custom R Functions
There are multiple custom R functions stored in separate R scripts at `Scripts/Custom_Functions`, including a few critical ones that are nested within each other. The 4 custom functions below are ordered from smallest/most granular to largest/most flexible: 

- [`apply_crosswalk_var()`](https://github.com/WA-LHJ-Analytic-Resources/WA-DOH-CHS-Death-Files-ETL/blob/main/Scripts/Custom_Functions/apply_crosswalk_var.R) - This function takes **one variable** from a BEDROCK data vintage, and crosswalks it to use the corresponding coded values from the WHALES data vintages.
- [`apply_crosswalk()`](https://github.com/WA-LHJ-Analytic-Resources/WA-DOH-CHS-Death-Files-ETL/blob/main/Scripts/Custom_Functions/apply_crosswalk.R) - This function takes a BEDROCK data vintage, and crosswalks **all applicable variables** to use the corresponding coded values from the WHALES data vintages. `apply_crosswalk()` works by iteratively using `apply_crosswalk_var()` over all applicable variables.
- [`harmonize_vintage()`](https://github.com/WA-LHJ-Analytic-Resources/WA-DOH-CHS-Death-Files-ETL/blob/main/Scripts/Custom_Functions/harmonize_vintage.R) - Performs the entire ETL data pipeline (load data, 1 - Schema Harmonization, 2 - Data Type Harmonization, 3 - Value Harmonization (via `apply_crosswalk()`) for **one BEDROCK or WHALES data vintage file**.
- [`harmonize_all_vintages()`](https://github.com/WA-LHJ-Analytic-Resources/WA-DOH-CHS-Death-Files-ETL/blob/main/Scripts/Custom_Functions/harmonize_all_vintages.R) - Performs the entire ETL data pipeline (load data, 1 - Schema Harmonization, 2 - Data Type Harmonization, 3 - Value Harmonization (via `apply_crosswalk()`) for **all identified BEDROCK or WHALES data vintage files**. `harmonize_all_vintages()` works by iteratively using `harmonize_vintage()` for all identified data vintage files. 


