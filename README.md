# Overview
WA LHJs are provided death certificate data by the WA DOH Center for Health Statistics (CHS) in the Secure Access Washington platform. This repository covers the preparation and use of person level microdata which can be used for in-depth population health analyses from 2010 to present.

## Author(s) & Contributor(s)
- [Tyler Bonnell](mailto:Tyler.Bonnell@co.snohomish.wa.us) (Snohomish County Health Department - Informatics & Data Management Epidemiologist)
- [Jacob Armitage](mailto:jacob.armitage@co.thurston.wa.us) (Thurston County Public Health & Social Services Deparmtent - Assessment & Evaluation Epidemiologist)
- [Neil Panlasigui](mailto:neilp@co.skagit.wa.us) (Skagit County Public Health - Epidemiologist)
- Danny Colombara (Public Health-Seattle King County)
- Jeremy Whitehurst (Public Health-Seattle King County)

## Motivation
The Washington Department of Health (WA DOH) Center for Health Statistics (CHS) provides two sets of death certificate statistical files which have distinct schemas and value-code sets, making it difficult to derive granular insights over extended time periods. This repository aims to address this problem by harmonizing multiple annual data vintages into a single, multi year data set.

| **Year(s)** | **Source**                                        | **Contact** | **Notes**                                                                                                                           |
| ----------- | ------------------------------------------------- | ----------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| 1980-2015   | BEDROCK                                  | DOH         | Only 2010 to 2015 data vintages are available in Secure Access Washington                                                                                                                                    |
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

# Workflow - Admin
This workflow details how repository administrators can adapt the code to accomodate new/future annual death statistical file vintages into `harmonized_data`.


### Workflow Diagram
![WA DOH CHS Death Files ETL Workflow Diagram](Resources/WA-DOH-CHS_Death-Files-ETL-Workflow-Diagram.png)

## Workflow Inputs

| **Data** | **Location** | **Last Update** | **Notes** |
|----------|--------------|-----------------|-----------|
| Raw Death Statistical Files | **User Determined** in [`.Renviron`](https://docs.posit.co/ide/user/ide/guide/environments/r/managing-r.html#renviron) file (`RAW_DEATH_FILES_FOLDER`) | 7/21/2026 | Users provide the file path to folder containing all of your organization's raw death statistical files (downloaded from Secure Access Washington) |
| `recode_variables_YYYY.csv` | `Resources/Crosswalks/YYYY` | 3/26/2026 | **Crosswalks data vintage variable coded values** to the standardized variable coding convention for `harmonized_data`. `recode_variables_YYYY.csv` are an **annual** file (as there are year-over-year variations in coding conventions), and **only include variable code-description value pairs that deviate from the standardized coding convention (i.e., if a variable's code-description value pair does not require recoding it is not included)**. | 
| `schema_data_types.csv` | `Resources/Schemas` | 3/26/2026 | Indicates the proper, finalized data types for all variables in `harmonized_data`. This is used to convert `harmonized_data` variables from character data type (used throughout the data pipeline) to intended data types for end use (such as dates, times, factors, and more). | 
| `schema_factors.csv` | `Resources/Schemas` | 3/26/2026 | This file provides all of the desired levels and labels for `harmonized_data`'s factor and ordered (factor) variables. | 

## Workflow Outputs 
| **Data** | **Location** | **Last Update** | **Notes** |
|----------|--------------|-----------------|-----------|
| `harmonized_data.parquet` | **User Determined** in `.Renviron` file (`HARMONIZED_DEATH_FILE_FOLDER`) | 7/21/2026 | A multi-year death statstical file (2010 to Present) containing a subset of frequently used variables. | 
| `Data Dictionary - Harmonized Data.xlsx` | `Resources/Schemas` | 8/3/2026 | A **Data Dictionary** describing the `variable(s)`, `class(es)`, `n_missing` (missing rows), `pct_missing` (missing rows/total rows x 100), `n_distinct` (distinct values in the variable), and `values` (a summary of all possible answer values for each `variable`, excludes values for unique identifier and/or high cardinality variables). | 
| `all_recode_variables.csv` | `Resources/Crosswalks` | 3/26/2026 | Uses `Scripts/crosswalk_review.R` to combines all annual `code_variables_YYYY.csv` files into 1 single file (`all_recode_variables.xlsx`) to enable quick, user review of variable recoding crosswalks across all data vintages.| 
| `all_rename_variables.csv` | `Resources/Crosswalks` | 3/26/2026 | Uses `Scripts/crosswalk_review.R` to combines all annual `rename_variables_YYYY.csv` files into 1 single file (`all_rename_variables.xlsx`) to enable quick, user review of variable renaming crosswalks across all data vintages. | 
| `all_recode_variables.csv` | `Resources/Crosswalks` | 3/26/2026 | Uses `Scripts/crosswalk_review.R` to combines all annual `code_variables_YYYY.csv` files into 1 single file (`all_recode_variables.xlsx`) to enable quick, user review of variable recoding crosswalks across all data vintages.| 

# Harmonizing a New Data Vintage

**Note:** This workflow assumes only minor variations across future WHALEs vintages. Any major changes to data vintages (such as those due to a subsequent system migration) would likely result in code-breaking changes and would require additional development efforts to harmonize the data.

1. Download the new year's WA DOH CHS Death Certificate Statistical Files from Secure Access Washington.

2. Under `Resources/Crosswalks`:
    - Create a new year (`YYYY`) subfolder for the data vintage.
    - Copy a `rename_variables_YYYY.csv` from the previous year and place it in the new year subfolder.
    - Copy a `recode_variables_YYYY.csv` from the previous year and place it in the new year subfolder.

3. Edit `rename_variables_YYYY.csv` (this is the **Variable Renaming Crosswalk**)
    - `file_year` - Enter in the new year (`YYYY`)
    - `from_name` - Enter in the variable names in the new year data vintage that would align with the `harmonized data` variable referenced in `to_name`. 
    - `to_name` - Keep as-is. These are the the cleaned variable names (harmonized across all years' data vintages)
    - `missing` - Default is `FALSE`. Mark `TRUE` for edge cases where the new year data vintage does not contain a `from_name` variable that could be cleaned/harmonized into the `to_name` variable. (If `missing` is `TRUE` it does not really matter what you enter for `from_name`).

4 Edit `recode_variables_YYYY.csv` (this is the **Variable Recoding Crosswalk**)
    - `file_year` - Enter in the new year (`YYYY`)
    - `variable` - Enter in the harmonized variable name (same as `to_name` from the **Variable Renaming Crosswalk**)
    - `from_code` - Enter in the code value for the new data year variable to be recoded (to align with the `harmonized data` coded values). 
        - **Note**: Codes that are already aligned the `harmonized data` coded values **do not need to be added to this file**.
    - `to_code` - Enter in the coded value `from_code` will be recoded to. This will be the respective `harmonized data` coded value.
    - `from_label` - Enter in the description value for the new data year variable to be recoded (to align with the `harmonized data` label values)
        - **Note**: Labels that are already aligned the `harmonized data` coded values **do not need to be added to this file**.
    - `to_label` - Enter in the description value `from_label` will be recoded to. This will be the respective `harmonized data` label value.
    - `notes` - A free-text column to add any context around the recoding of specific variables
    - `review_flag` - Default is `FALSE`. Mark `TRUE` if any specific recodings should be reviewed and discussed by SMEs before implementing. 

5. Run `Scripts/crosswalk_review.R`. It will create:
    - `Resources/Crosswalks/all_rename_variables.xlsx` - A collated spreadsheet of all yearly **Variable Renaming Crosswalks** (`rename_variables_YYYY.csv`). This is meant to support end user reviews of the renaming subprocess.
    - `Resources/Crosswalks/all_recode_variables.xlsx` - A collated spreadsheet of all yearly **Variable Recoding Crosswalks** (`recode_variables_YYYY.csv`). This is meant to support end user reviews the recoding subprocess.
    - `Resources/Review/Missing Variables Referenced in Rename Crosswalk.xlsx` - A spreadsheet summarizing all of the variables (and respective years) in `all_rename_variables.xlsx` that were flagged as `missing` = `TRUE`. This is meant to flag edge cases in the variable renaming subprocess where variables in the `harmonized data` are entirely missing/not present in the data (only for a few select years).
    -  `Resources/Review/Flagged Variable Recoding Operations.xlsx`- A spreadsheet summarizing all of the variables (and respective years) in `all_recode_variables.xlsx` that were flagged as `review_flag` = `TRUE`. This is meant to edge cases in the variable recodoing subprocess where a specific recode should be reviewed and discussed by SMEs before implementing. This is primarily used for data values not specifically listed in the WA DOH Data Dictionary. 

# Misc

## Custom Functions (& Helper R Scripts)

Custom R functions were developed to streamline and increase the legibility of this data pipeline. Custom functions are stored under `Scripts/Custom_Functions/`.

**1_harmonize_death_files.R**
- `load_crosswalk()`: Loads the `rename_variables_YYYY.csv` or `recode_variables_YYYY.csv` for the given year.
- `load_data_vintage()`: Loads the specified Death Statistical Annual File. Performs under-the-hood operations including Data Type Harmonization (all variables as character data type), adding any missing variables (that are present in other data vintages) with all values as NA, and converting standard placeholders (i.e. "" or "NA" to `NA` values).
- `rename_variables()`: Performs Schema Harmonization (data vintage variable names --> standard variable naming convention for `harmonized_data`) using related `rename_variables_YYYY.csv` file. Remaining variables are organized with `state_file_number` first, then the remaining in alphabetical order.
- `recode_variables()`: Performs Value Harmonization (data vintage variable coding --> standard variable coding convention for `harmonized_data`) using related `recode_variables_YYYY.csv` file. Only variable-code value pairs that are not in the standard variable coding convention for `harmonized_data` are converted.
- `visualize_completeness()`: Creates an interactive heatmap of variable percent completeness by file year (Note: some file years may use `NA` while others may also have explicit `Unknown` values).

**2_process_geography_variables.R**
- `county_wa_code_to_fips()`: Translates WA code geography variables to FIPS codes. These translations are then used to backfill missing FIP code information.
- `county_fips_to_literals()`: Translates FIPS code geography variables and translates to literal geographies. These translations are then used to backfill missing literal geography variable values.

**3_clean_harmonized_data.R**
- `unify_variables()`: Takes versions of similar variables (ex: `disposition_facility_code` - `disposition_facility_name`, and `funeral_home_code` and `funeral_home_name`) that are slightly different across annual data vintages, and combines them into a singular, standardized variable in `harmonized_data`.
- `combine_code_columns()`: Takes the many code columns (ex: `record_axis_code_1` to `record_axis_code_20`) and combines them into a single code column as a concatenated string (to allow for easier data management).
- `clean_date_variables()`: Takes the numerous date variables (stored as the character data type) and converts them to date data types. This function excepts dates formatted in many ways (see the `orders` parameter), and dates with improper formatting or unrealistic values are converted automatically to `NA`.
- `clean_time_variables()`: Takes the numerous time variables (ex: time_of_death, time_of_death_hour, time_of_death_minute, time_of_injury, time_of_injury_hour, time_of_injury_minute) whose format and availability can vary year-to-year, and converts the values from character data type to time (lubridate hms) data type.
- `clean_data_types()`: Uses `schema_data_types.csv` to convert `harmonized_data` variables to their final proper data types. **Note:** This does not apply to `date` and `time` related variables as they are handled previously/exclusively in `clean_date_variables()` and `clean_time_variables()`. Includes an audit feature to see original vs converted data types for all variables.
- `apply_variable_labels()`: This function is optional to use (as determined by `params$apply_variable_labels` in `0_setup.R`). It uses `schema_factors.csv` to apply proper levelling and labels to all factor and ordered (factor) variables indicated in `schema_data_types.csv`. It includes an audit feature to see applied levels and labels as well as any potentially unmatched values.
- `visualize_completeness()`: Creates an interactive heatmap of variable percent completeness by file year (Note: some file years may use `NA` while others may also have explicit `Unknown` values).
- `create_data_dictionary()`: Creates a data dictionary for the harmonized death data. 

**crosswalk_review.R**
- This is an R script - not a custom R function!
- This R script should be run anytime any of the `rename_variables_YYYY.csv` or `recode_variables_YYYY.csv` are edited.
- This R script loads and combines all `rename_variables_YYYY.csv` and `recode_variables_YYYY.csv` files into `all_rename_variables.csv` and  `all_recode_variables.csv` respectively which are easier to users to review and see the crosswalks implemented across all data vintages.
- Additionally, this R script produces 2 summary files in the `Resouces/Review` folder (`Missing Variables Referenced in Rename Crosswalk.xlsx` and `Flagged Variable Recording Operations.xlsx`) that **succinctly document critical data quality issues observed across annual data vintages that will need to be remedied by the project team**. 

### Other Resources
- [WA DOH Death Data User Guide](https://doh.wa.gov/sites/default/files/2024-10/422-155-WADeathFileDataUsersGuide2023_1.pdf)
