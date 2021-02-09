# Analysis of FracFocus.org Database

Kevin's working data set for analyzing FracFocus 2.0 (FF2.0) and FracFocus 3.0 (FF3.0).

## Getting Started

Due to size limitations, this git repository does not include any .csv or .zip files that appear locally. These may be downloaded from public Google Drive folder at the following link, placing the .zip files within the directories given the same names as in the Google Drive link.

**Data sets and methods paper are available on Google Drive at the following link:**
https://drive.google.com/drive/folders/1aFHUQh9AthIOrQ4QlYKRRLJJhPTniCeU?usp=sharing

### Loading the final cleaned data set

Final cleaned data in this repository should be placed under "8 Update Data/L5_updated_master_table.csv". See below, "Note on Inclusions and Exclusions".

Sample R code to load the full data:
```
library(dplyr)
L5 <- tbl_df(read.csv("8 Update Data/L5_updated_master_table.csv", stringsAsFactors = FALSE, colClasses = c("APINumber"="character"))) %>% select(-X)
```

Note that it helps to specify that the `APINumber`, which identifies the well, is a string and not a number.

#### Data subsets available

For analyses using the form as the smallest unit, "8 Update Data/updated forms data.csv" contains all of the form information (one row for each form, rather than one row for each ingredient). This set includes the columns `UploadKey`, `JobStartDate`, `JobEndDate`, `APINumber`, `StateNumber`, `CountyNumber`, `OperatorName`, `WellName`, `Latitude`, `Longitude`, `Projection`, `StateName`, `CountyName`, `FFVersion`, `LatitudeClean`, `LongitudeClean`, `StateNameFromCoords`, `StateAbbFromCoords`, `StateNumberFromCoords`, `CountyNameFromCoords`, `CountyCodeFromCoords`, `StateOK`, `CountyOK`, `FormUsedSystemApproach`, `SubmissionsAvailable`, `DownloadDate`, and `WithholdingForm`. All of these columns are selected from the original set, except `WithholdingForm`, which is a Boolean evaluating whether any ingredient on the form is withheld.

There is a dataset called "8 Update Data/updated ESSENTIALS.csv", which contains only the "essential" fields about each ingredient.


The following deprecated datasets are also available for the older versions (1/26/2018 download) of the data:

- "2 Cleaning/L5 Cleaning/L5 master table selected.csv" is the ingredient-level data set without the irrelevant and/or unreliable columns `TVD`, `TotalBaseWaterVolume`, `TotalBaseNonWaterVolume`, `FederalWell`, `IndianWell`, `Source`, `DTMOD`, `PurposeKey`, `IsWater`, `PurposePercentHFJob`, `PurposeIngredientMSDS`, `IngredientKey`, `PercentHighAdditive`, `PercentHFJob`, `IngredientComment`, `IngredientMSDS`, `MassIngredient`, `ClaimantCompany`, and `DisclosureKey`. This may be the recommended download for separate analyses at the ingredient level.

 - "2 Cleaning/final cleaned analysis subset.csv" also available (this is the L5 master table filtered for identifiable state).

 - "2 Cleaning/final cleaned analysis subset ESSENTIALS.csv" contains the most important fields (excluding columns like `TotalBaseWaterVolume`, etc.), to save time and storage space for quick analyses.

### Formatting of final data set

#### Overview

Each row in the data set represents one "ingredient". The ingredients can be grouped into a "form" that is identified by the `UploadKey` field. An `UploadKey` maps somewhat closely, but not exactly, to an `APINumber`. Some API numbers have multiple upload keys if multiple drilling operations occured at the same well site at different times.

The column `WithheldFlag` contains a Boolean stating if this ingredient is withheld (or, equivalently, if the `CASLabel` is anything other than "Valid").

The column `SystemApproachFlag` indicates whether this particular ingredient was withheld via the Systems Approach; the column `FormUsedSystemApproach` flag indicates whether any ingredient on the same form used the Systems Approach.

The column `SubmissionsAvailable` indicates whether the entries were either present in the original dataset or able to be scraped from PDFs, if they used the Systems Approach through FF3.0. The only cases of `SubmissionsAvailable==FALSE` should be those from FF3.0, from forms using the Systems Approach, whose PDFs could not be found online or converted to rows. It may be possible to add a few hundred rows from this category in the future (those that had >1 PDF forms per well site).

The column `FromPDF` indicates whether the row was present in the original dataset (`FALSE`), or whether it was scraped from a PDF file searched through FracFocus.org's "Find a Well" tool.

#### Data dictionary

(*) indicates that the column was added after the initial download from the FracFocus website. Italics indicate language taken from FracFocus's data dictionary for the initial download.

`UploadKey` -- A unique identifier for each form. Nearly a 1-to-1 correspondence with `APINumber`, but some wells have multiple forms.

`JobStartDate` -- *The date on which the hydraulic fracturing job was initiated.  Does not include site preparation or setup.*

`JobEndDate` -- *The date on which the hydraulic fracturing job was completed.  Does not include site teardown.*

`APINumber` -- A unique identifier for each well. A few wells have multiple forms (`UploadKey`s). According to FracFocus, *The American Petroleum Institute well identification number formatted as follows xx-xxx-xxxxx0000 Where: First two digits represent the state, second three digits represent the county, third 5 digits represent the well.*

`StateNumber` -- *The first two digits of the API number.  Range is from 01-50.*

`CountyNumber` -- *The 3 digit county code.*

`OperatorName` -- *The name of the operator.* This is the party responsible for registering on FracFocus and entering disclosures. The download contains 1169 unique operators.

`WellName` -- *The name of the well.*

`Latitude` -- *The lines that circle the earth horizontally, running side to side at equal distances apart on the earth. Latitude is typically expressed in degrees North/South. In the FracFocus system these lines are shown in decimal degrees and must be between 15 and 75.* Note that this may be reported in any of three projection systems.

`Longitude` -- *The lines that circle the earth vertically, running top to bottom that are equal distances apart at the equator and merge at the geographic top and bottom of the earth. Longitude is typically expressed in degrees East/West. In the FracFocus system the number representing these  lines are shown in decimal degrees and must be between -180 and -163. Note: Longitude number must be preceded by a negative sign.* Note that this may be reported in any of three projection systems.

`Projection` -- *The geographic coordinate system to which the latitude and longitude are related. In the FracFocus system the projection systems allowed are NAD (North American Datum) 27 or 83 and UTM (Universal Transverse Mercator).* This field, along with the coordinates, are later used to create the cleaned coordinate fields in one standardized projection system.

`TVD` -- *The vertical distance from a point in the well (usually the current or final depth) to a point at the surface, usually the elevation of the rotary kelly bushing.*

`TotalBaseWaterVolume` -- *The total volume of water used as a carrier fluid for the hydraulic fracturing job (in gallons).*

`TotalBaseNonWaterVolume` -- *The total volume of non water components used as a carrier fluid for the hydraulic fracturing job (in gallons).* 

`StateName` -- *The name of the state where the surface location of the well resides.  Calculated from the API number.*

`CountyName` -- *The name of the county were the surface location of the well resides.  Calculated from the API number.*

`FFVersion` -- An integer, either 2 or 3. *A key which designates which version of FracFocus was used when the disclosure was submitted.*

`FederalWell` -- *True = Yes, False = No.*

`IndianWell` -- Not entirely clear. Not provided in FracFocus documentation.

`Source` -- Also not clear. This entire column is filled with `NA` values.

`DTMOD` -- Also not clear. Again, this entire column is filled with `NA` values.

`PurposeKey` -- An identifier used to link the various FracFocus datasets during download (presumably, to link the ingredients to their purposes).

`TradeName` -- *The name of the product as defined by the supplier.*

`Supplier` -- *The name of the company that supplied the product for the hydraulic fracturing job (Usually the service company).* The same form may use chemicals from multiple different suppliers. Names are entered by hand by operators. For analyses, use the `SupplierClean` field to avoid misspellings. See "L3 Cleaning/supplier aliases.csv" for complete list of all suppliers and their misspellings.

`Purpose` -- *The reason the product was used (e.g. Surfactant, Biocide, Proppant).* Presumably also entered by the operator, but may be dictated by the supplier.

`IsWater` -- Unclear. This field is entirely populated by `NA` values.

`PurposePercentHFJob` -- Unclear. This field is entirely populated by `NA` values.

`PurposeIngredientMSDS` -- Unclear. This field is entirely populated by `NA` values.

`IngredientKey` -- An identifier used to link the various FracFocus datasets during download (presumably, to link ingredient data to form data).

`IngredientName` -- *Name of the chemical or for Trade Secret chemicals the chemical family name.* For analyses, use `IngredientNameClean`, since some chemicals have multiple names.

`CASNumber` -- *The Chemical Abstract Service identification number.* This should be unique for each chemical. Withholding in this study is determined by failure to adequately fill this field. Withheld chemicals may contain labels such as "Trade Secret" or "Proprietary". For analyses, use `CASNumberClean` to standardize such labels and identify invalid numbers.

`PercentHighAdditive` -- *The percent of the ingredient in the Trade Name product in % (Top of the range from MSDS).*

`PercentHFJob` -- *The amount of the ingredient in the total hydraulic fracturing volume in % by Mass.*

`IngredientComment` -- *Any comments related to the specific ingredient.* May include notes from operator such as contact information for supplier. Vast majority of ingredients have empty comment.

`IngredientMSDS` -- Unclear. Presumably, whether an MSDS (Material Safety Data Sheet) exists for the specified chemical, which would imply knowledge of human health effects.

`MassIngredient` -- Unclear. Presumably, the total mass of the chemical used.

`ClaimantCompany` -- Unclear. Entirely populated by `NA` values.

`DisclosureKey` -- An identifier used to link the various FracFocus datasets during download.

`LatitudeClean`* -- The well latitude, converted to the NAD83 projection system if it had been reported in NAD27. A few latitude values identified as errors and corrected manually (see "2 Cleaning/L3 Cleaning/Data Cleaning Log.txt").

`LongitudeClean`* -- The well longitude, converted to the NAD83 projection system if it had been reported in NAD27. A few longitude values identified as errors and corrected manually (see "2 Cleaning/L3 Cleaning/Data Cleaning Log.txt").

`StateNameFromCoords`* -- The state name determined by the cleaned latitude and longitude values, through the FCC Area API (https://geo.fcc.gov/api/census/ -- accessed 3/4/18).

`StateAbbFromCoords`* -- Two-letter state abbreviation from the FCC Area API.

`StateNumberFromCoords`* -- Two-digit state code from the FCC Area API.

`CountyNameFromCoords`* -- The county name from the FCC Area API.

`CountyCodeFromCoords`* -- The county identifier code from the FCC Area API.

`StateOK`* -- Boolean field indicating whether the state name matches between the operator's reported API number and the state determined from the cleaned coordinates. Analyses with states as random effects may want to exclude those with state discrepancies.

`CountyOK`* -- Boolean field indicating whether the county name matches between the operator's reported API number and the county determined from the cleaned coordinates.

`SupplierClean`* -- The standardized supplier name to eliminate spelling errors and multiple names for the same company. Supplier names treated as the same company can be found at "2 Cleaning/L3 Cleaning/supplier aliases.csv", which was partially generated by Levenshtein string-matching algorithms and partly by hand, and completely verified by hand. Blank entries, incorrect entries (e.g. "Surfactant"), or unidentifiable entries (e.g. "Multiple Suppliers" or "TES") were given the entry "UNIDENTIFIABLE" for this field.

`CASLabel`* -- One out of the set (Valid, Confidential, Proprietary, Trade Secret, Not Available, Invalid, Systems Approach). Determined by the CAS number. The label is Valid if and only if the `CASNumberClean` is not `NA`.

`CASNumberClean`* -- CAS numbers that are kept after discarding those that do not match to a verified chemical against an online database. See "2 Cleaning/Data Cleaning Log.txt" for more. Invalid CAS numbers are given `NA` values for this field.

`IngredientNameClean`* -- A standardized name of the chemical obtained from an online database. 1-to-1 correspondence with `CASNumberClean`.

`SystemApproachFlag`* -- Boolean field indicating whether that particular ingredient is indicated as a systems approach disclosure. Equivalent to whether `CASLabel` is "Systems Approach". NOTE: This means that individual chemicals with valid CAS numbers are NOT flagged as True, even if they are part of forms that do use the systems approach. For analyses, the `FormUsedSystemApproach` field is recommended above this one.

`WithheldFlag`* -- Boolean field indicating whether the CAS number is withheld. Determined from `CASLabel`. The ingredient is withheld if and only if the CAS label is "Valid" or "Systems Approach".

`FormUsedSystemApproach`* -- Boolean field indicating whether any ingredient on the form was listed as a systems disclosure. This is the recommended field for analyses, since the systems approach occurs at the level of the form.

`SubmissionsAvailable`* -- Boolean field indicating whether the raw chemicals were available. This will only be False if the form used the systems approach under FracFocus 3.0 (meaning the raw chemicals had to be scraped from a PDF) and the PDF was not obtainable for the raw ingredients.

`FromPDF`* -- Boolean field indicating whether the ingredient was scraped from a FracFocus PDF; otherwise, it was present in the original download.

`DownloadDate`* -- Date, in YYYY-MM-DD format, of download from FracFocus.org.

#### Treatment of Systems Approach disclosures

Wells that used the Systems Approach have 2 types of "ingredients" in the data set (my terminology):

* "Supplier ingredients" that list the trade name and purpose of the ingredient. These ingredients are composed of multiple raw chemicals, but the specific raw chemicals that compose it are not disclosed. These are identified by any row with `SystemApproachFlag==TRUE`.

* "Raw chemicals" that list the CAS number (or withholding reason, e.g. Proprietary). These ingredients compose the supplier ingredients above, but the Systems Approach does not offer which raw chemicals correspond to which supplier ingredient. These are identified by any row with `SystemApproachFlag==FALSE AND FormUsedSystemApproach==TRUE`.

**Before analysis, data set should be filtered according to the type of ingredient desired, to prevent double-counting.** Note that forms that did not use the Systems Approach do **not** have separate rows for supplier ingredients; the purpose and trade name is included along with each raw chemical.

These ideas may be clarified by viewing sample FracFocus disclosures under the directory "Sample FracFocus Disclosures".

Sample R code to filter the data:

```
# Extract only the raw chemicals
L5_chemicals <- L5 %>% filter(!SystemApproachFlag)
```

### Cleaning and analysis files

The repository is organized generally into folders as follows. FracFocus's raw compressed data was initially downloaded and uncompressed into "0 Raw Data." All of the initial cleaning (except the scraping and conversion of FF3.0 Systems Approach PDFs) is coded in "2 Cleaning/MASTER_CLEANING.R". The data update from Jan. 19, 2019 is downloaded, uncompressed, and cleaned under the directory "8 Update Data"; this raw data must be downloaded from our Google Drive folder. A complete log of the cleaning process can be found under "2 Cleaning/Data Cleaning Log.txt". Code to generate figures is located in "7 Final Exhibits/generate.R".

### Note on Inclusions and Exclusions

Because of space concerns, the raw data download from FracFocus.org is not included here, nor is the downloaded data from the Harvard paper (that was made available online). The many scraped PDFs for FF3.0 forms that used the systems approach (and the corresponding .py scripts, etc.), used to complete the data set in L5 cleaning, are also not included. See .gitignore file for a complete listing of exclusions.

