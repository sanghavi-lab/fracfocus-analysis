FRACFOCUS DATA DICTIONARY - Last updated: July 19th, 2017
--------------------------------------------------------
This data dictionary defines each attribute found in the FracFocusRegistry database backup which includes all disclosures 
locatable through the FracFocus ‘Find a Well’ search.


Table Name: RegistryUpload
--------------------------
pKey - Key index for the table

JobStartDate - The date on which the hydraulic fracturing job was initiated.  Does not include site preparation or setup.

JobEndDate - The date on which the hydraulic fracturing job was completed.  Does not include site teardown.

APINumber - The American Petroleum Institute well identification number formatted as follows xx-xxx-xxxxx0000 Where: First two digits 
represent the state, second three digits represent the county, third 5 digits represent the well.

StateNumber - The first two digits of the API number.  Range is from 01-50.

CountyNumber - The 3 digit county code.

OperatorName - The name of the operator.

WellName - The name of the well.

Latitude - The lines that circle the earth horizontally, running side to side at equal distances apart on the earth.   Latitude is typically 
expressed in degrees North/ South.  In the FracFocus system these lines are shown in decimal degrees and must be between 15 and 75.

Longitude - The lines that circle the earth vertically, running top to bottom that are equal distances apart at the equator 
and merge at the geographic top and bottom of the earth.  Longitude is typically expressed in degrees East/ West.  In the FracFocus 
system the number representing these  lines are shown in decimal degrees and must be between -180 and -163 Note: Longitude number must 
be preceded by a negative sign.

Projection - The geographic coordinate system to which the latitude and longitude are related. In the FracFocus 
system the projection systems allowed are NAD (North American Datum) 27 or 83 and UTM (Universal Transverse Mercator).

TVD - The vertical distance from a point in the well (usually the current or final depth) to a point at the surface, usually the 
elevation of the rotary kelly bushing.

TotalBaseWaterVolume - The total volume of water used as a carrier fluid for the hydraulic fracturing job (in gallons).

TotalBaseNonWaterVolume - The total volume of non water components used as a carrier fluid for the hydraulic fracturing job (in gallons)

StateName - The name of the state where the surface location of the well resides.  Calculated from the API number.

CountyName - The name of the county were the surface location of the well resides.  Calculated from the API number.

FFVersion - A key which designates which version of FracFocus was used when the disclosure was submitted.

FederalWell - True = Yes, False = No.


Table Name: RegistryUploadPurpose
---------------------------------
pKey - Key index for the table.

pKeyRegistryUpload - Foreign key linking to the RegistryUpload table.

TradeName - The name of the product as defined by the supplier.

Supplier - The name of the company that supplied the product for the hydraulic fracturing job (Usually the service company).

Purpose - The reason the product was used (e.g. Surfactant, Biocide, Proppant).


Table Name: RegistryUploadIngredients
-------------------------------------
pKey - Key index for the table.

pKeyPurpose - Foreign key linking to the RegistryUploadPurpose table.

IngredientName - Name of the chemical or for Trade Secret chemicals the chemical family name.

CASNumber - The Chemical Abstract Service identification number.

PercentHighAdditive - The percent of the ingredient in the Trade Name product in % (Top of the range from MSDS).

PercentHFJob - The amount of the ingredient in the total hydraulic fracturing volume in % by Mass.

IngredientComment - Any comments related to the specific ingredient.