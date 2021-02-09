library(dplyr)
library(textclean)
library(stringr)
library(httr)





# L1: Compare old and new csv files to obtain table with rows that need to be cleaned and added====


load_all_new_data <- function () {
  all_csv_files <- c("fracfocuscsv/FracFocusRegistry_1.csv",
                     "fracfocuscsv/FracFocusRegistry_2.csv",
                     "fracfocuscsv/FracFocusRegistry_3.csv",
                     "fracfocuscsv/FracFocusRegistry_4.csv",
                     "fracfocuscsv/FracFocusRegistry_5.csv",
                     "fracfocuscsv/FracFocusRegistry_6.csv",
                     "fracfocuscsv/FracFocusRegistry_7.csv",
                     "fracfocuscsv/FracFocusRegistry_8.csv",
                     "fracfocuscsv/FracFocusRegistry_9.csv",
                     "fracfocuscsv/FracFocusRegistry_10.csv",
                     "fracfocuscsv/FracFocusRegistry_11.csv",
                     "fracfocuscsv/FracFocusRegistry_12.csv",
                     "fracfocuscsv/FracFocusRegistry_13.csv",
                     "fracfocuscsv/FracFocusRegistry_14.csv",
                     "fracfocuscsv/FracFocusRegistry_15.csv",
                     "fracfocuscsv/FracFocusRegistry_16.csv",
                     "fracfocuscsv/FracFocusRegistry_17.csv")
  list <- lapply(all_csv_files, function(fname) {
    read.csv(paste0("8 Update Data/", fname), stringsAsFactors = FALSE, 
             colClasses = c("APINumber"="character"))
  })
  
  master_table <- do.call(rbind.data.frame, list)
  
  return (tbl_df(master_table))
}

new_master_table <- load_all_new_data()

old_L5 <- tbl_df(read.csv("2 Cleaning/L5 Cleaning/L5 master table.csv", 
                          stringsAsFactors = FALSE, colClasses = c("APINumber"="character"))) %>% select(-X)
old_upload_keys <- unique(old_L5$UploadKey)

uncleaned_master <- new_master_table %>% filter(
  !(UploadKey %in% old_upload_keys)
)


# L2 =====

# Search for non-ASCII characters
column_classes <- sapply(uncleaned_master, class)
char_type_indices <- unname(which(column_classes == "character"))  # records all indices of char columns
replaced_indices <- data.frame(column = c(), row = c())
replaced_strings <- c()
for (i in char_type_indices) {
  print(paste("beginning column", i))
  v <- pull(uncleaned_master, i)
  nonascii <- which(replace_non_ascii(str_replace_all(v, " ", "")) != str_replace_all(v, " ", ""))
  for (rowentry in nonascii) {
    replaced_indices <- rbind(replaced_indices, data.frame(column=i, row=rowentry))
    replaced_strings <- c(replaced_strings, v[rowentry])
  }
}
# Results: 1023 entries with non-ascii characters
w <- str_replace_all(replaced_strings, "\n", "")
w <- str_replace_all(w, "\t", "")
which_replaced_strings <- which(replace_non_ascii(str_replace_all(w, " ", "")) != str_replace_all(w, " ", ""))
# all non-ascii characters taken care of

L2_updated <- uncleaned_master
for (r in 1:nrow(replaced_indices)) {
  col <- replaced_indices$column[r]
  row <- replaced_indices$row[r]
  entry <- L2_updated[row, col]
  print(paste0(r, "/1023 replacing entry: ", entry))
  entry <- str_replace_all(entry, "\n", "")
  entry <- str_replace_all(entry, "\t", "")
  L2_updated[row, col] <- entry
}
write.csv(L2_updated, file="8 Update Data/L2 updated.csv")




# L3 =====

# Exclude all 88,184 entries made to Frac Focus 1.0
L3_updated <- L2_updated %>% filter(FFVersion != 1)

# Quality assurance check 1: unique combination of fracture date and API number
QA_table_1 <- L3_updated %>%
  select(UploadKey, JobStartDate, JobEndDate, APINumber) %>%
  distinct()
# Results: 18,921 unique UploadKeys; each have unique JobStartDate, JobEndDate, APINumber combination
distinct_uploadkeys <- QA_table_1 %>%
  distinct(JobStartDate, JobEndDate, APINumber, .keep_all = TRUE) %>%
  pull(UploadKey)
# Results: 18,570 unique JobStartDate/JobEndDate/APINumber combinations; 351 duplicate UploadKeys
L3_updated <- L3_updated %>%
  filter(UploadKey %in% distinct_uploadkeys)
# Results: Excluded 12,312 rows

# Quality assurance check 2: fracture date after January 1, 2011
L3_updated <- L3_updated %>%
  filter(grepl("201[12345678]", JobStartDate))
# Results: Filtered out 2,434 submissions from before 2011; last JobStartDate was 12/31/2018


# State verification: ensure Latitude/Longitude match StateName 
# (which is calculated from StateNumber, calculated from APINumber)
projection_summary <- L3_updated %>%
  group_by(Projection) %>%
  summarize(NumSubmissions = n(), NumJobs = n_distinct(UploadKey), NumWells = n_distinct(APINumber))
# Results: NAD27: 414887 submissions, 10386 jobs
#          NAD83: 262752 submissions, 7723 jobs
#          WGS84: 16285 submissions, 397 jobs
# Convert NAD27 to NAD83
NAD27_points_to_convert <- L3_updated %>%
  filter(Projection == "NAD27") %>%
  select(UploadKey, Latitude, Longitude) %>%
  rename(lat = Latitude, lon = Longitude) %>%
  distinct(UploadKey, .keep_all=TRUE) %>%
  mutate(eht="N/A", inDatum="NAD27", outDatum="NAD83(2011)", spcZone="auto", utmZone="auto")
# Make manual corrections for latitudes out of range [25, 50] and longitudes outside range [-130, -65]
for (i in which(NAD27_points_to_convert$UploadKey %in% c(   # switch lat and lon for these UploadKeys
  "84115b41-f470-4ae8-a9a7-9721006e3711",
  "10993081-6739-4d9e-9056-b314b07d7fb5"
))) {
  lat <- NAD27_points_to_convert[i, 3]
  NAD27_points_to_convert[i, 3] <- NAD27_points_to_convert[i, 2]
  NAD27_points_to_convert[i, 2] <- lat
}
for (i in which(NAD27_points_to_convert$UploadKey %in% c(   # negative longitude for these UploadKeys
  "4267797c-63d3-448c-97af-edec3d9934f9", 
  "d2eaf41d-bf01-407e-8e7f-13d9de224a9a",
  "72fd733e-2ab2-430d-910b-37f97bdcc248",
  "5d286e91-4a59-46f4-bd35-4e6e0e4286c5",
  "f838249d-7308-4de4-a4b2-7e234f896256",
  "879a6b42-a30f-4d15-8924-eeb6c00f9ea3"
))) {
  NAD27_points_to_convert[i, 3] <- -NAD27_points_to_convert[i, 3]
}
# These 3 have unsalvageable errors:
# d4358031-8937-4cd4-9227-801045baf5c0
# d82618d9-ea80-4ea0-ae9a-134df8d45ae7
# b71f0646-2c79-43b1-ace8-c71846839a6c
# Now write them to separate files
writeNAD <- function (startnum, endnum) {
  write.csv(NAD27_points_to_convert[startnum:endnum,], 
            file=paste0("8 Update Data/NAD27 points to convert/", startnum, "_", endnum, ".csv"), 
            quote=FALSE, row.names=FALSE)
}
writeNAD(1, 1000)
writeNAD(1001, 4000)
writeNAD(4001, 8000)
writeNAD(8001, nrow(NAD27_points_to_convert))
# MANUAL USE OF CONVERTER HERE TO GET OUTPUT FILES
allcsvs <- list.files(path = "8 Update Data/NAD83 converted points/", pattern = "[0-9]+\\_[0-9]+\\.csv")
list <- lapply(allcsvs, function(fname) {
  read.csv(paste0("8 Update Data/NAD83 converted points/", fname), na.strings="N/A", stringsAsFactors=FALSE)
})
NAD83_converted_points <- tbl_df(do.call(rbind.data.frame, list)) %>% distinct() # weird doubles
# Add LatitudeClean and LongitudeClean to hold NAD83, converted or original, or WGS84 coordinates
get_lat_clean <- function (projection, uploadkey, srcLat) {
  if (projection == "NAD27") {
    return (NAD83_converted_points$destLat[which(NAD83_converted_points$ID == uploadkey)])
  }
  return (srcLat)
}
get_lon_clean <- function (projection, uploadkey, srcLon) {
  if (projection == "NAD27") {
    return (NAD83_converted_points$destLon[which(NAD83_converted_points$ID == uploadkey)])
  }
  return (srcLon)
}
L3_updated <- L3_updated %>%      # (note: takes a while)
  mutate(LatitudeClean = mapply(get_lat_clean, Projection, UploadKey, Latitude)) %>%
  mutate(LongitudeClean = mapply(get_lon_clean, Projection, UploadKey, Longitude))
# Get area of each UploadKey from coordinates using FCC Area API
area_from_coords <- L3_updated %>%
  distinct(UploadKey, .keep_all = TRUE) %>%
  select(UploadKey, LatitudeClean, LongitudeClean) %>%
  mutate(StateNameFromCoords = "", StateAbbFromCoords = "", StateNumberFromCoords = 0, CountyNameFromCoords = "", CountyCodeFromCoords = 0)
invalid_keys <- c()
for (i in 1:nrow(area_from_coords)) {
  print(paste0("Querying ", i, "/", nrow(area_from_coords)))
  FCC_resp <- GET(paste0("https://geo.fcc.gov/api/census/area?lat=", area_from_coords$LatitudeClean[i], "&lon=", area_from_coords$LongitudeClean[i], "&format=json"))
  if (status_code(FCC_resp) == 200 && length(content(FCC_resp)$results) > 0) {
    FCC_resp <- content(FCC_resp)$results[[1]]
    area_from_coords$StateNameFromCoords[i] <- FCC_resp$state_name
    area_from_coords$StateAbbFromCoords[i] <- FCC_resp$state_code
    area_from_coords$StateNumberFromCoords[i] <- as.integer(FCC_resp$state_fips)
    area_from_coords$CountyNameFromCoords[i] <- FCC_resp$county_name
    area_from_coords$CountyCodeFromCoords[i] <- as.integer(FCC_resp$county_fips)
  } else {
    invalid_keys <- c(invalid_keys, area_from_coords$UploadKey[i])
    area_from_coords$StateNameFromCoords[i] <- NA
    area_from_coords$StateAbbFromCoords[i] <- NA
    area_from_coords$StateNumberFromCoords[i] <- NA
    area_from_coords$CountyNameFromCoords[i] <- NA
    area_from_coords$CountyCodeFromCoords[i] <- NA
  }
}
# Match to master table
L3_updated <- left_join(L3_updated, (area_from_coords %>% select(-LatitudeClean, -LongitudeClean)), by="UploadKey")
# Add StateOK and CountyOK fields
L3_updated <- L3_updated %>% 
  mutate(StateOK = (!is.na(StateNameFromCoords) & StateName == StateNameFromCoords)) %>%
  mutate(CountyOK = (!is.na(CountyNameFromCoords) & CountyName == CountyNameFromCoords))

# Fix all misspellings of Supplier in new SupplierClean field
# Read supplier aliases.csv as data frame and convert to list of vectors, getting rid of "\t" and "\n"
supplier_aliases_df <- read.csv("2 Cleaning/L3 Cleaning/supplier aliases.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
supplier_aliases_list <- apply(supplier_aliases_df, 1, function (row) {
  return (unique(str_replace_all(str_replace_all(row[which(!is.na(row))], "\n", ""), "\t", "")))
})
# Add SupplierClean column containing first possible spelling of each company
L3_updated <- L3_updated %>%
  mutate(SupplierClean = as.character(lapply(Supplier, function (supplierdirty) {
    lst_idx <- match(TRUE, lapply(supplier_aliases_list, function (v) {supplierdirty %in% v}))
    if (is.na(lst_idx)) {
      return (NA)
    }
    return (supplier_aliases_list[[lst_idx]][1])
  })))

# Remove spaces from CASNumber field
L3_updated <- L3_updated %>% mutate(CASNumber = str_replace_all(CASNumber, " ", ""))

write.csv(L3_updated, file="8 Update Data/L3 updated.csv")

# L4:
L4_updated <- L3_updated

# CASLabel generation: Valid, Invalid, Confidential, Proprietary, Trade Secret, or Not Available
confidentials <- c("Confidential", "CBI", "Confidnetial", "confidential", "CONFIDENTIAL",
                   "ConfidentialInfo", "ConfidentialBusines", "Confidenial", "Confidentail",
                   "ConfBusInfo", "Confid.Bus.Info", "Confidental", "BusinessConfidental",
                   "Conf", "Confidentia1", "Confinential", "CONFIDENTIALBUSINES",
                   "Confindential", "COnfidential", "Condidential", "Confdential", "Coinfidential")
proprietaries <- c("PROPRIETARY", "Proprietary", "proprietary", "Proprietatry", "3rdPartyProprietar",
                   "Proprietar", "ProprietaryBlend", "Prop", "7732-18-5proprietary", "PROP",
                   "7732-18-5/propr", "Propritary", "PROPRITARY", "propietary", "propriety",
                   "3rdpartyproprietar", "Propietary", "proprietry", "Proprietart", "Priprietary",
                   "Proprietery", "Properitary", "Propreitary", "Propriatary", "Prop.", "Proprietarty",
                   "PROPRIERTARY", "PROPRIERARY", "PRIOPRIETARY", "prop", "Propriety", "proprietarty",
                   "PRORIETARY", "Proprietaryl", "PROPRIETARY0.10", "Proptietary", "Proprietory",
                   "Propreitory", "Proprieatary", "propriatary", "proprietory", "Porprietary")
tradesecrets <- c("TradeSecret", "TRADESECRET", "tradesecret", "TradeSecret,disc.", "Tradesecret",
                  "TRADESECRETS", "TS", "ts", "tradeseccret", "TradeSeceret", "tracesecret", "TradeName",
                  "TradSecret", "TRADESECERET", "tradeSecret", "TradeSecrer", "TradeSecrte",
                  "TradeSecert", "Tradesecret.", "Trade")
notavailables <- c("NOTPROVIDED", "N/A", "na", "NA", "NotAvailable", "N.A.", "None", "none", 
                   "NOTAVAILABLE", "NONE", "n/a", "unknown", "Unavailable", "Notavailable",
                   "Notavailable.", "\"N/A\"", "NOTASSIGNED", "undisclosed", "NotApplicable",
                   "notlisted", "Undisclosed", "notassigned", "Notapplicable.", "UNK", "N/D",
                   "N\\A", "NULL", "unk", "Notlisted", "NotEstablished", "non", "n/A", "Unknown",
                   "NotAssigned", "NA?", "N/a", "Na", "(N/A#)", "BA", "Blank", "CASNotAssigned", 
                   "NoneListed", "UNKNOWN", "Notassigned", "NotListed", "CASnotassigned")

L4_updated$CASNumber[which(is.na(L4_updated$CASNumber))] <- "NA"
getCASLabel <- function (CAS) {
  if (grepl("^[0-9]+\\-[0-9][0-9]\\-[0-9]$", CAS)) {
    # It is a properly formatted number: check through digit verification
    digits <- as.numeric(strsplit(str_replace_all(CAS, "-", ""), "")[[1]])
    checksum <- 0
    for (i in 1:(length(digits)-1)) {
      checksum <- checksum + digits[i] * (length(digits) - i)
    }
    if (checksum %% 10 == digits[length(digits)]) {
      return ("Valid")
    } else {
      return ("Invalid")
    }
  } else if (CAS %in% confidentials) {
    return ("Confidential")
  } else if (CAS %in% proprietaries) {
    return ("Proprietary")
  } else if (CAS %in% tradesecrets) {
    return ("Trade Secret")
  } else if (CAS %in% notavailables) {
    return ("Not Available")
  }
  return ("Invalid")
}
L4_updated <- L4_updated %>% mutate(CASLabel = as.character(lapply(CASNumber, getCASLabel)))

# Check for leading zeros in valid CASNumbers
L4_updated %>% 
  select(CASNumber, CASLabel) %>%
  filter(CASLabel == "Valid") %>%
  filter(substring(CASNumber, 1, 1) == "0")
# Results: 8,323 mathematically valid CASNumbers with leading zeros

cut_leading_zeros_CAS <- function (CASNumber, CASLabel) {
  if (CASLabel != "Valid") {
    return (NA)
  }
  if (substring(CASNumber, 1, 1) != "0") {
    return (CASNumber)
  }
  return (cut_leading_zeros_CAS(substring(CASNumber, 2), CASLabel))
}
L4_updated <- L4_updated %>% 
  mutate(CASNumberClean = mapply(cut_leading_zeros_CAS, CASNumber, CASLabel))
names(L4_updated$CASNumberClean) <- NULL



# First check all existing CASNumberClean in previously cleaned data
raw_nums2vnames <- tibble(CASNumberClean = unique(L4_updated$CASNumberClean))
cas2name <- tbl_df(read.csv("2 Cleaning/L5 Cleaning/L5 master table.csv", stringsAsFactors=F)) %>%
  select(CASNumberClean, IngredientNameClean) %>%
  filter(!is.na(CASNumberClean)) %>%
  distinct(CASNumberClean, .keep_all = TRUE)
raw_nums2vnames <- left_join(raw_nums2vnames, cas2name, "CASNumberClean") 
raw_nums2vnames <- raw_nums2vnames %>%
  mutate(verified_names = ifelse(is.na(IngredientNameClean), "NOT FOUND", IngredientNameClean)) %>%
  filter(!is.na(CASNumberClean))

# Check against chemical database
raw_nums <- raw_nums2vnames$CASNumberClean
verified_names <- raw_nums2vnames$verified_names

# Check NIH database
for (i in 1:length(raw_nums)) {
  if (verified_names[i] == "NOT FOUND") {
    raw_num <- raw_nums[i]
    r <- GET(paste0("https://chem.nlm.nih.gov/chemidplus/rn/startswith/", raw_num))
    ct <- content(r, "text")
    match <- regexpr("Substance Name\\:\\&nbsp\\;.+?<", ct)
    if (match != -1) {
      vname <- substring(ct, match[1]+21, attr(match, "match.length")[1]+match[1]-2)
      verified_names[i] <- vname
    }
    print(paste0("i=", i, "/", length(raw_nums), ";  CAS=", raw_nums[i], ";    vname: ", verified_names[i]))
    Sys.sleep(3.1)   # has a max 1 search per 3 seconds
  }
}
# Through www.chemnet.com
for (i in 1:length(raw_nums)) {
  if (verified_names[i] == "NOT FOUND") {
    r <- GET(paste0("http://www.chemnet.com/Products/supplier.cgi?f=plist;terms=", raw_nums[i], ";submit=search"))
    ct <- content(r, "text")
    match <- str_match(ct, "Found <b> (\\d+) </b> products for <h1>(?:.|\\r|\\n)*?<p>(?:.|\\r|\\n)*?<a href=.*?>(.*?)(?:<| \\[)")
    if (match[1,2] != "0" && !is.na(match[1,3])) {
      verified_names[i] <- match[1,3]
    }
    print(paste0("Raw_num=", raw_nums[i], "    vname=", verified_names[i]))
  }
}
# Through webbook.nist.gov
for (i in 1:length(raw_nums)) {
  if (verified_names[i] == "NOT FOUND") {
    r <- GET(paste0("https://webbook.nist.gov/cgi/cbook.cgi?ID=", raw_nums[i], "&Units=SI"))
    ct <- content(r, "text")
    vname <- str_match(ct, "<title>(.*?)</title>")[1,2]
    if (vname != "Registry Number Not Found") {
      verified_names[i] <- vname
    }
    print(paste0("raw_num=", raw_nums[i], "     vname=", verified_names[i]))
  }
}
# Through http://www.sigmaaldrich.com/catalog/AdvancedSearchPage.do
for (i in 1:length(raw_nums)) {
  if (verified_names[i] == "NOT FOUND") {
    r <- GET(paste0("https://www.sigmaaldrich.com/catalog/search?interface=CAS%20No.&term=", raw_nums[i], "&N=0&lang=en&region=US&focus=product&mode=mode+matchall"))
    ct <- content(r, "text")
    vname <- str_match(ct, "<h2 class=\"name\".*?>(.*?)</h2>")[1,2]
    if (!is.na(vname)) {
      verified_names[i] <- vname
    }
    print(paste0("raw_num=", raw_nums[i], "     vname=", verified_names[i]))
  }
}
# Manual verification through scifinder.cas.org
raw_nums[which(verified_names == "NOT FOUND")]    # prints out CAS numbers to manually search
set_vname <- function (casnum, vname) {
  new_verified_names <- verified_names
  new_verified_names[which(raw_nums == casnum)] <- vname
  eval.parent(substitute(verified_names <- new_verified_names))
}
set_vname("5789-27-5", "4-Imidazolidinone, 5-methylene-3-phenyl-2-thioxo-")
set_vname("6474-02-8", "Acetamide, N-[4-[[[(4-methylphenyl)sulfonyl](2-methyl-2H-tetrazol-5-yl)amino]sulfonyl]phenyl]-")
set_vname("32685-03-3", "Phosphonic acid, P,P'-[[[2-[(2-hydroxyethyl)(phosphonomethyl)amino]ethyl]imino]bis(methylene)]bis-")
set_vname("424-85-1", "Pregn-4-ene-3,11,20-trione, 9-fluoro-21-hydroxy- (7CI,8CI,9CI)")
set_vname("6742-94-5", "Ethanone, 1-[(5alpha,17beta)-17-(acetyloxy)-3-[(difluoroboryl)oxy]androst-2-en-2-yl]- (9CI)")
set_vname("1310-62-9", "DNA (mouse strain C57BL/6J clone K330328H03 EST (expressed sequence tag)) (9CI)")
set_vname("253222-68-3", "GenBank AW300807 (9CI)")
# Set all that aren't found to invalid
invalids <- raw_nums[which(verified_names == "NOT FOUND")]
for (inv_cas in invalids) {
  i <- which(L4_updated$CASNumberClean == inv_cas)
  L4_updated$CASLabel[i] = "Invalid"
  L4_updated$CASNumberClean[i] <- NA
}
raw_nums <- raw_nums[! raw_nums %in% invalids]
verified_names <- verified_names[verified_names != "NOT FOUND"]
# Add IngredientNameClean field to master table
cas2vname <- tbl_df(data.frame(CASNumberClean = raw_nums, IngredientNameClean = verified_names, stringsAsFactors=FALSE))
L4_updated <- left_join(L4_updated, cas2vname, by = "CASNumberClean")

# Many entries for water (base fluid) do not include a CAS number, so identify these as valid disclosures
water_identifiers <- c(
  "Water (Including Mix Water Supplied by Client)*", "Water", "Carrier / Base Fluid - Water",
  "2% KCL Water", "Fresh Water", "Water (Including Mix Water Supplied by Client)", "Water, other",
  "Recycled Water ", "4% KCL Water", "Brine Water", "Lease Water", "NFIDB:2% KCL Water", "3% KCL Water",
  "Field Salt Water", "Produced Brine Water", "Tulare Water", "NFIDB:Lease Water", "water", "Water ",
  "KCl Water", "Produced Water", "1% KCL Water", "NFIDB:4% KCL Water", "NFIDB:Brine Water",
  "Recycled Water", "4% Salt Water", "10% Salt Water", "2% KCl Water", "NFIDB:3% KCL Water",
  "Water Moisture", "6% KCL Water", "fresh water", "NFIDB:3% NaCl Water", "NFIDB:6% KCL Water",
  "NFIDB:7% KCL Water", "18% Salt Water", "3% NaCl Water", "3% NACL Water", "4% NaCl Water", "5% KCl Water",
  "5% KCL Water", "7% KCL Water", "water, other", "2% KCl Lokern Water", "Brackish Water",
  "Brackish Water ", "NaCl Water", "NFIDB:15% Salt Water", "15% Salt Water", "3% Salt Water",
  "Fresh water", "KCL Water", "Lease Salt Water", "lease water", "NFIDB:5% KCL Water",
  "NFIDB:Water", "Produced Water ", "Production Water", "Salt Water",
  "Water (including mix water supplied by client)*", "Water, Other", " Water ", "1.5% KCL Water",
  "10% KCL Water", "3% KCl Water", "4% KCl Water", "4% NaCl Water ", "6% Salt Water ?",
  "Field Water", "NFIDB - 4 percent KCL Water", "NFIDB:1% KCL Water", "NFIDB:10% KCL Water",
  "NFIDB:10% Salt Water", "NFIDB:Sea Water", "produced Water", "Seawater", "Tulare  Water",
  "Water (Including Mix Water Supplied by Client).", "Water (including mix water supplied by Client)*",
  "Water (including Mix Water supplied by Client)*", "Water (major)",
  "Water, Including MIx water supplied by client", "Water,other",
  "water(including mix water supplied by client)*", "Water/Salt"
)
rows_to_change <- which((L4_updated$IngredientName %in% water_identifiers) &
                          (L4_updated$CASLabel %in% c("Invalid", "Not Available")))
for (r in rows_to_change) {
  L4_updated$CASLabel[r] <- "Valid"
  L4_updated$CASNumberClean[r] <- "7732-18-5"
  L4_updated$IngredientNameClean[r] <- "Water"
  print(r)
}

# Identify System Disclosures
L4_updated <- L4_updated %>% 
  select(-SystemApproach) %>%
  mutate(SystemApproachFlag = 
           !is.na(CASNumber) & 
           !is.na(IngredientName) &
           (CASNumber == "Listed" | 
              CASNumber == "ListedBelow" |
              CASNumber == "SystemDisclosure" |
              grepl("listed below", IngredientName, ignore.case=TRUE) |
              IngredientName == "Listed with chemicals" |
              IngredientName == "Listed with Ingredients" |
              IngredientName == "Listed with Other Chemicals" |
              IngredientName == "Listed with Other ingredients" |
              IngredientName == "Listed with Chemicals" | 
              IngredientName == "Listed with Chemical Ingredients"))
L4_updated$CASLabel[L4_updated$SystemApproachFlag] <- "Systems Approach"

# Add WithheldFlag field: TRUE if CASLabel == Valid or Systems Approach; FALSE otherwise
caslabel2withheld <- tbl_df(data.frame(
  CASLabel = c("Valid", "Systems Approach", "Invalid", "Proprietary", "Confidential", "Trade Secret", "Not Available"),
  WithheldFlag = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE))
L4_updated <- left_join(L4_updated, caslabel2withheld, by = "CASLabel")

# Add Systems Approach Form flag (TRUE if the Form has any ingredients that used SA)
L4_updated <- L4_updated %>%
  group_by(UploadKey) %>%
  mutate(FormUsedSystemApproach = sum(SystemApproachFlag) > 0) %>%
  ungroup()
# RESULTS: 9,905 / 18,506 forms used Systems Approach

write.csv(L4_updated, file="8 Update Data/L4 updated.csv")


# L5 cleanup: this download already includes FF3 SA forms in entirety, so just assign relevant attributes
L5_updated <- L4_updated %>% mutate(
  SubmissionsAvailable = TRUE,
  FromPDF = FALSE
)

write.csv(L5_updated, "8 Update Data/L5 updated.csv")



# Combine original and updated data sets =====

L5_original <- tbl_df(read.csv("2 Cleaning/L5 Cleaning/L5 master table.csv", 
                               stringsAsFactors = FALSE, colClasses = c("APINumber"="character"))) %>% 
  select(-X)

L5_original <- L5_original %>% mutate(DownloadDate = "2018-01-26")
L5_updated <- L5_updated %>% mutate(DownloadDate = "2019-01-19")

L5_updated_master_table <- bind_rows(L5_original, L5_updated)

write.csv(L5_updated_master_table, "8 Update Data/L5_updated_master_table.csv")



# Other data sets ====

essentials <- L5_updated_master_table %>%
  select(UploadKey, JobStartDate, JobEndDate, APINumber, FFVersion, StateName, StateAbbFromCoords, SupplierClean, CASLabel, CASNumberClean, IngredientNameClean, SystemApproachFlag, WithheldFlag, FormUsedSystemApproach, SubmissionsAvailable, FromPDF, DownloadDate)
write.csv(essentials, file="8 Update Data/updated ESSENTIALS.csv")

forms_data <- L5_updated_master_table %>%
  group_by(UploadKey) %>%
  mutate(WithholdingForm = sum(WithheldFlag) >= 1) %>%
  ungroup() %>%
  distinct(UploadKey, .keep_all=TRUE) %>%
  select(UploadKey, JobStartDate, JobEndDate, APINumber, StateNumber, CountyNumber, OperatorName, WellName,
         Latitude, Longitude, Projection, StateName, CountyName, FFVersion, LatitudeClean, LongitudeClean,
         StateNameFromCoords, StateAbbFromCoords, StateNumberFromCoords, CountyNameFromCoords, CountyCodeFromCoords,
         StateOK, CountyOK, FormUsedSystemApproach, SubmissionsAvailable, WithholdingForm, DownloadDate)
write.csv(forms_data, file="8 Update Data/updated forms data.csv")




