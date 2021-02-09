# This file will work through different levels, L0-L5, of cleaning.

setwd("Fracking studies/")
library(dplyr)
library(tibble)
library(textclean)
library(stringr)
library(httr)

# L1: simple registry upload data combined into one master table.
# No other changes made.
load_all_registry_data <- function () {
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
                     "fracfocuscsv/FracFocusRegistry_13.csv")
  list <- lapply(all_csv_files, function(fname) {
    read.csv(paste0("0 Raw Data/", fname), stringsAsFactors = FALSE, colClasses = c("APINumber"="character"))
  })
  
  master_table <- do.call(rbind.data.frame, list)
  
  return (tbl_df(master_table))
}
L1_master_table <- load_all_registry_data()

# Get columns and their classes
column_classes <- sapply(L1_master_table, class)
write.table(column_classes, file="2 Cleaning/L1 column names and classes.csv", col.names=FALSE)

write.csv(L1_master_table, file="2 Cleaning/L1 Cleaning/L1 master table.csv")

# L2:
# Search for non-ASCII characters
char_type_indices <- unname(which(column_classes == "character"))  # records all indices of char columns
replaced_indices <- data.frame(column = c(), row = c())
replaced_strings <- c()
for (i in char_type_indices) {
  print(paste("beginning column", i))
  v <- pull(master_table, i)
  nonascii <- which(replace_non_ascii(str_replace_all(v, " ", "")) != str_replace_all(v, " ", ""))
  for (rowentry in nonascii) {
    replaced_indices <- rbind(replaced_indices, data.frame(column=i, row=rowentry))
    replaced_strings <- c(replaced_strings, v[rowentry])
  }
}
# Results: 9805 entries with non-ascii characters
w <- str_replace_all(replaced_strings, "\n", "")
w <- str_replace_all(w, "\t", "")
which_replaced_strings <- which(replace_non_ascii(str_replace_all(w, " ", "")) != str_replace_all(w, " ", ""))
# Results: all 9805 entries were solved by deleting \t and \n
L2_master_table <- L1_master_table
for (r in 1:nrow(replaced_indices)) {
  col <- replaced_indices$column[r]
  row <- replaced_indices$row[r]
  entry <- L2_master_table[row, col]
  print(paste0(r, "/9805 replacing entry: ", entry))
  entry <- str_replace_all(entry, "\n", "")
  entry <- str_replace_all(entry, "\t", "")
  L2_master_table[row, col] <- entry
}
write.csv(L2_master_table, file="2 Cleaning/L2 Cleaning/L2 master table.csv")

# L3:
# Exclude all 43,972 entries made to Frac Focus 1.0
L3_master_table <- L2_master_table %>% filter(FFVersion != 1)

# Quality assurance check 1: unique combination of fracture date and API number
QA_table_1 <- L3_master_table %>%
  select(UploadKey, JobStartDate, JobEndDate, APINumber) %>%
  distinct()
# Results: 91,181 unique UploadKeys; each have unique JobStartDate, JobEndDate, APINumber combination
distinct_uploadkeys <- QA_table_1 %>%
  distinct(JobStartDate, JobEndDate, APINumber, .keep_all = TRUE) %>%
  pull(UploadKey)
# Results: 89,753 unique JobStartDate/JobEndDate/APINumber combinations; 1428 duplicate UploadKeys
L3_master_table <- L3_master_table %>%
  filter(UploadKey %in% distinct_uploadkeys)
# Results: Excluded 49,729 submissions

# Quality assurance check 2: fracture date after January 1, 2011
L3_master_table <- L3_master_table %>%
  filter(grepl("201[12345678]", JobStartDate))
# Results: Filtered out 983 submissions from before 2011; last JobStartDate was 1/9/2018

# State verification: ensure Latitude/Longitude match StateName (which is calculated from StateNumber, calculated from APINumber)
projection_summary <- L3_master_table %>%
  group_by(Projection) %>%
  summarize(NumSubmissions = n(), NumJobs = n_distinct(UploadKey), NumWells = n_distinct(APINumber))
# Results: NAD27: 2170883 submissions, 59847 jobs
#          NAD83: 859655 submissions, 28799 jobs
#          WGS84: 32926 submissions, 1069 jobs
# Convert NAD27 to NAD83
NAD27_points_to_convert <- L3_master_table %>%
  filter(Projection == "NAD27") %>%
  select(UploadKey, Latitude, Longitude) %>%
  rename(lat = Latitude, lon = Longitude) %>%
  distinct(UploadKey, .keep_all=TRUE) %>%
  mutate(eht="N/A", inDatum="NAD27", outDatum="NAD83(2011)", spcZone="auto", utmZone="auto")
# Some manual corrections based on errors in NAD conversion
NAD27_points_to_convert[52295, 3] <- -98.732102099
for (i in c(54013, 54191, 55625, 56771, 57048, 57300)) {
  lat <- NAD27_points_to_convert[i, 3]
  NAD27_points_to_convert[i, 3] <- NAD27_points_to_convert[i, 2]
  NAD27_points_to_convert[i, 2] <- lat
}
NAD27_points_to_convert[59121, 3] <- -NAD27_points_to_convert[59121, 3]
NAD27_points_to_convert[59127, 3] <- -NAD27_points_to_convert[59127, 3]
writeNAD <- function (startnum, endnum) {
  write.csv(NAD27_points_to_convert[startnum:endnum,], file=paste0("2 Cleaning/NAD27 points to convert/", startnum, "_", endnum, ".csv"), quote=FALSE, row.names=FALSE)
}
writeNAD(1, 1000)
writeNAD(1001, 4000)
writeNAD(4001, 8000)
for (i in (1:12)*4000+4000) {
  writeNAD(i+1, i+4000)
}
writeNAD(56001, nrow(NAD27_points_to_convert))
# MANUAL USE OF CONVERTER HERE TO GET OUTPUT FILES
# Aggregate all 16 output files into one table with UploadKey, ConvertedLatitude, ConvertedLongitude
allcsvs <- list.files(path = "2 Cleaning/NAD83 converted points/", pattern = "[0-9]+\\_[0-9]+\\.csv")
list <- lapply(allcsvs, function(fname) {
  read.csv(paste0("2 Cleaning/NAD83 converted points/", fname), na.strings="N/A", stringsAsFactors=FALSE)
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
L3_master_table <- L3_master_table %>%      # (note: takes a while)
  mutate(LatitudeClean = mapply(get_lat_clean, Projection, UploadKey, Latitude)) %>%
  mutate(LongitudeClean = mapply(get_lon_clean, Projection, UploadKey, Longitude))
# Get area of each UploadKey from coordinates using FCC Area API
area_from_coords <- L3_master_table %>%
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
#L3_master_table <- inner_join(L3_master_table, area_from_coords, by = "UploadKey")
L3_master_table <- left_join(L3_master_table, (area_from_coords %>% select(-LatitudeClean, -LongitudeClean)), by="UploadKey")
# Add StateOK and CountyOK fields
L3_master_table <- L3_master_table %>% 
  mutate(StateOK = (!is.na(StateNameFromCoords) & StateName == StateNameFromCoords)) %>%
  mutate(CountyOK = (!is.na(CountyNameFromCoords) & CountyName == CountyNameFromCoords))

# Fix all misspellings of Supplier in new SupplierClean field
# Read supplier aliases.csv as data frame and convert to list of vectors, getting rid of "\t" and "\n"
supplier_aliases_df <- read.csv("2 Cleaning/L3 Cleaning/supplier aliases.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
supplier_aliases_list <- apply(supplier_aliases_df, 1, function (row) {
  return (unique(str_replace_all(str_replace_all(row[which(!is.na(row))], "\n", ""), "\t", "")))
})
# Add SupplierClean column containing first possible spelling of each company
L3_master_table <- L3_master_table %>%
  mutate(SupplierClean = as.character(lapply(Supplier, function (supplierdirty) {
    lst_idx <- match(TRUE, lapply(supplier_aliases_list, function (v) {supplierdirty %in% v}))
    if (is.na(lst_idx)) {
      return (NA)
    }
    return (supplier_aliases_list[[lst_idx]][1])
  })))

# Remove spaces from CASNumber field
L3_master_table <- L3_master_table %>% mutate(CASNumber = str_replace_all(CASNumber, " ", ""))

write.csv(L3_master_table, file="2 Cleaning/L3 Cleaning/L3 master table.csv")

# L4:
L4_master_table <- L3_master_table
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

L4_master_table$CASNumber[which(is.na(L4_master_table$CASNumber))] <- "NA"
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
L4_master_table <- L4_master_table %>% mutate(CASLabel = as.character(lapply(CASNumber, getCASLabel)))

# Check for leading zeros in valid CASNumbers
L4_master_table %>% 
  select(CASNumber, CASLabel) %>%
  filter(CASLabel == "Valid") %>%
  filter(substring(CASNumber, 1, 1) == "0")
# Results: 87,856 mathematically valid CASNumbers with leading zeros
cut_leading_zeros_CAS <- function (CASNumber, CASLabel) {
  if (CASLabel != "Valid") {
    return (NA)
  }
  if (substring(CASNumber, 1, 1) != "0") {
    return (CASNumber)
  }
  return (cut_leading_zeros_CAS(substring(CASNumber, 2), CASLabel))
}
L4_master_table <- L4_master_table %>% 
  mutate(CASNumberClean = mapply(cut_leading_zeros_CAS, CASNumber, CASLabel))
names(L4_master_table$CASNumberClean) <- NULL
# Check against chemical database
# Results: 1,200 unique, mathematically valid CAS numbers
raw_nums <- unique(L4_master_table %>% pull(CASNumberClean))
verified_names <- rep("NOT FOUND", length(raw_nums))
# Check NIH database
for (i in 1:length(raw_nums)) {
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
# Results: 1,130 entries verified through NIH
# Manually entered remaining unfound numbers into EPA database search
# Results: just 1 single positive result:
verified_names[which(raw_nums == "63-65-0")] <- "2-Propenoic acid, 2-methyl-, telomer with 2-propanol and 2-propenoic acid, sodium salt"
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
# Results: 9 entries verified by chemnet
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
# Results: 5 entries verified by webbook.nist.gov
# Through http://www.commonchemistry.org/
for (i in 1:length(raw_nums)) {
  if (verified_names[i] == "NOT FOUND") {
    r <- GET(paste0("http://www.commonchemistry.org/ChemicalDetail.aspx?ref=", raw_nums[i]))
    ct <- content(r, "text")
    vname <- str_match(ct, "<span id=\"indexNameLabel\">(.*?)</span>")[1,2]
    if (!is.na(vname)) {
      verified_names[i] <- vname
    }
    print(paste0("raw_num=", raw_nums[i], "     vname=", verified_names[i]))
  }
}
# Results: 0 entries verified by commonchemistry.org
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
# Results: 2 entries verified by sigma aldrich
# Manual verification through scifinder.cas.org
set_vname <- function (casnum, vname) {
  new_verified_names <- verified_names
  new_verified_names[which(raw_nums == casnum)] <- vname
  eval.parent(substitute(verified_names <- new_verified_names))
}
set_vname("126950-60-5", "Secondary alcohols, C12-14")
set_vname("397256-50-7", "2-Propenoic acid, polymer with sodium ethenesulfonate (1:1), peroxydisulfuric acid ([(HO)S(O)2]2O2) sodium salt (1:2)-initiated, reaction products with sodium P,P'-ethenylidenebis[phosphonate] (4:1)")
set_vname("224635-63-6", "Acetic acid, reaction products with acetophenone, cyclohexylamine, formaldehyde and methanol")
set_vname("773-18-2", "2H,6H-8a,4a-Propenopyrano[3,2-b]pyran (8CI,9CI)")
set_vname("1004542-84-0", "Sulfamic acid, N-bromo-, sodium salt, hydrate (1:1:?)")
set_vname("56652-26-7", "Poly(oxymethylene), α-[(4,4-dimethyl-3-oxazolidinyl)methyl]-ω-hydroxy- (9CI)")
set_vname("344329-87-9", "Cyclobutanol, 1-(nitromethyl)-")
set_vname("344329-35-7", "4-Piperidinol, 2-methyl-")
set_vname("1130-43-4", "2(3H)-Furanone, dihydro-5-methyl-5-(4-methylpentyl)-, (S)- (9CI)")
set_vname("267-56-1", "Benzo[1,2-b:5,4-b']difuran")
set_vname("67254-71-1", "Alcohols, C10-12, ethoxylated")
set_vname("910644-97-2", "2-Propenoic acid, ammonium salt (1:1), polymer with 2-propenamide, sodium salt")
set_vname("1009-07-0", "Acetamide, N-(5-chloro-2-mercaptophenyl)-")
set_vname("27176-67-6", "5-Norbornene-2,3-dicarboxylic acid, trans-, dimethyl ester, polymer with 2-norbornene (8CI)")
set_vname("304443-60-5", "Acetic acid ethenyl ester, polymer with α-hydro-ω-hydroxypoly(oxy-1,2-ethanediyl) hydrolyzed")
set_vname("9062-77-5", "Thyroid stimulator, long-acting")
set_vname("39382-21-3", "2-Butenedioic acid (2E)-, polymer with 1,2-ethanediol and α,α'-[(1-methylethylidene)di-4,1-phenylene]bis[ω-hydroxypoly[oxy(methyl-1,2-ethanediyl)]] (9CI)")
set_vname("7647-40-7", "Bicyclo[4.2.1]nonan-9-one, 7-methyl-, (2,4-dinitrophenyl)hydrazone, exo- (8CI)")
set_vname("128850-89-5", "11-Oxa-3,6-diaza-10-siladodecanoic acid, 3,6-bis(carboxymethyl)-10,10-dimethoxy-, sodium salt (1:3)")
set_vname("68848-87-3", "Titanium alloy, base, Ti 78,Nb 19,Mo 3 (9CI)")
set_vname("250-72-6", "Pyrrolo[2,3-b]pyrrole")
set_vname("12291-65-5", "Boron calcium oxide (B6Ca2O11), hydrate (1:5)")
set_vname("700842-79-1", "Hybridur 540")
set_vname("686411-32-5", "DNA (Acyrthosiphon pisum clone ID0AAA30CF09 EST (expressed sequence tag)) (9CI)")
set_vname("1422006-64-1", "Humic acids, potassium salts, polymers with N,N-dimethyl-2-propenamide, maleic anhydride and sodium 2-methyl-2-[(1-oxo-2-propen-1-yl)amino]-1-propanesulfonate (1:1), sodium salts")
set_vname("6743-01-7", "Benzoic acid, 4-[[(2,3,6,7-tetrahydro-1,3,7-trimethyl-2,6-dioxo-1H-purin-8-yl)methyl]amino]-, methyl ester")
set_vname("1352632-71-3", "Phosphoric acid, mixed diesters with C6-12 alcs., Et alc. and polyethylene glycol mono-Ph ether")
set_vname("51867-05-1", "Methanone, (2a,3-dihydrobenz[cd]indol-1(2H)-yl)phenyl-")
set_vname("10244-99-2", "5β-Rosane-5,6β-diol, (13S)- (8CI)")
set_vname("189959-15-7", "Amines, N-tallow alkyldipropylenetri-, ethoxylated")
set_vname("68593-28-2", "L-Methionine, N-(dimethylphosphinothioyl)-, methyl ester")
set_vname("1160525-87-0", "Phosphonic acid, P-[(1R,2R)-1,2-dihydroxypropyl]-, ammonium salt (1:?)")
set_vname("62601-60-9", "POC-OS 2020")
set_vname("90036-35-4", "2H-Pyrrole-5-carbonitrile, 3,4-dihydro-4-methyl-")
set_vname("182235-14-9", "1-Piperidinyloxy, 4,4'-[1,6-hexanediylbis(formylimino)]bis[2,2,6,6-tetramethyl-")
set_vname("68824-85-1", "Iron alloy, base, Fe 48,Ni 34,Cr 13,Ti 2.2,Nb 1.5,Mn 0.9,Al 0.3,Si 0.2 (9CI)")
set_vname("2690-05-3", "Pentane, 1,1,1,2,2,3,4,4,5,5,5-undecafluoro-3-(1,1,2,2,2-pentafluoroethyl)-")
set_vname("1736-47-6", "1H-Indene, 1,1,2,2,3,3,4,5,6,7-decafluoro-2,3-dihydro-")
set_vname("5549-29-1", "[1,1'-Biphenyl]-3-carboxamide, 5-chloro-N-(2-chloro-4-methylphenyl)-2-hydroxy-")
set_vname("915053-39-3", "DNA, d(T-T-C-G-G-T-T-G-T-C-G-A-T-A-T-G-A-G-G-A-T-C-T) (9CI)")
set_vname("94266-97-4", "Benzamide, N-(aminoiminomethyl)-4-hydroxy-3,5-dimethoxy-, hydrochloride (1:1)")
set_vname("31017-83-1", "Poly(oxy-1,2-ethanediyl), α,α'-[(dodecylimino)di-2,1-ethanediyl]bis[ω-hydroxy-")
set_vname("681130-15-4", "Piperidine, 4-(4-fluorobenzoyl)-1-[(2-methyl-3-furanyl)carbonyl]- (9CI)")
set_vname("69411-36-5", "Valine, N-(2,6-difluorophenyl)-, (3-phenoxyphenyl)methyl ester")
set_vname("34302-65-3", "Propanoic acid, 2-hydroxy-, ammonium salt (1:?)")
set_vname("53845-65-1", "2-Propenoic acid, sodium salt (1:1), polymer with 2-propenamide and sodium 2-methyl-2-[(1-oxo-2-propen-1-yl)amino]-1-propanesulfonate (1:1)")
# Results: Only 9 left that aren't found
# Set all that aren't found to invalid
invalids <- raw_nums[which(verified_names == "NOT FOUND")]
for (inv_cas in invalids) {
  i <- which(L4_master_table$CASNumberClean == inv_cas)
  L4_master_table$CASLabel[i] = "Invalid"
  L4_master_table$CASNumberClean[i] <- NA
}
raw_nums <- raw_nums[! raw_nums %in% invalids]
verified_names <- verified_names[verified_names != "NOT FOUND"]
# Add IngredientNameClean field to master table
cas2vname <- tbl_df(data.frame(CASNumberClean = raw_nums, IngredientNameClean = verified_names, stringsAsFactors=FALSE))
L4_master_table <- left_join(L4_master_table, cas2vname, by = "CASNumberClean")

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
# inserted after L4 cleaning
rows_to_change <- which((L4_master_table$IngredientName %in% water_identifiers) &
                          (L4_master_table$CASLabel %in% c("Invalid", "Not Available")))
for (r in rows_to_change) {
  L5$CASLabel[r] <- "Valid"
  L5$CASNumberClean[r] <- "7732-18-5"
  L5$IngredientNameClean[r] <- "Water"
  L5$WithheldFlag[r] <- FALSE
  print(r)
}

# Identify System Disclosures
L4_master_table <- L4_master_table %>% 
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
# RESULTS: 113,853 Systems Approach submissions
L4_master_table$CASLabel[L4_master_table$SystemApproachFlag] <- "Systems Approach"

# Add WithheldFlag field: TRUE if CASLabel == Valid or Systems Approach; FALSE otherwise
caslabel2withheld <- tbl_df(data.frame(
  CASLabel = c("Valid", "Systems Approach", "Invalid", "Proprietary", "Confidential", "Trade Secret", "Not Available"),
  WithheldFlag = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE))
L4_master_table <- left_join(L4_master_table, caslabel2withheld, by = "CASLabel")

# Add Systems Approach Form flag (TRUE if the Form has any ingredients that used SA)
L4_master_table <- L4_master_table %>%
  group_by(UploadKey) %>%
  mutate(FormUsedSystemApproach = sum(SystemApproachFlag) > 0) %>%
  ungroup()
# RESULTS: 10,391 / 89,715 forms used Systems Approach

write.csv(L4_master_table, file="2 Cleaning/L4 Cleaning/L4 master table.csv")



# L5 cleanup

# Add FF3.0 entries with SA

FF3sys <- L4_master_table %>%
  filter(FFVersion == 3, FormUsedSystemApproach)
# Find number of forms with more than one possible csv file
getFileNamePattern <- function (APINumber) {   # takes APINumber as a string (which it should be anyway)
  pat <- paste0(substr(APINumber, 1, 2), "\\-", substr(APINumber, 3, 5), "\\-",
    substr(APINumber, 6, 10), "\\-", substr(APINumber, 11, 12), "\\-", substr(APINumber, 13, 14), "\\-",
    "[0-9]+(?:\\([1-9]\\))?\\.csv"
  )
  return (pat)
}
FF3sys.forms <- FF3sys %>% 
  distinct(UploadKey, .keep_all=TRUE) %>% 
  select(UploadKey, APINumber, JobStartDate, JobEndDate, WellName, StateName, StateOK, OperatorName)
FF3sys.forms <- FF3sys.forms %>%
  mutate(FileNamePat = as.character(sapply(APINumber, getFileNamePattern)))
FF3sys.forms <- FF3sys.forms %>%
  mutate(NumMatchingFiles = as.numeric(sapply(FileNamePat, function(fnpat) {
    return (length(list.files(path = "2 Cleaning/Scraping FF3 SA/C ingredient table output/",
                              pattern = fnpat)))
  })))
# RESULTS: Of 9,376, there are 8,763 forms with 1 matching file (93.5%); 
  # 110 forms with no matching files (1.2%);
  # 503 forms with >1 matching files (5.4%) (includes a few accidental double-downloads)

# Load all FF3.0 entries with 1 downloaded csv file and create SubmissionsAvailable logical field and FromPDF logical field
L5 <- L4_master_table %>%
  mutate(SubmissionsAvailable = !FormUsedSystemApproach | FFVersion == 2) %>%  # start with all FF3.0 SA forms unavailable, replace with available when read
  mutate(FromPDF = FALSE)

# Iterate through all forms with 1 csv file
FF3sys.forms.singles <- filter(FF3sys.forms, NumMatchingFiles == 1)
# NOTE: WIll take an extraordinarily long time!
for (r in 291:nrow(FF3sys.forms.singles)) {  
  # Read file
  filename <- list.files(path = "2 Cleaning/Scraping FF3 SA/C ingredient table output/",
                        pattern = FF3sys.forms.singles$FileNamePat[r])
  new_ingr_tbl <- tbl_df(read.csv(paste0("2 Cleaning/Scraping FF3 SA/C ingredient table output/", filename), 
                                  stringsAsFactors=FALSE)) %>%
    select(-X)
  
  # Find well data from master table
  master_well_info <- filter(L5, UploadKey == FF3sys.forms.singles$UploadKey[r])[1,]   # one sample row from same well, to get well location etc.
  master_well_idx <- which(L5$UploadKey == master_well_info$UploadKey)[1]              # index of row in master table so new rows can be inserted at right location
  
  # Add rows to master table for raw chemical ingredients from SA
  new_ingrs <- master_well_info %>% filter(FALSE)
  first <- 1 + match(
    "Items above are Trade Names with the exception of Base Water . Items below are the individual ingredients.",
    new_ingr_tbl$Trade.name
  )
  if (!is.na(first)) {
    for (ingr in first:nrow(new_ingr_tbl)) {
      # Construct and add row to master table
      new_ingrs <- bind_rows(master_well_info, new_ingrs)   # new row is first, will change all ingredient-specific data
      new_ingrs$Source[1] <- NA
      new_ingrs$DTMOD[1] <- NA
      new_ingrs$PurposeKey[1] <- NA
      new_ingrs$TradeName[1] <- NA
      new_ingrs$Supplier[1] <- NA
      new_ingrs$Purpose[1] <- NA
      new_ingrs$IsWater[1] <- NA
      new_ingrs$PurposePercentHFJob[1] <- NA
      new_ingrs$PurposeIngredientMSDS[1] <- NA
      new_ingrs$IngredientKey[1] <- NA
      new_ingrs$IngredientName[1] <- new_ingr_tbl$Ingredients[ingr]
      tmp_cas <- str_replace_all(new_ingr_tbl$CAS[ingr], " ", "")   # remove spaces
      tmp_cas <- str_replace_all(tmp_cas, "\n", "")                 # remove newlines
      new_ingrs$CASNumber[1] <- str_replace_all(tmp_cas, "\t", "")  # remove tabs
      new_ingrs$PercentHighAdditive[1] <- new_ingr_tbl$Max.additive[ingr]
      new_ingrs$PercentHFJob[1] <- new_ingr_tbl$Max.HF.fluid[ingr]
      new_ingrs$IngredientComment[1] <- new_ingr_tbl$Comments[ingr]
      new_ingrs$IngredientMSDS[1] <- NA
      new_ingrs$MassIngredient[1] <- NA
      new_ingrs$ClaimantCompany[1] <- NA
      new_ingrs$SupplierClean[1] <- "UNIDENTIFIABLE"
      
      # Must verify CAS number before assigning as clean
      CAS <- new_ingrs$CASNumber[1]
      new_ingrs$CASLabel[1] <- getCASLabel(CAS)
      if (new_ingrs$CASLabel[1] == "Invalid") {
        new_ingrs$CASLabel[1] <- "NEEDS VALIDATION"
      }
      cleanCASidx <- match(cut_leading_zeros_CAS(CAS, new_ingrs$CASLabel[1]), L5$CASNumberClean)  # first index of same ingredient type in master table
      
      new_ingrs$CASNumberClean[1] <- L5$CASNumberClean[cleanCASidx]
      new_ingrs$IngredientNameClean[1] <- L5$IngredientNameClean[cleanCASidx]
      
      new_ingrs$SystemApproachFlag[1] <- FALSE
      
      new_ingrs$WithheldFlag[1] <- !(new_ingrs$CASLabel[1] == "Valid" | new_ingrs$CASLabel[1] == "Systems Approach")
      
      new_ingrs$SubmissionsAvailable[1] <- TRUE
      new_ingrs$FromPDF[1] <- TRUE
    }
    insertion_loc <- match(master_well_info$UploadKey, L5$UploadKey)
    L5 <- bind_rows(
      L5[1:insertion_loc-1,],
      new_ingrs,
      L5[insertion_loc:nrow(L5),]
    )
    
    # Make rest of rows available
    L5$SubmissionsAvailable[which(L5$UploadKey == master_well_info$UploadKey)] <- TRUE
  }
  
  print(paste0("Finished ", r, "/", nrow(FF3sys.forms.singles)))
}

# Check all previously unidentified ingredients against databases
unconfirmed_CAS <- unique((L5$CASNumber[which(is.na(L5$CASNumberClean) & L5$CASLabel == "Valid")]))  # all the mathematically valid CAS numbers that did not match up to any clean ingredients present in the dataset already
cas2vname <- tbl_df(data.frame(unconfirmed_CAS = unconfirmed_CAS, verified_name = NA))
# Check NIH database
for (i in 1:nrow(cas2vname)) {
  raw_num <- cas2vname$unconfirmed_CAS[i]
  r <- GET(paste0("https://chem.nlm.nih.gov/chemidplus/rn/startswith/", raw_num))
  ct <- content(r, "text")
  match <- regexpr("Substance Name\\:\\&nbsp\\;.+?<", ct)
  if (match != -1) {
    vname <- substring(ct, match[1]+21, attr(match, "match.length")[1]+match[1]-2)
    cas2vname$verified_name[i] <- vname
  }
  print(paste0("i=", i, "/", nrow(cas2vname), ";  CAS=", cas2vname$unconfirmed_CAS[i], ";    vname: ",
               cas2vname$verified_name[i]))
  Sys.sleep(3.1)   # has a max 1 search per 3 seconds
}
# Manual verifications of remaining through Scifinder
cas2vname$verified_name[which(cas2vname$unconfirmed_CAS == "93213-74-2")] <- "Benzene, 2-(bromomethyl)-1,3-dinitro-"
cas2vname$verified_name[which(cas2vname$unconfirmed_CAS == "1607814-38-9")] <- "Humic acids, potassium salts, polymers with N,N-dimethyl-2-propenamide, sodium acrylate and sodium 2-methyl-2-[(1-oxo-2-propen-1-yl)amino]-1-propanesulfonate (1:1), peroxydisulfuric acid ([(HO)S(O)2]2O2) sodium salt (1:2)-initiated"
cas2vname$verified_name[which(cas2vname$unconfirmed_CAS == "35152-74-0")] <- "Tyrosine, O-[4-hydroxy-3-(2-methylpropyl)phenyl]-3,5-diiodo-"

cas2vname$unconfirmed_CAS <- as.character(cas2vname$unconfirmed_CAS)
# Change CASLabel of unverified names to Invalid, while adding CASNumberClean and IngredientNameClean to verified names
for (i in 1:nrow(cas2vname)) {
  master_idxs <- which(L5$FromPDF & L5$CASNumber == cas2vname$unconfirmed_CAS[i])
  if (is.na(cas2vname$verified_name[i])) {
    L5$CASLabel[master_idxs] <- "Invalid"
    L5$WithheldFlag[master_idxs] <- TRUE
  } else {
    L5$CASLabel[master_idxs] <- "Valid"
    L5$WithheldFlag[master_idxs] <- FALSE
    L5$CASNumberClean[master_idxs] <- cas2vname$unconfirmed_CAS[i]
    L5$IngredientNameClean[master_idxs] <- cas2vname$verified_name[i]
  }
}

# Some manual checks completed; convert NEEDS VALIDATION to Invalid
unvalidated_idxs <- which(L5$CASLabel == "NEEDS VALIDATION")
L5$CASLabel[unvalidated_idxs] <- "Invalid"
L5$WithheldFlag[unvalidated_idxs] <- TRUE

write.csv(L5, file="2 Cleaning/L5 Cleaning/L5 master table.csv")




# TODO if time: load those with >1 downloaded csv file




# ANALYSIS SUBSET
analysis_subset <- L5 %>% 
  filter(StateOK)
write.csv(analysis_subset, file="2 Cleaning/final cleaned analysis subset.csv")

# ESSENTIALS
essentials <- analysis_subset %>%
  select(UploadKey, JobStartDate, JobEndDate, APINumber, FFVersion, StateName, StateAbbFromCoords, SupplierClean, CASLabel, CASNumberClean, IngredientNameClean, SystemApproachFlag, WithheldFlag, FormUsedSystemApproach, SubmissionsAvailable, FromPDF)
write.csv(essentials, file="2 Cleaning/final cleaned analysis subset ESSENTIALS.csv")

# SELECTED
L5_selected <- L5 %>% 
  select(-TVD, -TotalBaseWaterVolume, -TotalBaseNonWaterVolume, -FederalWell, -IndianWell,
         -Source, -DTMOD, -PurposeKey, -IsWater, -PurposePercentHFJob, -PurposeIngredientMSDS, 
         -IngredientKey, -PercentHighAdditive, -PercentHFJob, 
         -IngredientComment, -IngredientMSDS, -MassIngredient, -ClaimantCompany, 
         -DisclosureKey)
write.csv(L5_selected, file="2 Cleaning/L5 Cleaning/L5 master table selected.csv")

# FORM DATA
L5_forms <- L5 %>%
  group_by(UploadKey) %>%
  mutate(WithholdingForm = sum(WithheldFlag) > 1) %>%         # 12/26/18 question: should be >= 1?
  ungroup() %>%
  distinct(UploadKey, .keep_all=TRUE) %>%
  select(UploadKey, JobStartDate, JobEndDate, APINumber, StateNumber, CountyNumber, OperatorName, WellName,
         Latitude, Longitude, Projection, StateName, CountyName, FFVersion, LatitudeClean, LongitudeClean,
         StateNameFromCoords, StateAbbFromCoords, StateNumberFromCoords, CountyNameFromCoords, CountyCodeFromCoords,
         StateOK, CountyOK, FormUsedSystemApproach, SubmissionsAvailable, WithholdingForm)
write.csv(L5_forms, file="2 Cleaning/L5 Cleaning/L5 forms data.csv")

