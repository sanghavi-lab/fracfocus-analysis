# A temporary fix to convert all Water entries to disclosed entries

# waters <- L5_chemicals %>%
#   select(UploadKey, OperatorName, WellName, IngredientName, CASNumber, CASNumberClean, CASLabel, Supplier,
#          SupplierClean, WithheldFlag, FormUsedSystemApproach, FromPDF) %>%
#   filter(WithheldFlag) %>%
#   filter(grepl(pattern="[Ww]ater", x=IngredientName))

names <- waters %>%
  group_by(IngredientName, CASLabel) %>%
  summarize(n=n()) %>%
  arrange(desc(n))
names


water_identifiers <- c(
  "Water (Including Mix Water Supplied by Client)*",
  "Water",
  "Carrier / Base Fluid - Water",
  "2% KCL Water",
  "Fresh Water",
  "Water (Including Mix Water Supplied by Client)",
  "Water, other",
  "Recycled Water ",
  "4% KCL Water",
  "Brine Water",
  "Lease Water",
  "NFIDB:2% KCL Water",
  "3% KCL Water",
  "Field Salt Water",
  "Produced Brine Water",
  "Tulare Water",
  "NFIDB:Lease Water",
  "water",
  "Water ",
  "KCl Water",
  "Produced Water",
  "1% KCL Water",
  "NFIDB:4% KCL Water",
  "NFIDB:Brine Water",
  "Recycled Water",
  "4% Salt Water",
  "10% Salt Water",
  "2% KCl Water",
  "NFIDB:3% KCL Water",
  "Water Moisture",
  "6% KCL Water",
  "fresh water",
  "NFIDB:3% NaCl Water",
  "NFIDB:6% KCL Water",
  "NFIDB:7% KCL Water",
  "18% Salt Water",
  "3% NaCl Water",
  "3% NACL Water",
  "4% NaCl Water",
  "5% KCl Water",
  "5% KCL Water",
  "7% KCL Water",
  "water, other",
  "2% KCl Lokern Water",
  "Brackish Water",
  "Brackish Water ",
  "NaCl Water",
  "NFIDB:15% Salt Water",
  "15% Salt Water",
  "3% Salt Water",
  "Fresh water",
  "KCL Water",
  "Lease Salt Water",
  "lease water",
  "NFIDB:5% KCL Water",
  "NFIDB:Water",
  "Produced Water ",
  "Production Water",
  "Salt Water",
  "Water (including mix water supplied by client)*",
  "Water, Other",
  " Water ",
  "1.5% KCL Water",
  "10% KCL Water",
  "3% KCl Water",
  "4% KCl Water",
  "4% NaCl Water ",
  "6% Salt Water ?",
  "Field Water",
  "NFIDB - 4 percent KCL Water",
  "NFIDB:1% KCL Water",
  "NFIDB:10% KCL Water",
  "NFIDB:10% Salt Water",
  "NFIDB:Sea Water",
  "produced Water",
  "Seawater",
  "Tulare  Water",
  "Water (Including Mix Water Supplied by Client).",
  "Water (including mix water supplied by Client)*",
  "Water (including Mix Water supplied by Client)*",
  "Water (major)",
  "Water, Including MIx water supplied by client",
  "Water,other",
  "water(including mix water supplied by client)*",
  "Water/Salt"
)




# get row numbers of appropriate waters
rows_to_change <- which((L5$IngredientName %in% water_identifiers) &
                          (L5$CASLabel %in% c("Invalid", "Not Available")))
for (r in rows_to_change) {
  L5$CASLabel[r] <- "Valid"
  L5$CASNumberClean[r] <- "7732-18-5"
  L5$IngredientNameClean[r] <- "Water"
  L5$WithheldFlag[r] <- FALSE
  print(r)
}


