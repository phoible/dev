#! /usr/bin/env Rscript

## This script reads in the raw data files from various tertiary sources, and
## aggregates them into a single R data.frame object called "phoible",
## which is then written out to the data directory.

library(stringi)  # for proper string handling & (de)normalization

## GLOBAL OPTIONS (restored at end)
saf <- getOption("stringsAsFactors")
options(stringsAsFactors=FALSE)
debug <- FALSE

## FILE PATHS
data_dir <- file.path("raw-data")
output_dir <- file.path("data")
glotto_path <- file.path("mappings", "InventoryID-LanguageCodes.csv")
output_path <- file.path(output_dir, "phoible-nofeats.csv")
output_path_rdata <- file.path(output_dir, "phoible-nofeats.RData")
if (!dir.exists(output_dir))  dir.create(output_dir, mode="0755")

## LOAD EXTERNAL FUNCTIONS
## provides: get_codepoints, order_ipa, remove_brackets, keep_first,
## parse_sparse, and validate_data. (also provides other functions not used
## here but that are used internally by the functions used here: denorm,
## make_typestring, fill_cells, create_glyph_type_variables, add_arch_stars,
## and remove_arch_stars)
source(file.path("scripts", "aggregation-helper-functions.R"))

## SOURCE DATA FILE PATHS
features_path <- file.path(data_dir, "FEATURES", "phoible-segments-features.tsv")
er_path <- file.path(data_dir, "ER", "ER_inventories.tsv")
ea_path <- file.path(data_dir, "EA", "EA_inventories.tsv")
ea_ipa_path <- file.path(data_dir, "EA", "EA_IPA_correspondences.tsv")
ph_path <- file.path(data_dir, "PH", "phoible_inventories.tsv")
uz_path <- file.path(data_dir, "UZ", "UZ_inventories.tsv")
gm_afr_path <- file.path(data_dir, "GM", "gm-afr-inventories.tsv")
gm_sea_path <- file.path(data_dir, "GM", "gm-sea-inventories.tsv")
aa_path <- file.path(data_dir, "AA", "AA_inventories.tsv")
spa_path <- file.path(data_dir, "SPA", "SPA_Phones.tsv")
spa_ipa_path <- file.path(data_dir, "SPA", "SPA_IPA_correspondences.tsv")
spa_iso_path <- file.path(data_dir, "SPA", "SPA_LangNamesCodes.tsv")
upsid_segments_path <- file.path(data_dir, "UPSID", "UPSID_Segments.tsv")
upsid_lang_codes_path <- file.path(data_dir, "UPSID", "UPSID_LanguageCodes.tsv")
upsid_ipa_path <- file.path(data_dir, "UPSID", "UPSID_IPA_correspondences.tsv")
ra_path <- file.path(data_dir, "RA", "Ramaswami1999.csv")
saphon_path <- file.path(data_dir, "SAPHON", "saphon20121031.tsv")
saphon_ipa_path <- file.path(data_dir, "SAPHON", "saphon_ipa_correspondences.tsv")

## ## ## ## ## ##
##  LOAD DATA  ##
## ## ## ## ## ##

## Australian Phonologies inventory data. All columns are dense:
cat("processing ER\n")
er_raw <- read.delim(er_path, na.strings="", blank.lines.skip=FALSE)
er_data <- validate_data(er_raw, "er", debug=debug)
if (!debug) rm(er_raw)

## Eurasian Phonologies inventory data. All columns are dense:
## InventoryID, LanguageCode, LanguageName, Phoneme
## Dialect information is (sometimes) included parenthetically in the
## LanguageName field (like in AA/SAPHON)
cat("processing EA\n")
ea_raw <- read.delim(ea_path, na.strings="", blank.lines.skip=FALSE)
ea_ipa <- read.delim(ea_ipa_path, na.strings="", quote="")
## If the "LanguageName" column has parenthetical info, copy language name to
## "SpecificDialect" and remove parenthetical from "LanguageName"
name_has_parens <- stri_detect_fixed(ea_raw$LanguageName, "(")
ea_raw$SpecificDialect <- ifelse(name_has_parens, ea_raw$LanguageName, NA)
ea_raw$LanguageName <- keep_first(ea_raw$LanguageName, split.on=" (")
ea_raw <- merge(ea_raw, ea_ipa, all.x=TRUE, all.y=FALSE)
## clean up
ea_data <- validate_data(ea_raw, "ea", debug=debug)
if(!debug) rm(ea_raw, ea_ipa)

## PH has only first cell filled in several columns.
## Only column guaranteed unique for each inventory is FileNames
cat("processing PH\n")
ph_raw <- read.delim(ph_path, na.strings="", blank.lines.skip=TRUE)
sparse_cols <- c("InventoryID", "LanguageCode", "LanguageName", "Phoneme",
                 "SpecificDialect", "FileNames")
ph_data <- parse_sparse(ph_raw, id_col="FileNames", fill_cols=sparse_cols)
## clean up
ph_data <- validate_data(ph_data, "ph", debug=debug)
if (!debug) rm(ph_raw)

## UZ has only first cell filled in several columns.
## Only column guaranteed unique for each inventory is FileNames
cat("processing UZ\n")
uz_raw <- read.delim(uz_path, na.strings="", blank.lines.skip=TRUE)
sparse_cols <- c("InventoryID", "LanguageCode", "LanguageName", "Phoneme",
                 "SpecificDialect", "FileNames")
uz_data <- parse_sparse(uz_raw, id_col="FileNames", fill_cols=sparse_cols)
## clean up
uz_data <- validate_data(uz_data, "uz", debug=debug)
if (!debug) rm(uz_raw)

## GM has dense lx.code, name, and dialect columns, but sparse FileNames column.
## Only column guaranteed unique for each inventory is FileNames.
cat("processing GM\n")
gm_afr_raw <- read.delim(gm_afr_path, na.strings="", quote="",
                         blank.lines.skip=FALSE)
gm_sea_raw <- read.delim(gm_sea_path, na.strings="", quote="",
                         blank.lines.skip=FALSE)
gm_raw <- rbind(gm_afr_raw, gm_sea_raw)
gm_data <- parse_sparse(gm_raw, id_col="InventoryID", fill_cols="FileNames")
## clean up
gm_data <- validate_data(gm_data, "gm", debug=debug)
if (!debug) rm(gm_raw, gm_afr_raw, gm_sea_raw)

## AA has blank lines between languages; InventoryID is sparse and unique; all
## other columns are dense.
cat("processing AA\n")
aa_raw <- read.delim(aa_path, na.strings="", blank.lines.skip=TRUE)
aa_raw$InventoryID <- zoo::na.locf(aa_raw$InventoryID)
## If the "LanguageName" column has parenthetical info, copy language name to
## "SpecificDialect" and remove parenthetical from "LanguageName"
name_has_parens <- stri_detect_fixed(aa_raw$LanguageName, "(")
aa_raw$SpecificDialect <- ifelse(name_has_parens, aa_raw$LanguageName, NA)
aa_raw$LanguageName <- keep_first(aa_raw$LanguageName, split.on=" (")
## clean up
aa_data <- validate_data(aa_raw, "aa", debug=debug)
if (!debug) rm(aa_raw)

## SPA has sparse columns: spaLangNum, LanguageName, spaPhoneNum, spaDescription
## but no blank lines between inventories.
cat("processing SPA\n")
spa_raw <- read.delim(spa_path, na.strings="", quote="")
spa_iso <- read.delim(spa_iso_path, na.strings="", quote="")
spa_ipa <- read.delim(spa_ipa_path, na.strings="", quote="")
spa_ipa <- spa_ipa[c("spaDescription", "Phoneme")]
## assign temporary integer ID and fill sparsities
sparse_cols <- c("spaLangNum", "LanguageName", "spaDescription")
spa_data <- parse_sparse(spa_raw, id_col="InventoryID", fill_cols=sparse_cols)
## Merge in ISO codes and IPA representation of phonemes and allophones
spa_data <- merge(spa_data, spa_iso, all.x=TRUE, sort=FALSE)
spa_data <- merge(spa_data, spa_ipa, all.x=TRUE, sort=FALSE)
spa_data$spaAllophoneDescription <- remove_brackets(spa_data$spaAllophoneDescription,
                                                    type="square")
spa_data$Allophones <- spa_ipa$Phoneme[match(spa_data$spaAllophoneDescription,
                                             spa_ipa$spaDescription)]
## SPA does not include marginal phonemes; we denote this with NA
spa_data$Marginal <- NA
## clean up
spa_data <- validate_data(spa_data, "spa", debug=debug)
if (!debug) rm(spa_raw, spa_ipa, spa_iso)

## UPSID has database-like tables, so we basically just merge things
cat("processing UPSID\n")
upsid_lang_codes <- read.delim(upsid_lang_codes_path, na.strings="", quote="")
upsid_segments <- read.delim(upsid_segments_path, na.strings="", quote="")
upsid_ipa <- read.delim(upsid_ipa_path, na.strings="", quote="")
upsid_ipa <- upsid_ipa[c("upsidCCID", "Phoneme")]
upsid_data <- merge(upsid_lang_codes, upsid_segments, by="upsidLangNum")
upsid_data <- merge(upsid_data, upsid_ipa, by="upsidCCID")
## add column for marginal phonemes
upsid_data$Marginal <- as.logical(upsid_data$anomalous)
## clean up
upsid_data <- validate_data(upsid_data, "upsid", debug=debug)
if (!debug) rm(upsid_ipa, upsid_segments, upsid_lang_codes)

## RAMASWAMI is a wide-format data source: 1 row per language, phonemes as
## column headers, with boolean presence/absence indicators in the cells.
## There is an integer ID in the first column of the raw data.
cat("processing RA\n")
ra_raw <- read.csv(ra_path, na.strings="", quote="", as.is=TRUE, header=FALSE)
ra_data <- apply(ra_raw[4:nrow(ra_raw),], 1, function (i)
    data.frame(InventoryID=i[1], LanguageName=i[3], LanguageCode=i[4],
               Phoneme=c(t(ra_raw[2, 5:length(i)][as.logical(as.numeric(i[5:length(i)]))])),
               row.names=NULL))
ra_data <- do.call(rbind, ra_data)
## RA does not indicate marginal phonemes; we denote this with NA
ra_data$Marginal <- NA
## clean up
ra_data <- validate_data(ra_data, "ra", debug=debug)
if (!debug) rm(ra_raw)

## SAPHON is a wide-format data source: 1 row per language, phonemes as column
## headers, with boolean presence indicators in the cells (absence = "").
## NOTE: this code is fragile, and is built for saphon20121031.tsv, which was
## hand-corrected from the original CSV version to remove extraneous line 
## breaks and quotes, and convert delimiters from comma to tab (several cells
## had internal commas). Future releases of SAPHON may break this code.
cat("processing SAPHON\n")
saphon_ipa <- read.delim(saphon_ipa_path, as.is=TRUE, header=TRUE)
saphon_ipa$IPA <- order_ipa(saphon_ipa$IPA)
saphon_raw <- read.delim(saphon_path, na.strings="", quote="", as.is=TRUE,
                         header=FALSE, row.names=NULL)
saphon_starting_row <- 3
saphon_phoneme_cols <- 18:342  # column 343: +/- tone, 344: +/- nasal harmony
## collect the list of possible phonemes and convert to IPA
saphon_phones <- as.vector(t(saphon_raw[1, saphon_phoneme_cols]))
saphon_phones <- saphon_ipa$IPA[match(saphon_phones, saphon_ipa$SAPHON)]
saphon_phones <- order_ipa(saphon_phones)
## fill in empty cells with 0
saphon_raw[is.na(saphon_raw)] <- "0"
## for each language, extract inventoryID, name, ISO code, and phonemes
## with a "1" in their column.
saphon_data <- lapply(saphon_starting_row:nrow(saphon_raw), function (i)
    data.frame(LanguageName=saphon_raw[i, 2], LanguageCode=saphon_raw[i, 6],
               Phoneme=saphon_phones[as.logical(as.integer(saphon_raw[i, saphon_phoneme_cols]))],
               InventoryID=saphon_raw[i, 1], row.names=NULL))
saphon_data <- do.call(rbind, saphon_data)
## discard dialect information from ISO codes ("lang_dialect" -> "lang")
iso_has_dialect <- stri_detect_fixed(saphon_data$LanguageCode, "_")
saphon_data$LanguageCode <- keep_first(saphon_data$LanguageCode, split.on="_")
## handle entries that have two ISO codes ("lga lgb" -> "lga")
saphon_data$LanguageCode <- keep_first(saphon_data$LanguageCode, split.on=" ")
## extract parenthetical dialect from LanguageName, if present
name_has_parens <- stri_detect_fixed(saphon_data$LanguageName, "(")
saphon_data$SpecificDialect <- sapply(stri_split_regex(saphon_data$LanguageName, "[()]"),
                                      function (x) ifelse(length(x) > 1, x[2], ""))
saphon_data$LanguageName <- keep_first(saphon_data$LanguageName, split.on=" (")
## handle dialects that don't have parenthetical indications in LanguageName
named_dialect <- iso_has_dialect & !name_has_parens
## make sure we're not overwriting non-empty cells in SpecificDialect column
overlap <- intersect(which(saphon_data$SpecificDialect != ""),
                     which(named_dialect))
if (length(overlap)) warning(length(overlap), "non-empty entries in SAPHON",
                             "SpecificDialect column have been overwritten.")
saphon_data$SpecificDialect[named_dialect] <- saphon_data$LanguageName[named_dialect]
## SAPHON does not indicate marginal phonemes; we denote this with NA
saphon_data$Marginal <- NA
## clean up
saphon_data <- validate_data(saphon_data, "saphon", debug=debug)
if (!debug) rm(saphon_raw, saphon_ipa)


## ## ## ## ## ## ## ## ##
## COMBINE DATA SOURCES ##
## ## ## ## ## ## ## ## ##

## combine into one data frame
data_sources_list <- list(ph_data, aa_data, spa_data, upsid_data, ra_data,
                          gm_data, saphon_data, uz_data, ea_data, er_data)
all_data <- do.call(rbind, data_sources_list)
all_data <- all_data[order(all_data$InventoryID),]

## MERGE IN GLOTTOLOG CODES
glotto_mapping <- read.csv(glotto_path)
glotto_mapping <- glotto_mapping[c("InventoryID", "Glottocode", "ISO6393")]
all_data <- merge(all_data, glotto_mapping, all.x=TRUE)

## ADD GLYPH IDs
all_data$GlyphID <- get_codepoints(all_data$Phoneme)

## CONVERT INVENTORY ID TO INTEGER
all_data$InventoryID <- as.integer(all_data$InventoryID)

## SAVE
output_fields <- c("InventoryID", "Glottocode", "ISO6393", "LanguageName",
                   "SpecificDialect", "GlyphID", "Phoneme", "Allophones",
                   "Marginal", "SegmentClass", "Source")
phoible_nofeats <- all_data[output_fields]
write.csv(phoible_nofeats, file=output_path, row.names=FALSE, quote=TRUE,
          eol="\n", fileEncoding="UTF-8")
save(phoible_nofeats, file=output_path_rdata)
## WRITE LOG FILE
if(exists("unfamiliar_glyphs")) {
    log_path <- file.path(output_dir, "unfamiliar-glyphs.csv")
    write.csv(unfamiliar_glyphs, file=log_path, row.names=FALSE, quote=FALSE,
              eol="\n", fileEncoding="UTF-8")
}

## RESET GLOBAL OPTIONS
options(stringsAsFactors=saf)
