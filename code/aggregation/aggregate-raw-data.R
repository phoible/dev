#! /usr/bin/R

# This script reads in the raw data files from various tertiary sources, and 
# aggregates them into a single R data.frame object called "all.data",
# which is then written out to the root directory of the repository.

library(zoo)      # provides function na.locf (last observ. carry forward)
library(plyr)     # provides function rbind.fill
library(stringi)  # for proper string handling & (de)normalization

data.dir <- file.path("..", "..", "data")
output.fname <- file.path("..", "..", "phoible-alldata-phonemes.tsv")

# # # # # # # #
# FILE PATHS  #
# # # # # # # #
features.path <- file.path(data.dir, "FEATURES", "phoible-segments-features.tsv")
uw.path <- file.path(data.dir, "UW", "phoible_inventories.tsv")
aa.path <- file.path(data.dir, "AA", "AA_inventories.tsv")
spa.path <- file.path(data.dir, "SPA", "SPA_Phones.tsv")
spa.ipa.path <- file.path(data.dir, "SPA", "SPA_IPA_correspondences.tsv")
spa.iso.path <- file.path(data.dir, "SPA", "SPA_LangNamesCodes.tsv")
upsid.segments.path <- file.path(data.dir, "UPSID", "UPSID_Segments.tsv")
upsid.character.codes.path <- file.path(data.dir, "UPSID", "UPSID_CharCodes.tsv")
upsid.languages.path <- file.path(data.dir, "UPSID", "UPSID_Languages.tsv")
upsid.language.codes.path <- file.path(data.dir, "UPSID", "UPSID_LanguageCodes.tsv")
upsid.ipa.path <- file.path(data.dir, "UPSID", "UPSID_IPA_correspondences.tsv")
ramaswami.path <- file.path(data.dir, "RAMASWAMI", "Ramaswami1999.csv")
# TODO: CASL
# TODO: SAPHON
# TODO: OCEANIA
# TODO: STEDT

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION TO PROPOGATE PHONEMES, LANGUAGE, DIALECT, FILENAMES, ETC #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
fillCells <- function(df, cols) {
	for(col in cols) {
		if(!is.na(match(col, colnames(df)))) {
			thiscol <- df[[match(col, colnames(df))]]
			if(!is.na(thiscol[1])) thiscol <- na.locf(thiscol)
			df[[match(col, colnames(df))]] <- thiscol
		} else {
			stop("Bad column name passed to 'fillCells'.")
		}
	}
	return(df)
} 

# # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION TO COLLAPSE ALLOPHONES TO A SINGLE CELL  #
# # # # # # # # # # # # # # # # # # # # # # # # # # #
collapseAllophones <- function(x, col) {
	# x is the data frame, col is the name of the column to split on
	# (col should typically be "LanguageCode")
	spl <- lapply(split(x, x[col]), function(i) split(i, i$Phoneme))
	spl <- lapply(spl, function(i) lapply(i, function(j) { 
			h <- j;  
			h$Allophones <- gsub("[", "", gsub("]", "", 
							paste(as.vector(ifelse(is.na(j$Allophones), 
							j$Phoneme, j$Allophones)), collapse=" "), 
							fixed=TRUE), fixed=TRUE);
			h$AllophoneNotes <- gsub("\"", "", 
								paste(as.vector(j$AllophoneNotes), 
								collapse="; "), fixed=TRUE)
			return(h[1,]) 
			}))
	mrg <- do.call(rbind, lapply(spl, function(i) do.call(rbind, i)))
	rownames(mrg) <- NULL
	return(mrg)
	# TODO: inconsistent results across data sources
	# (see https://github.com/uzling/code/issues/3)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION TO DENORMALIZE AND THEN RE-NORMALIZE UNICODE STRINGS #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
denormRenorm <- function(x) {
    s <- stri_trans_general(stri_trans_general(x, "Any-NFD"), "Any-NFC")
}

# # # # # # # # #
# DATA SOURCES  #
# # # # # # # # #
# UW has only first cell filled in various columns
uw.raw <- read.delim(uw.path, na.strings="", stringsAsFactors=FALSE)
uw.raw$Allophones <- denormRenorm(uw.raw$Allophones)
uw.raw$LanguageCode <- na.locf(uw.raw$LanguageCode)
uw.split <- split(uw.raw, uw.raw$LanguageCode)
# using 'split' before 'fillCells' prevents things like 'SpecificDialect'
# from copying beyond the row extent of each LanguageCode
uw.data <- unsplit(lapply(uw.split, fillCells, c("LanguageName", 
		   "SpecificDialect", "Phoneme", "FileNames")), 
		   uw.raw$LanguageCode)
uw.data <- collapseAllophones(uw.data, "LanguageCode")
uw.data$Source <- "uw"
rm(uw.raw, uw.split)

# AA has blank lines between languages, but no gaps like in UW or SPA
# AA lists marginal phonemes in angle brackets like this <h>
# use "LanguageName" to collapse allophones: there are specific dialects
aa.data <- read.delim(aa.path, na.strings="", blank.lines.skip=TRUE,
					  stringsAsFactors=FALSE)
aa.data$Allophones <- denormRenorm(aa.data$Allophones)
aa.data <- collapseAllophones(aa.data, "LanguageName")
aa.data$Source <- "aa"


# SPA
# TODO: SPA data does not have ISO codes; is there a mapping to SPA language numbers?
# TODO: the SPA "Notes" column just has numeric codes in it. where is the key?
# TODO: SPA has allophone information, but not in IPA?
spa.ipa <- read.delim(spa.ipa.path, na.strings="", stringsAsFactors=FALSE, quote="")
spa.iso <- read.delim(spa.iso.path, na.strings="", stringsAsFactors=FALSE, quote="")
spa.raw <- read.delim(spa.path, na.strings="", stringsAsFactors=FALSE, quote="")
spa.raw$spaLangNum <- na.locf(spa.raw$spaLangNum)
spa.split <- split(spa.raw, spa.raw$spaLangNum)
spa.data <- unsplit(lapply(spa.split, fillCells, c("LanguageName", 
			"spaPhoneNum", "spaDescription")), spa.raw$spaLangNum)
spa.data <- merge(spa.data, spa.ipa, by="spaDescription", sort=FALSE)
spa.data$spaAllophoneDescription <- gsub("[", "", gsub("]", "", 
									spa.data$spaAllophoneDescription, 
									fixed=TRUE), fixed=TRUE)
spa.data$Allophones <- spa.ipa$Phoneme[match(spa.data$spaAllophoneDescription,
									   spa.ipa$spaDescription)]
spa.data <- collapseAllophones(spa.data, "LanguageName")
spa.data <- merge(spa.data, spa.iso)
spa.data$Source <- "spa"
rm(spa.raw, spa.split, spa.ipa, spa.iso)

# UPSID
upsid.ipa <- read.delim(upsid.ipa.path, na.strings="", 
			 stringsAsFactors=FALSE, quote="")
upsid.segments <- read.delim(upsid.segments.path, na.strings="", 
				  stringsAsFactors=FALSE, quote="")
upsid.language.codes <- read.delim(upsid.language.codes.path, 
						na.strings="", stringsAsFactors=FALSE, quote="")
upsid.data <- merge(upsid.language.codes, upsid.segments, by="upsidLangNum")
upsid.data <- merge(upsid.data, upsid.ipa[c("upsidCCID", "Phoneme")], by="upsidCCID")
upsid.data$Source <- "upsid"
rm(upsid.ipa, upsid.segments, upsid.language.codes)

# RAMASWAMI
ramaswami.raw <- read.delim(ramaswami.path, na.strings="", quote="", 
				 as.is=TRUE, header=FALSE, stringsAsFactors=FALSE)
ramaswami.data <- apply(ramaswami.raw[4:nrow(ramaswami.raw),], 1, 
				  function(i) data.frame(LanguageName=i[2], LanguageCode=i[3], 
				  Phoneme=c(t(ramaswami.raw[2, 4:length(i)][as.logical(as.numeric(i[4:length(i)]))])), 
				  row.names=NULL))
ramaswami.data <- do.call(rbind, ramaswami.data)
ramaswami.data$Source <- "ramaswami"
rm(ramaswami.raw)

# TODO: CASL

# TODO: SAPHON

# TODO: OCEANIA

# TODO: STEDT


# # # # # # # # # # # # #
# COMBINE DATA SOURCES  #
# # # # # # # # # # # # #
# COMBINE INTO ONE DATA FRAME
data.sources.list <- list(uw.data, aa.data, spa.data, upsid.data,
                          ramaswami.data)  # casl.data, saphon.data
all.data <- do.call(rbind.fill, data.sources.list)
all.data <- all.data[with(all.data, order(LanguageCode, Source)),]
# MARK MARGINAL PHONEMES
all.data$Marginal <- stri_count_fixed(all.data$Phoneme, "<") > 0
all.data$Phoneme <- stri_replace_all_fixed(all.data$Phoneme, replacement="", pattern="<")
all.data$Phoneme <- stri_replace_all_fixed(all.data$Phoneme, replacement="", pattern=">")
# REMOVE ALL TIEBARS
all.data$Phoneme <- stri_replace_all_fixed(all.data$Phoneme, replacement="", pattern="͡")
all.data$Phoneme <- stri_replace_all_fixed(all.data$Phoneme, replacement="", pattern="͜")
# NORMALIZATION
all.data$Phoneme <- denormRenorm(all.data$Phoneme)
all.data$Allophones <- denormRenorm(all.data$Allophones)
# FACTOR TO DROP UNUSED LEVELS
all.data$Phoneme <- factor(all.data$Phoneme)
# ASSIGN GLYPH IDs
all.data$GlyphID <- stri_trans_general(all.data$Phoneme, "Any-Hex/Unicode")
all.data$GlyphID <- stri_replace_all_fixed(all.data$GlyphID, replacement="", pattern="U")
all.data$GlyphID <- stri_replace_first_fixed(all.data$GlyphID, replacement="", pattern="+")

# TODO: VALIDATE ISO CODES

# WRITE OUT DATA
# TODO: need to implement Class column
output.columns <- c("LanguageCode", "LanguageName", "SpecificDialect", 
                    "Phoneme", "Allophones", "Source", "GlyphID")
write.table(all.data[,output.columns], file=output.fname, sep="\t", eol="\n",
            row.names=FALSE, quote=FALSE, fileEncoding="UTF-8")
