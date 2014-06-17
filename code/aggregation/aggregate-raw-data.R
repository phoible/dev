#! /usr/bin/R
### Aggregate various source data files into the PHOIBLE framework ###
library(zoo)   # provides function na.locf (last observ. carry forward)
library(plyr)  # provides function rbind.fill

# the definition of datadir assumes that the GitHub "code" and "data" 
# repos were cloned into the same parent directory, and this script is
# running from its original location in "data/R/"
datadir <- "../../data"
scriptdir <- getwd()  # save for later

# # # # # # # #
# FILE PATHS  #
# # # # # # # #
features.path <- paste(datadir, "FEATURES", "phoible-segments-features.tsv", sep="/")
uw.path <- paste(datadir, "UW", "phoible_inventories.tsv", sep="/")
aa.path <- paste(datadir, "AA", "AA_inventories.tsv", sep="/")
spa.path <- paste(datadir, "SPA", "SPA_Phones.tsv", sep="/")
spa.ipa.path <- paste(datadir, "SPA", "SPA_IPA_correspondences.tsv", sep="/")
spa.iso.path <- paste(datadir, "SPA", "SPA_LangNamesCodes.tsv", sep="/")
upsid.segments.path <- paste(datadir, "UPSID", "UPSID_Segments.tsv", sep="/")
upsid.character.codes.path <- paste(datadir, "UPSID", "UPSID_CharCodes.tsv", sep="/")
upsid.languages.path <- paste(datadir, "UPSID", "UPSID_Languages.tsv", sep="/")
upsid.language.codes.path <- paste(datadir, "UPSID", "UPSID_LanguageCodes.tsv", sep="/")
upsid.ipa.path <- paste(datadir, "UPSID", "UPSID_IPA_correspondences.tsv", sep="/")
ramaswami.path <- paste(datadir, "RAMASWAMI", "Ramaswami1999.csv", sep="/")
# CASL
# SAPHON
# OCEANIA
# STEDT


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
			#h$Allophones <- gsub("[", "", gsub("]", "", 
			#				paste(as.vector(j$Allophones), collapse=" "), 
			#				fixed=TRUE), fixed=TRUE);
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


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION TO REMOVE DUPLICATE LANGUAGE DATA (RESPECTING TRUMP ORDER) #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
remove.duplicate.langs <- function(x, col) {
	if(length(unique(x[col])) > 1) x <- x[x[col] == min(x[col]),]
	return(x)
}


# # # # # # # # #
# DATA SOURCES  #
# # # # # # # # #
# UW has only first cell filled in various columns
uw.raw <- read.delim(uw.path, na.strings="", stringsAsFactors=FALSE)
uw.raw$LanguageCode <- na.locf(uw.raw$LanguageCode)
uw.split <- split(uw.raw, uw.raw$LanguageCode)
# using 'split' before 'fillCells' prevents things like 'SpecificDialect'
# from copying beyond the row extent of each LanguageCode
uw.data <- unsplit(lapply(uw.split, fillCells, c("LanguageName", 
		   "SpecificDialect", "Phoneme", "FileNames")), 
		   uw.raw$LanguageCode)
uw.data <- collapseAllophones(uw.data, "LanguageCode")
uw.data$source <- "uw"
rm(uw.raw, uw.split)

# AA has blank lines between languages, but no gaps like in UW or SPA
# AA lists marginal phonemes in angle brackets like this <h>
# use "LanguageName" to collapse allophones: there are specific dialects
aa.data <- read.delim(aa.path, na.strings="", blank.lines.skip=TRUE,
					  stringsAsFactors=FALSE)
aa.data <- collapseAllophones(aa.data, "LanguageName")
aa.data$source <- "aa"


# SPA
# TODO: SPA data does not have ISO codes; is there a mapping to SPA language numbers?
# TODO: the SPA "Notes" column just has numeric codes in it. where is the key?
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
spa.data$source <- "spa"
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
upsid.data$source <- "upsid"
rm(upsid.ipa, upsid.segments, upsid.language.codes)

# RAMASWAMI
ramaswami.raw <- read.delim(ramaswami.path, na.strings="", quote="", 
				 as.is=TRUE, header=FALSE, stringsAsFactors=FALSE)
ramaswami.data <- apply(ramaswami.raw[4:nrow(ramaswami.raw),], 1, 
				  function(i) data.frame(LanguageName=i[2], LanguageCode=i[3], 
				  Phoneme=c(t(ramaswami.raw[2, 4:length(i)][as.logical(as.numeric(i[4:length(i)]))])), 
				  row.names=NULL))
ramaswami.data <- do.call(rbind, ramaswami.data)
ramaswami.data$source <- "ramaswami"
rm(ramaswami.raw)

# CASL

# SAPHON

# OCEANIA

# STEDT


# # # # # # # # # # # # #
# COMBINE DATA SOURCES  #
# # # # # # # # # # # # #
# COMBINE INTO ONE DATA FRAME
data.sources.list <- list(uw.data, aa.data, spa.data, upsid.data, ramaswami.data)  # casl.data, saphon.data
all.data <- do.call(rbind.fill, data.sources.list)
all.data <- all.data[with(all.data, order(LanguageCode, source)),]
# HANDLE MARGINAL PHONEMES
all.data$Marginal <- "<" %in% all.data$Phoneme
all.data$Phoneme <- gsub("<", "", gsub(">", "", all.data$Phoneme, fixed=TRUE), fixed=TRUE)
# REPLACE C-CEDILLA BASE+DIACRITIC WITH SINGLE GRAPH
all.data$Phoneme <- gsub("ç", "ç", all.data$Phoneme, fixed=TRUE)
# REMOVE ALL TIEBARS
all.data$Phoneme <- gsub("͡", "", all.data$Phoneme, fixed=TRUE)
all.data$Phoneme <- gsub("͜", "", all.data$Phoneme, fixed=TRUE)

# FACTOR AFTER SUBSTITUTIONS
all.data$Phoneme <- factor(all.data$Phoneme)


# VALIDATE ISO CODES



# LOAD THE FEATURES AND IMPLEMENT THE RULES
feats <- read.delim(features.path, sep='\t', stringsAsFactors=TRUE)
feat.columns <- c("tone", "stress", "syllabic", "short", "long", 
				  "consonantal", "sonorant", "continuant", 
				  "delayedRelease", "approximant", "tap", "trill", 
				  "nasal", "lateral", "labial", "round", "labiodental", 
				  "coronal", "anterior", "distributed", "strident", 
				  "dorsal", "high", "low", "front", "back", "tense", 
				  "retractedTongueRoot", "advancedTongueRoot", 
				  "periodicGlottalSource", "epilaryngealSource", 
				  "spreadGlottis", "constrictedGlottis", "fortis", 
				  "raisedLarynxEjective", "loweredLarynxImplosive", 
				  "click")
all.data <- merge(all.data, feats, by.x="Phoneme", by.y="segment", all.x=TRUE, all.y=FALSE, sort=FALSE)
# handle UPSID disjuncts
upsid.disjunct.indices <- grepl("|", all.data$Phoneme, fixed=TRUE)
upsid.disjuncts <- strsplit(as.character(all.data$Phoneme[upsid.disjunct.indices]), split="|", fixed=TRUE)
upsid.feats <- do.call(rbind, lapply(upsid.disjuncts, function(i) {
			   left.index <- which(feats$segment == i[1])
			   right.index <- which(feats$segment == i[2])
			   matches <- unlist(lapply(seq_along(feats[left.index,]), function(i) feats[left.index, i] == feats[right.index, i]))
			   output <- feats[left.index,]
			   output[!matches] <- 0
			   output$segment <- paste(i, collapse="|")
			   return(output)
			   }))
all.data[upsid.disjunct.indices, feat.columns] <- upsid.feats[feat.columns]

# TEMPORARY CODE FOR DEBUGGING
# still a few phonemes without features
missing.feats <- all.data[is.na(all.data$syllabic),]
#sink("~/Desktop/featurelessPhonemes.tsv")
cat(paste(c("phonemes without feature vectors:",
          unique(as.character(missing.feats$Phoneme))), collapse="\n"))
#sink()

# TRUMP ORDERING: more preferred data sources come earlier in the list
trump.order <- c("uw", "spa", "aa", "upsid", "ramaswami")  # "casl", "saphon"
all.data$source <- factor(all.data$source, levels=trump.order, ordered=TRUE)
split.data <- split(all.data, all.data$LanguageCode)
reduced.data <- lapply(split.data, remove.duplicate.langs, "source")
reduced.data <- do.call(rbind, reduced.data)
rownames(reduced.data) <- NULL


# address specific problems between with features
# put it all in for one, except for the diacritics
# Another method will need to be built to output the data with (or linked to) allophone and the explanation of allophones data?
