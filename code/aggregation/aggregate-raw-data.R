#! /usr/bin/env Rscript

## This script reads in the raw data files from various tertiary sources, and
## aggregates them into a single R data.frame object called "all.data",
## which is then written out to the root directory of the repository.

library(zoo)      # provides function na.locf (last observ. carry forward)
library(plyr)     # provides function rbind.fill
library(stringi)  # for proper string handling & (de)normalization

## ## ## ##
## SETUP ##
## ## ## ##
## set global options (to be restored at end)
saf <- getOption("stringsAsFactors")
options(stringsAsFactors=FALSE)
## file paths
root.dir <- file.path("..", "..")
data.dir <- file.path(root.dir, "data")
## output filenames
output.fname <- file.path(root.dir, "phoible-phoneme-level.tsv")
output.rdata <- file.path(root.dir, "phoible-phoneme-level.RData")

## WHICH DATA COLUMNS TO KEEP (FEATURE COLUMNS GET ADDED LATER)
output.fields <- c("LanguageCode", "LanguageName", "SpecificDialect",
                   "Phoneme", "Allophones", "Source", "GlyphID", "InventoryID")

## TRUMP ORDERING (for choosing which entry to keep when there are multiple
## entries for a language). Preferred data sources come earlier in the list.
trump.order <- c("ph", "gm", "spa", "aa", "upsid", "ra", "saphon")
apply.trump <- FALSE

## SOURCE DATA FILE PATHS
features.path <- file.path(data.dir, "FEATURES", "phoible-segments-features.tsv")
ph.path <- file.path(data.dir, "PH", "phoible_inventories.tsv")
aa.path <- file.path(data.dir, "AA", "AA_inventories.tsv")
spa.path <- file.path(data.dir, "SPA", "SPA_Phones.tsv")
spa.ipa.path <- file.path(data.dir, "SPA", "SPA_IPA_correspondences.tsv")
spa.iso.path <- file.path(data.dir, "SPA", "SPA_LangNamesCodes.tsv")
upsid.segments.path <- file.path(data.dir, "UPSID", "UPSID_Segments.tsv")
upsid.character.codes.path <- file.path(data.dir, "UPSID", "UPSID_CharCodes.tsv")
upsid.languages.path <- file.path(data.dir, "UPSID", "UPSID_Languages.tsv")
upsid.language.codes.path <- file.path(data.dir, "UPSID", "UPSID_LanguageCodes.tsv")
upsid.ipa.path <- file.path(data.dir, "UPSID", "UPSID_IPA_correspondences.tsv")
ra.path <- file.path(data.dir, "RA", "Ramaswami1999.csv")
gm.afr.path <- file.path(data.dir, "GM", "gm-afr-inventories.tsv")
gm.sea.path <- file.path(data.dir, "GM", "gm-sea-inventories.tsv")
saphon.path <- file.path(data.dir, "SAPHON", "saphon20121031.tsv")
saphon.ipa.path <- file.path(data.dir, "SAPHON", "saphon_ipa_correspondences.tsv")
## TODO: OCEANIA
## TODO: STEDT
## TODO: JIPA

## ## ## ## ## ##
##  FUNCTIONS  ##
## ## ## ## ## ##

## unicode normalization functions
denorm <- function(x) {
    s <- stri_trans_general(x, "Any-NFD")
}
denormRenorm <- function(x) {
    s <- stri_trans_general(stri_trans_general(x, "Any-NFD"), "Any-NFC")
}

## mark marginal phonemes in a boolean column and remove <angle brackets>
markMarginal <- function(x) {
    x$Marginal <- stri_detect_fixed(x$Phoneme, "<")
    x$Phoneme <- stri_replace_all_fixed(x$Phoneme, replacement="", pattern="<")
    x$Phoneme <- stri_replace_all_fixed(x$Phoneme, replacement="", pattern=">")
    return(x)
}

## propogate values through "long and sparse" data sources
fillCells <- function(df, cols) {
	for(col in cols) {
		if(!is.na(match(col, colnames(df)))) {
		    thiscol <- df[[col]]
		    if(!is.na(thiscol[1])) thiscol <- na.locf(thiscol)
		    df[[col]] <- thiscol
		} else {
			stop("Bad column name passed to 'fillCells'.")
		}
	}
	return(df)
}

## remove square brackets from allophones and collapse to a single cell
collapseAllophones <- function(x, col) {
	# x is the data frame, col is the name of the column to split on
    # (col should typically be "InventoryID"). The first line splits
    # the data by "col", then within "col" it splits again by SpecificDialect
    # and then again by Phoneme.
    spl <- split(x, x[[col]])
    spt <- lapply(spl, function(i) {
        if(length(unique(i$SpecificDialect)) > 1) {
            dial <- split(i, i$SpecificDialect)
            lect <- lapply(dial, function(j) {
                phon <- split(j, j$Phoneme)
                emes <- lapply(phon, function(k) {
                    h <- k[1,]
                    h$Allophones <- ifelse(is.na(h$Allophones), h$Phoneme,
                                           paste(as.vector(k$Allophones),
                                                 collapse=" "))
                    h$Allophones <- stri_replace_all_fixed(h$Allophones,
                                                           replacement="",
                                                           pattern="[")
                    h$Allophones <- stri_replace_all_fixed(h$Allophones,
                                                           replacement="",
                                                           pattern="]")
                    tmp <- paste(as.vector(k$AllophoneNotes), collapse="; ")
                    h$AllophoneNotes <- stri_replace_all_fixed(tmp,
                                                               replacement="",
                                                               pattern="\"")
                    return(h)
                    })
                emes <- do.call(rbind, emes)
                })
            collapsed <- do.call(rbind, lect)
        } else {
            phon <- split(i, i$Phoneme)
            collapsed <- lapply(phon, function(j) {
                h <- j[1,];
                h$Allophones <- ifelse(is.na(h$Allophones), h$Phoneme,
                                       paste(as.vector(j$Allophones),
                                             collapse=" "));
                h$Allophones <- stri_replace_all_fixed(h$Allophones, replacement="",
                                                       pattern="[");
                h$Allophones <- stri_replace_all_fixed(h$Allophones, replacement="",
                                                       pattern="]");
                tmp <- paste(as.vector(j$AllophoneNotes), collapse="; ")
                h$AllophoneNotes <- stri_replace_all_fixed(tmp, replacement="",
                                                           pattern="\"");
                return(h)
            })
            collapsed <- do.call(rbind, collapsed)
        }
        return(collapsed)
    })
    merged <- do.call(rbind, spt)
	rownames(merged) <- NULL
	merged$Allophones <- denorm(merged$Allophones)
	return(merged)
}

## read "long and sparse" data format (PH, GM)
parseSparse <- function(x, abbr, cols=NULL, addID=FALSE) {
    if (addID) {
        ## create inventory IDs (sequential integers based on doculect)
        x$InventoryID <- NA
        x[!is.na(x$FileNames), "InventoryID"] <- seq_len(sum(!is.na(x$FileNames)))
    }
    ## remove empty rows
    x <- x[!is.na(x$Phoneme),]
    ## fill in sparsity (this is harmless when not needed)
    x$LanguageCode <- na.locf(x$LanguageCode)
    x$InventoryID <- na.locf(x$InventoryID)
    ## denormalize (allophones get denormalized in collapseAllophones function)
    x$Phoneme <- denorm(x$Phoneme)
    if(!is.null(cols)) {
        # using 'split' before 'fillCells' prevents things like SpecificDialect
        # from copying beyond the row extent of each LanguageCode
        y <- split(x, x$InventoryID)
        z <- unsplit(lapply(y, fillCells, cols), x$InventoryID)
    } else {
        z <- x
    }
    z$Source <- abbr
    z <- collapseAllophones(z, "InventoryID")
}

## remove duplicate languages (respecting trump order)
removeDuplicateLangs <- function(x, cols) {
    # cols should be a character vector. Selection is done with the min()
    # function, so it works well for the "Source" column (which is set up as an
    # ordered factor). We also pass "SpecificDialect" (which is a plain
    # character vector) as a tiebreaker, in which case min() uses alphabetical
    # order based on locale. This is not ideal as it may yield different results
    # on different machines, but we don't currently have a way of specifying
    # trump order for dialects of the same language that come from the same
    # data source.
    for (col in cols) {
        if(length(unique(x[[col]])) > 1) {
            x <- x[x[[col]] == min(x[[col]]),]
        }
    }
    x
}

## assign GlyphIDs
assignGlyphID <- function(phones) {
    ids <- stri_trans_general(phones, "Any-Hex/Unicode")
    ids <- stri_replace_all_fixed(ids, replacement="", pattern = "U")
    ids <- stri_replace_first_fixed(ids, replacement="", pattern = "+")
}

## check for duplicate features
checkDuplicateFeatures <- function(df) {
    dups <- df$segment[duplicated(df$segment)]
    if (length(dups)) {
        for (dup in dups) {
            dup.frame <- df[df$segment %in% dup,]
            for (rnum in nrow(dups) - 1) {
                test <- identical(dup.frame[rnum,], dup.frame[rnum + 1,])
                if (!test) stop("There are duplicated entries in the feature ",
                                "table that have differing feature vectors.")
            }
        }
        df <- df[!duplicated(df$segment),]
        warning("There are duplicated entries in the feature table, but they ",
                "all have identical feature vectors so I'm just deleting the ",
                "duplicate rows before merging with the language data.")
    }
    df
}

## ## ## ## ## ##
##  LOAD DATA  ##
## ## ## ## ## ##
## PH has only first cell filled in some columns
ph.raw <- read.delim(ph.path, na.strings="")
ph.data <- parseSparse(ph.raw, "ph", c("LanguageName", "SpecificDialect",
                                       "Phoneme", "FileNames"), addID=TRUE)
rm(ph.raw)

## GM has dense lx.code, name, and dialect columns, but sparse FileNames column
gm.afr.raw <- read.delim(gm.afr.path, na.strings="", quote="",
                         blank.lines.skip=FALSE)
gm.sea.raw <- read.delim(gm.sea.path, na.strings="", quote="",
                         blank.lines.skip=FALSE)
gm.raw <- rbind(gm.afr.raw, gm.sea.raw)
gm.data <- parseSparse(gm.raw, "gm", "FileNames", addID=TRUE)
rm(gm.raw)

## AA has blank lines between languages, but no sparse columns like PH, GM
## AA lists marginal phonemes in angle brackets like this <h>
aa.data <- read.delim(aa.path, na.strings="", blank.lines.skip=FALSE)
## assign inventoryID (must use "startrows" method because no sparse columns)
aa.data$InventoryID <- NA
startrows <- c(1, which(is.na(aa.data$LanguageCode)) + 1)
aa.data[startrows, "InventoryID"] <- seq_len(length(startrows))
aa.data$InventoryID <- na.locf(aa.data$InventoryID)
aa.data <- aa.data[!is.na(aa.data$Phoneme),]  # remove blank rows
## collapse based on "InventoryID" because AA includes different dialects with
## same ISO code, and not all languages have an entry under "SpecificDialect"
aa.data <- collapseAllophones(aa.data, "InventoryID")
## If the "LanguageName" column has parenthetical info, copy language name to
## "SpecificDialect" and remove parenthetical from "LanguageName"
has.parens.bool <- stri_detect_fixed(aa.data$LanguageName, "(")
aa.data$SpecificDialect <- ifelse(has.parens.bool, aa.data$LanguageName, NA)
aa.data$LanguageName <- ifelse(has.parens.bool,
                               sapply(stri_split_fixed(aa.data$LanguageName, " ("),
                                      function(x) x[1]), aa.data$LanguageName)
aa.data$Source <- "aa"

## SPA
spa.ipa <- read.delim(spa.ipa.path, na.strings="", stringsAsFactors=FALSE, quote="")
spa.iso <- read.delim(spa.iso.path, na.strings="", stringsAsFactors=FALSE, quote="")
spa.raw <- read.delim(spa.path, na.strings="", stringsAsFactors=FALSE, quote="")
spa.raw$InventoryID <- spaLangNum
## is it OK to use spaLangNum as InventoryID instead of assigning new sequential ID?
##spa.raw$InventoryID <- NA
##spa.raw[!is.na(spa.raw$spaLangNum), "InventoryID"] <- seq_len(sum(!is.na(spa.raw$spaLangNum)))
spa.raw <- merge(spa.raw, spa.iso, all.x=TRUE, sort=FALSE)
spa.raw <- merge(spa.raw, spa.ipa[,c("spaDescription", "Phoneme")], all.x=TRUE,
                 sort=FALSE)
spa.raw$Allophones <- spa.ipa$Phoneme[match(spa.raw$spaAllophoneDescription,
                                            spa.ipa$spaDescription)]
spa.data <- parseSparse(spa.raw, "spa", "LanguageName")  # "spaPhoneNum", "spaDescription"
spa.data <- spa.data[, c("LanguageCode", "LanguageName", "Phoneme", "Allophones",
                         "Source", "InventoryID")]
rm(spa.raw, spa.ipa, spa.iso)

## UPSID
upsid.ipa <- read.delim(upsid.ipa.path, na.strings="", quote="",
                        stringsAsFactors=FALSE)
upsid.segments <- read.delim(upsid.segments.path, na.strings="", quote="",
                             stringsAsFactors=FALSE)
upsid.language.codes <- read.delim(upsid.language.codes.path, na.strings="",
                                   quote="", stringsAsFactors=FALSE)
upsid.data <- merge(upsid.language.codes, upsid.segments, by="upsidLangNum")
upsid.data <- merge(upsid.data, upsid.ipa[c("upsidCCID", "Phoneme")], by="upsidCCID")
upsid.data <- within(upsid.data, {
    Source <- "upsid"
    Phoneme <- denorm(Phoneme)
})
upsid.data$InventoryID <- upsidLangNum
## is it OK to use upsidLangNum as InventoryID instead of assigning new sequential ID?
##upsid.data$InventoryID <- NA
##upsid.data[!is.na(upsid.data$upsidLangNum), "InventoryID"] <- seq_len(sum(!is.na(upsid.data$upsidLangNum)))
rm(upsid.ipa, upsid.segments, upsid.language.codes)

## RAMASWAMI
ra.raw <- read.delim(ra.path, na.strings="", quote="", as.is=TRUE, header=FALSE)
ra.data <- apply(ra.raw[4:nrow(ra.raw),], 1,
				 function(i) data.frame(LanguageName=i[2], LanguageCode=i[3], InventoryID=i[1],
				 Phoneme=c(t(ra.raw[2, 4:length(i)][as.logical(as.numeric(i[4:length(i)]))])),
				 row.names=NULL))
ra.data <- do.call(rbind, ra.data)
ra.data$Source <- "ra"
rm(ra.raw)

## SAPHON
## TODO: this code is fragile, and is built for saphon20121031.tsv, which was
## hand-corrected from the original CSV version to remove extraneous line breaks
## and quotes, and convert delimiters from comma to tab (several cells had
## internal commas). Future releases of SAPHON may break this code.
saphon.ipa <- read.delim(saphon.ipa.path, as.is=TRUE, header=TRUE)
saphon.raw <- read.delim(saphon.path, na.strings="", quote="", as.is=TRUE,
                         header=FALSE, row.names=NULL)
saphon.starting.row <- 3
saphon.phoneme.cols <- 17:341  # 342 is "tone", 343 is "nasal harmony"
## collect the list of possible phonemes and convert to IPA
saphon.phones <- as.vector(t(saphon.raw[1, saphon.phoneme.cols]))
saphon.phones <- saphon.ipa$IPA[match(saphon.phones, saphon.ipa$SAPHON)]
## fill in empty cells with 0
saphon.raw[is.na(saphon.raw)] <- "0"
## for each language, extract name, ISO code, and phonemes with a "1" in their column
saphon.data <- apply(saphon.raw[saphon.starting.row:nrow(saphon.raw),], 1,
                     function(i) data.frame(LanguageName=i[1], LanguageCode=i[5],
                     Phoneme=saphon.phones[as.logical(as.integer(i[saphon.phoneme.cols]))],
                     row.names=NULL))
saphon.data <- do.call(rbind, saphon.data)
## remove dialect information from ISO codes
saphon.is.dialect <- stri_detect_fixed(saphon.data$LanguageCode, "_")
saphon.data$LanguageCode <- sapply(stri_split_fixed(saphon.data$LanguageCode, "_"),
                                   function(x) x[1])
## handle entries that have two ISO codes
saphon.data$LanguageCode <- sapply(stri_split_fixed(saphon.data$LanguageCode, " "),
                                   function(x) x[1])
## extract dialect information from LanguageName, if it exists
saphon.has.parens <- stri_detect_fixed(saphon.data$LanguageName, "(")
saphon.data$SpecificDialect <- sapply(stri_split_regex(saphon.data$LanguageName, "[()]"),
                                      function(x) ifelse(length(x) > 1, x[2], ""))
saphon.data$LanguageName <- sapply(stri_split_regex(saphon.data$LanguageName, "[()]"),
                                   function(x) x[1])
## handle dialects that don't have parenthetical indications in LanguageName
named.dialect <- saphon.is.dialect & !saphon.has.parens
saphon.data$SpecificDialect[named.dialect] <- saphon.data$LanguageName[named.dialect]
saphon.data$Source <- "saphon"
rm(saphon.raw, saphon.ipa)
## TODO: add inventory ID for SAPHON

## TODO: OCEANIA

## TODO: STEDT

## TODO: JIPA

## ## ## ## ## ## ## ## ##
## COMBINE DATA SOURCES ##
## ## ## ## ## ## ## ## ##
## combine into one data frame
data.sources.list <- list(ph.data, aa.data, spa.data, upsid.data,
                          ra.data, gm.data, saphon.data)
all.data <- do.call(rbind.fill, data.sources.list)
all.data <- all.data[with(all.data, order(LanguageCode, Source)),]
## remove all tiebars
all.data$Phoneme <- stri_replace_all_fixed(all.data$Phoneme, replacement="", pattern="͡")
all.data$Phoneme <- stri_replace_all_fixed(all.data$Phoneme, replacement="", pattern="͜")
## should all be denormalized already, but make sure
all.data$Phoneme <- denorm(all.data$Phoneme)
all.data$Allophones <- denorm(all.data$Allophones)
## make boolean column "Marginal" and remove angle brackets
all.data <- markMarginal(all.data)
## Assign glyph IDs
all.data$GlyphID <- assignGlyphID(all.data$Phoneme)
## uniqueify inventory IDs
all.data$InventoryID <- with(all.data, paste(LanguageCode, Source, InventoryID, sep="-"))

## ## ## ## ## ## ## ## ## ##
## LOAD THE FEATURES TABLE ##
## ## ## ## ## ## ## ## ## ##
feats <- read.delim(features.path, sep='\t', stringsAsFactors=FALSE)
feats$segment <- denorm(feats$segment)
feats <- checkDuplicateFeatures(feats)
feats$GlyphID <- assignGlyphID(feats$segment)
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
all.data <- merge(all.data, feats, by.x="GlyphID", by.y="GlyphID", all.x=TRUE,
                  all.y=FALSE, sort=FALSE)

## handle UPSID disjuncts
upsid.disjunct.indices <- grepl("|", all.data$Phoneme, fixed=TRUE)
upsid.disjuncts <- strsplit(as.character(all.data$Phoneme[upsid.disjunct.indices]),
                            split="|", fixed=TRUE)
upsid.feats <- do.call(rbind, lapply(upsid.disjuncts, function(i) {
    left.index <- which(feats$segment == i[1])
    right.index <- which(feats$segment == i[2])
    matches <- unlist(lapply(seq_along(feats[left.index,]),
                             function(i) feats[left.index, i] == feats[right.index, i]))
    output <- feats[left.index,]
    output[!matches] <- 0
    output$segment <- paste(i, collapse="|")
    return(output)
}))
all.data[upsid.disjunct.indices, feat.columns] <- upsid.feats[feat.columns]


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## filter duplicate languages using trump ordering ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
if(apply.trump) {
    all.data$Source <- factor(all.data$Source, levels=trump.order, ordered=TRUE)
    split.data <- split(all.data, all.data$LanguageCode)
    reduced.data <- lapply(split.data, removeDuplicateLangs,
                           c("Source", "SpecificDialect", "LanguageName"))
    all.data <- do.call(rbind, reduced.data)
    rownames(all.data) <- NULL
    rm(reduced.data)
}


## ## ## ## ## ## ## ## ## ## ##
## WRITE OUT AGGREGATED DATA  ##
## ## ## ## ## ## ## ## ## ## ##
final.data <- all.data[, c(output.fields, feat.columns)]
## Rdata
save(final.data, file=output.rdata)
## tab-delimited
write.table(final.data, file=output.fname, sep="\t", eol="\n",
            row.names=FALSE, quote=FALSE, fileEncoding="UTF-8")
## reset options
options(stringsAsFactors=saf)
