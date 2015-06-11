#! /usr/bin/env Rscript

## This script reads in the raw data files from various tertiary sources, and
## aggregates them into a single R data.frame object called "all.data",
## which is then written out to the root directory of the repository.

library(zoo)      # provides function na.locf (last observ. carry forward)
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

## do you want to clean up intermediate files when finished? (FALSE for debugging)
clear.intermed.files <- TRUE

## SOURCE DATA FILE PATHS
features.path <- file.path(data.dir, "FEATURES", "phoible-segments-features.tsv")
ph.path <- file.path(data.dir, "PH", "phoible_inventories.tsv")
aa.path <- file.path(data.dir, "AA", "AA_inventories.tsv")
spa.path <- file.path(data.dir, "SPA", "SPA_Phones.tsv")
spa.ipa.path <- file.path(data.dir, "SPA", "SPA_IPA_correspondences.tsv")
spa.iso.path <- file.path(data.dir, "SPA", "SPA_LangNamesCodes.tsv")
upsid.segments.path <- file.path(data.dir, "UPSID", "UPSID_Segments.tsv")
upsid.language.codes.path <- file.path(data.dir, "UPSID", "UPSID_LanguageCodes.tsv")
upsid.ipa.path <- file.path(data.dir, "UPSID", "UPSID_IPA_correspondences.tsv")
ra.path <- file.path(data.dir, "RA", "Ramaswami1999.tsv")
gm.afr.path <- file.path(data.dir, "GM", "gm-afr-inventories.tsv")
gm.sea.path <- file.path(data.dir, "GM", "gm-sea-inventories.tsv")
saphon.path <- file.path(data.dir, "SAPHON", "saphon20121031.tsv")
saphon.ipa.path <- file.path(data.dir, "SAPHON", "saphon_ipa_correspondences.tsv")

## ## ## ## ## ##
##  FUNCTIONS  ##
## ## ## ## ## ##

## unicode denormalization
denorm <- function (x) {
    s <- stri_trans_general(x, "Any-NFD")
}

## assign GlyphIDs
assignGlyphID <- function (phones) {
    ids <- stri_trans_general(phones, "Any-Hex/Unicode")
    ids <- stri_replace_all_fixed(ids, replacement="", pattern = "U")
    ids <- stri_replace_first_fixed(ids, replacement="", pattern = "+")
}

## assign integer ID
assignIntegerID <- function (df, col) {
    df$InventoryID <- NA
    df$InventoryID[!is.na(df[[col]])] <- seq_len(sum(!is.na(df[[col]])))
    df$InventoryID <- na.locf(df$InventoryID)
    df
}

## remove brackets
removeBrackets <- function (x, type="square") {
    brak <- switch(type, square=c("[", "]"), angle=c("<", ">"))
    x <- stri_replace_first_fixed(x, pattern=brak[1], replacement="")
    x <- stri_replace_first_fixed(x, pattern=brak[2], replacement="")
}

## mark marginal phonemes in a boolean column and remove <angle brackets>
markMarginal <- function (df) {
    df$Marginal <- stri_detect_fixed(df$Phoneme, "<")
    df$Phoneme <- removeBrackets(df$Phoneme, "angle")
    df
}

## split character vectors and keep first element after split
strsplitKeepFirst <- function (x, split.on) {
    x <- sapply(stri_split_fixed(x, split.on), function (y) y[1])
}

## propogate values through "long and sparse" data sources
fillCells <- function (df, cols) {
    for(col in cols) {
        if(!is.na(match(col, colnames(df)))) {
            thiscol <- df[[col]]
            if(!is.na(thiscol[1])) thiscol <- na.locf(thiscol)
            df[[col]] <- thiscol
        } else {
            stop("Bad column name passed to 'fillCells'.")
        }
    }
    df
}

## helper function to parse long-and-sparse type source data
parseSparse <- function (df, id.col, split.col=NULL, fill.cols=NULL) {
    ## assign integer ID
    df <- assignIntegerID(df, id.col)
    ## fill sparse columns
    if(is.null(split.col)) split.col <- "InventoryID"
    df.split <- split(df, df[[split.col]])
    df <- unsplit(lapply(df.split, fillCells, fill.cols), df[[split.col]])
}

## helper function to clean up processed input data
cleanUp <- function (df, source.id, output.cols=NULL) {
    ## mark marginal phonemes
    if (!"Marginal" %in% colnames(df)) df <- markMarginal(df)
    ## output columns
    if (is.null(output.cols)) {
        output.cols <- c("Phoneme", "Allophones", "Marginal", "InventoryID",
                         "Source", "LanguageCode", "LanguageName", "SpecificDialect")
    }
    ## add missing columns
    for (col in output.cols) if (!col %in% colnames(df)) df[[col]] <- NA
    ## check phoneme and allophone string length for possible invalid data.
    ## If anything looks odd, can examine interactively after the fact:
    ## load("phoible-phoneme-level.RData")  # loads "final.data"
    ## with(final.data, Phoneme[nchar(Phoneme) > 7])
    ## with(final.data, Allophones[nchar(Allophones) > 11])
    ## (7 and 11 are reasonable cutoffs based on table values, edit as needed)
    cat("\nTable of codepoints per phoneme (", source.id, "):", sep="")
    print(table(nchar(df$Phoneme)))
    cat("\nTable of codepoints per allophone (", source.id, ") :", sep="")
    print(table(nchar(df$Allophones)))
    ## collapse allophones
    df <- collapseAllophones(df)
    ## assign source ID
    df$Source <- source.id
    ## remove blank lines
    df <- df[!is.na(df$Phoneme), output.cols]
}

## collapse allophones to a single cell (make data one phoneme per row)
collapseAllophones <- function (df, split.col="InventoryID") {
    ## retain NAs in Allophones column if data source had no allophonic info
    if (all(is.na(df$Allophones))) return (df)
    ## replace NAs in Allophones column with the phoneme representation
    df$Allophones[is.na(df$Allophones)] <- df$Phoneme[is.na(df$Allophones)]
    ## remove square brackets
    df$Allophones <- removeBrackets(df$Allophones)
    ## split data by inventory
    by.inv <- split(df, df[[split.col]])
    ## loop through inventories...
    by.inv <- lapply(by.inv, function (i) {
        ## split inventories by phoneme
        by.pho <- split(i, i$Phoneme)
        ## loop through phonemes...
        by.pho <- lapply(by.pho, function (j) {
            allo <- j$Allophones
            if (!j$Phoneme[1] %in% allo) allo <- c(j$Phoneme[1], allo)
            j[1, "Allophones"] <- paste(allo, collapse=" ")
            j[1,]
        })
        ## unsplit inventory
        inv <- do.call(rbind, by.pho)
    })
    ## unsplit data
    df <- do.call(rbind, by.inv)
    df
}

## remove duplicate languages (respecting trump order)
removeDuplicateLangs <- function(df, cols) {
    # cols should be a character vector. Selection is done with the min()
    # function, so it works well for the "Source" column (which is set up as an
    # ordered factor). We also pass "SpecificDialect" (which is a plain
    # character vector) as a tiebreaker, in which case min() uses alphabetical
    # order based on locale. This is not ideal as it may yield different results
    # on different machines, but we don't currently have a way of specifying
    # trump order for dialects of the same language that come from the same
    # data source.
    for (col in cols) {
        if(length(unique(df[[col]])) > 1) {
            df <- df[df[[col]] == min(df[[col]]),]
        }
    }
    df
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
## PH has only first cell filled in several columns.
## Only column guaranteed unique for each inventory is FileNames
ph.raw <- read.delim(ph.path, na.strings="", blank.lines.skip=TRUE)
#ph.raw$Phoneme <- na.locf(ph.raw$Phoneme)
ph.data <- parseSparse(ph.raw, id.col="FileNames",
                       fill.cols=c("LanguageCode", "LanguageName", "Phoneme",
                                   "SpecificDialect", "FileNames"))
## clean up
ph.data <- cleanUp(ph.data, "ph")
if (clear.intermed.files) rm(ph.raw)

## GM has dense lx.code, name, and dialect columns, but sparse FileNames column.
## Only column guaranteed unique for each inventory is FileNames.
gm.afr.raw <- read.delim(gm.afr.path, na.strings="", quote="",
                         blank.lines.skip=FALSE)
gm.sea.raw <- read.delim(gm.sea.path, na.strings="", quote="",
                         blank.lines.skip=FALSE)
gm.raw <- rbind(gm.afr.raw, gm.sea.raw)
gm.data <- parseSparse(gm.raw, id.col="FileNames", fill.cols="FileNames")
## clean up
gm.data <- cleanUp(gm.data, "gm")
if (clear.intermed.files) rm(gm.raw)

## AA has blank lines between languages, but no sparse columns like PH, GM. Thus
## we need to delimit inventories based on the blank lines.
aa.raw <- read.delim(aa.path, na.strings="", blank.lines.skip=FALSE)
startrows <- c(1, which(is.na(aa.raw$LanguageCode)) + 1)
aa.raw$InventoryID <- NA
aa.raw$InventoryID[startrows] <- seq_len(length(startrows))
aa.raw$InventoryID <- na.locf(aa.raw$InventoryID)
## If the "LanguageName" column has parenthetical info, copy language name to
## "SpecificDialect" and remove parenthetical from "LanguageName"
name.has.parens <- stri_detect_fixed(aa.raw$LanguageName, "(")
aa.raw$SpecificDialect <- ifelse(name.has.parens, aa.raw$LanguageName, NA)
aa.raw$LanguageName <- ifelse(name.has.parens,
                               sapply(stri_split_fixed(aa.raw$LanguageName, " ("),
                                      function (x) x[1]), aa.raw$LanguageName)
## clean up
aa.data <- cleanUp(aa.raw, "aa")
if (clear.intermed.files) rm(aa.raw)

## SPA has sparse columns: spaLangNum, LanguageName, spaPhoneNum, spaDescription
## but no blank lines between inventories.
spa.raw <- read.delim(spa.path, na.strings="", quote="")
spa.iso <- read.delim(spa.iso.path, na.strings="", quote="")
spa.ipa <- read.delim(spa.ipa.path, na.strings="", quote="")
spa.ipa <- spa.ipa[c("spaDescription", "Phoneme")]
## assign integer ID and fill sparsities
spa.data <- parseSparse(spa.raw, id.col="spaLangNum",
                        fill.cols=c("spaLangNum", "LanguageName", "spaDescription"))
## Overwrite integer ID with spaLangNum; it's already a unique (and meaningful) integer
spa.data$InventoryID <- spa.data$spaLangNum
## Merge in ISO codes and IPA representation of phonemes and allophones
spa.data <- merge(spa.data, spa.iso, all.x=TRUE, sort=FALSE)
spa.data <- merge(spa.data, spa.ipa, all.x=TRUE, sort=FALSE)
spa.data$spaAllophoneDescription <- removeBrackets(spa.data$spaAllophoneDescription)
spa.data$Allophones <- spa.ipa$Phoneme[match(spa.data$spaAllophoneDescription,
                                             spa.ipa$spaDescription)]
## SPA does not include marginal phonemes; we denote this with NA
spa.data$Marginal <- NA
## clean up
spa.data <- cleanUp(spa.data, "spa")
if (clear.intermed.files) rm(spa.raw, spa.ipa, spa.iso)

## UPSID has database-like tables, so we basically just merge things
upsid.language.codes <- read.delim(upsid.language.codes.path, na.strings="", quote="")
upsid.segments <- read.delim(upsid.segments.path, na.strings="", quote="")
upsid.ipa <- read.delim(upsid.ipa.path, na.strings="", quote="")
upsid.ipa <- upsid.ipa[c("upsidCCID", "Phoneme")]
upsid.data <- merge(upsid.language.codes, upsid.segments, by="upsidLangNum")
upsid.data <- merge(upsid.data, upsid.ipa, by="upsidCCID")
## upsidLangNum is already a unique (and meaningful) integer
upsid.data$InventoryID <- upsid.data$upsidLangNum
## add column for marginal phonemes
upsid.data$Marginal <- as.logical(upsid.data$anomalous)
## clean up
upsid.data <- cleanUp(upsid.data, "upsid")
if (clear.intermed.files) rm(upsid.ipa, upsid.segments, upsid.language.codes)

## RAMASWAMI is a wide-format data source: 1 row per language, phonemes as
## column headers, with boolean presence/absence indicators in the cells.
## There is an integer ID in the first column of the raw data.
ra.raw <- read.delim(ra.path, na.strings="", quote="", as.is=TRUE, header=FALSE)
ra.data <- apply(ra.raw[4:nrow(ra.raw),], 1, function (i)
    data.frame(InventoryID=i[1], LanguageName=i[2], LanguageCode=i[3],
               Phoneme=c(t(ra.raw[2, 4:length(i)][as.logical(as.numeric(i[4:length(i)]))])),
               row.names=NULL))
ra.data <- do.call(rbind, ra.data)
## RA does not indicate marginal phonemes; we denote this with NA
ra.data$Marginal <- NA
## clean up
ra.data <- cleanUp(ra.data, "ra")
if (clear.intermed.files) rm(ra.raw)

## SAPHON is a wide-format data source: 1 row per language, phonemes as
## column headers, with boolean presence indicators in the cells (absence = "").
## NOTE: this code is fragile, and is built for saphon20121031.tsv, which was
## hand-corrected from the original CSV version to remove extraneous line breaks
## and quotes, and convert delimiters from comma to tab (several cells had
## internal commas). Future releases of SAPHON may break this code.
saphon.ipa <- read.delim(saphon.ipa.path, as.is=TRUE, header=TRUE)
saphon.raw <- read.delim(saphon.path, na.strings="", quote="", as.is=TRUE,
                         header=FALSE, row.names=NULL)
saphon.starting.row <- 3
saphon.phoneme.cols <- 17:341  # 342 is boolean "tone", 343 is boolean "nasal harmony"
## collect the list of possible phonemes and convert to IPA
saphon.phones <- as.vector(t(saphon.raw[1, saphon.phoneme.cols]))
saphon.phones <- saphon.ipa$IPA[match(saphon.phones, saphon.ipa$SAPHON)]
## fill in empty cells with 0
saphon.raw[is.na(saphon.raw)] <- "0"
## for each language, extract name, ISO code, and phonemes with a "1" in their
## column. Also add integer inventory ID
saphon.data <- lapply(saphon.starting.row:nrow(saphon.raw), function (i)
    data.frame(LanguageName=saphon.raw[i, 1], LanguageCode=saphon.raw[i, 5],
               Phoneme=saphon.phones[as.logical(as.integer(saphon.raw[i, saphon.phoneme.cols]))],
               InventoryID=i, row.names=NULL))
saphon.data <- do.call(rbind, saphon.data)
## discard dialect information from ISO codes ("lng_dia" -> "lng")
iso.has.dialect <- stri_detect_fixed(saphon.data$LanguageCode, "_")
saphon.data$LanguageCode <- strsplitKeepFirst(saphon.data$LanguageCode, "_")
## handle entries that have two ISO codes ("lga lgb" -> "lga")
saphon.data$LanguageCode <- strsplitKeepFirst(saphon.data$LanguageCode, " ")
## extract parenthetical dialect from LanguageName, if present
name.has.parens <- stri_detect_fixed(saphon.data$LanguageName, "(")
saphon.data$SpecificDialect <- sapply(stri_split_regex(saphon.data$LanguageName, "[()]"),
                                      function (x) ifelse(length(x) > 1, x[2], ""))
saphon.data$LanguageName <- strsplitKeepFirst(saphon.data$LanguageName, " (")
## handle dialects that don't have parenthetical indications in LanguageName
named.dialect <- iso.has.dialect & !name.has.parens
## make sure we're not overwriting non-empty cells in SpecificDialect column
overlap <- intersect(which(saphon.data$SpecificDialect != ""), which(named.dialect))
if (length(overlap)) warning(length(overlap), "non-empty entries in SAPHON",
                             "SpecificDialect column have been overwritten.")
saphon.data$SpecificDialect[named.dialect] <- saphon.data$LanguageName[named.dialect]
## SAPHON does not indicate marginal phonemes; we denote this with NA
saphon.data$Marginal <- NA
## clean up
saphon.data <- cleanUp(saphon.data, "saphon")
if (clear.intermed.files) rm(saphon.raw, saphon.ipa)


## ## ## ## ## ## ## ## ##
## COMBINE DATA SOURCES ##
## ## ## ## ## ## ## ## ##
## combine into one data frame
data.sources.list <- list(ph.data, aa.data, spa.data, upsid.data,
                          ra.data, gm.data, saphon.data)
all.data <- do.call(rbind, data.sources.list)
all.data <- all.data[with(all.data, order(LanguageCode, Source, InventoryID)),]
## uniqueify inventory IDs across data sources
all.data$InventoryID <- with(all.data, paste(LanguageCode, Source, InventoryID, sep="-"))
## make sure all tiebars are removed (should already be cleaned from source)
all.data$Phoneme <- stri_replace_all_fixed(all.data$Phoneme, replacement="", pattern="͡")
all.data$Phoneme <- stri_replace_all_fixed(all.data$Phoneme, replacement="", pattern="͜")
## should all be denormalized already, but make sure
all.data$Phoneme <- denorm(all.data$Phoneme)
all.data$Allophones <- denorm(all.data$Allophones)
## Assign glyph IDs
all.data$GlyphID <- assignGlyphID(all.data$Phoneme)


## ## ## ## ## ## ## ## ## ##
## LOAD THE FEATURES TABLE ##
## ## ## ## ## ## ## ## ## ##
feats <- read.delim(features.path, sep='\t')
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
upsid.disjunct.indices <- stri_detect_fixed(all.data$Phoneme, "|")
upsid.disjuncts <- stri_split_fixed(all.data$Phoneme[upsid.disjunct.indices],
                                    pattern="|")
upsid.feats <- do.call(rbind, lapply(upsid.disjuncts, function(i) {
    left.index <- which(feats$segment == i[1])
    right.index <- which(feats$segment == i[2])
    matches <- unlist(lapply(seq_along(feats[left.index,]),
                             function(i) feats[left.index, i] == feats[right.index, i]))
    output <- feats[left.index,]
    output[!matches] <- 0
    output$segment <- paste(i, collapse="|")
    output
}))
all.data[upsid.disjunct.indices, feat.columns] <- upsid.feats[feat.columns]
## clean up
if (clear.intermed.files) rm(upsid.feats, upsid.disjunct.indices, upsid.disjuncts,
                             ph.data, aa.data, spa.data, upsid.data, ra.data,
                             gm.data, saphon.data)


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
    if (clear.intermed.files) rm(split.data, reduced.data)
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
## clean up
if (clear.intermed.files) rm(features.path, ph.path, aa.path, spa.path,
                             spa.ipa.path, spa.iso.path, upsid.segments.path,
                             upsid.language.codes.path, upsid.ipa.path, ra.path,
                             gm.afr.path, gm.sea.path, saphon.path, saphon.ipa.path)
## reset options
options(stringsAsFactors=saf)
