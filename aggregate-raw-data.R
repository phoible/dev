#! /usr/bin/env Rscript

## This script reads in the raw data files from various tertiary sources, and
## aggregates them into a single R data.frame object called "all.data",
## which is then written out to the data directory.

library(stringi)  # for proper string handling & (de)normalization

unfamiliar.glyphs <- data.frame()

## ## ## ##
## SETUP ##
## ## ## ##

## set global options (to be restored at end)
saf <- getOption("stringsAsFactors")
options(stringsAsFactors=FALSE)
## file paths
root.dir <- file.path(".")
data.dir <- file.path(root.dir, "raw-data")
output.dir <- file.path(root.dir, "data")
results.dir <- file.path(root.dir, "results")
mapping.dir <- file.path(root.dir, "mappings")
if (!dir.exists(results.dir))  dir.create(results.dir, mode="0755")
## output filenames
output.fname <- file.path(output.dir, "phoible-by-phoneme.tsv")
output.rdata <- file.path(output.dir, "phoible-by-phoneme.RData")
inventory.id.log <- file.path(results.dir, "failed-invID-lookups.txt")
phone.validity.log <- file.path(results.dir, "unfamiliar-phones.txt")

## WHICH DATA COLUMNS TO KEEP (FEATURE COLUMNS GET ADDED LATER)
output.fields <- c("LanguageCode", "LanguageName", "SpecificDialect", "Phoneme",
                   "Allophones", "Source", "Trump", "GlyphID", "InventoryID")

## TRUMP ORDERING (for choosing which entry to keep when there are multiple
## entries for a language). Preferred data sources come earlier in the list.
trump.order <- c("er", "ph", "uz", "gm", "spa", "aa", "ea", "saphon", "upsid", "ra")
## Duplicate inventories will be marked as FALSE in the Trump column. The
## variable "trump.group" is used to determine which inventories count as
## potential duplicates for this purpose. "LanguageCode" (the default) means
## that selecting all TRUE values in the Trump column will return exactly one
## inventory for each unique LanguageCode.
trump.group <- "LanguageCode"
## The variable "trump.tiebreaker" is used to pick which inventory gets kept
## within each "trump.group".  Selection is done with the min()
## function, so it works well for the "Source" column (which is set up as an
## ordered factor). We also pass "InventoryID" by default (which is a numeric
## vector).
trump.tiebreaker <- c("Source", "InventoryID")
if (any(!trump.tiebreaker %in% output.fields)) warning("column \"", col,
                                                       "\" in trump.tiebreaker",
                                                       " not found.")

## clean up intermediate files when finished? (FALSE for debugging)
clear.intermed.files <- FALSE
## should zero-valued features be converted to NAs? (use at own risk)
convert.unvalued.to.NA <- FALSE

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
uz.path <- file.path(data.dir, "UZ", "UZ_inventories.tsv")
ea.path <- file.path(data.dir, "EA", "EA_inventories.tsv")
ea.ipa.path <- file.path(data.dir, "EA", "EA_IPA_correspondences.tsv")
er.path <- file.path(data.dir, "ER", "ER_inventories.tsv")


## TODO: uncomment when ready to merge in Glottolog codes.
# mapping.path <- file.path(mapping.dir, "InventoryID-ISO-gcode-Bibkey-Source.tsv")

## ## ## ## ## ##
##  FUNCTIONS  ##
## ## ## ## ## ##

## FUNCTION: denormalize strings
denorm <- function(x) stri_trans_general(x, "Any-NFD")

## FUNCTION: re-normalize just the c-cedilla after denormalizing
fix_c_cedilla <- function(strings) {
    c_cedilla <- "\u00E7"
    c_cedilla_denormed <- "\u0063\u0327"
    stri_replace_all_fixed(strings, pattern=c_cedilla_denormed,
                           replacement=c_cedilla)
}

## FUNCTION: convert strings to plus-separated codepoints
codepoints <- function(strings) {
    cps <- stri_trans_general(strings, "Any-Hex/Unicode")
    cps <- stri_replace_all_fixed(cps, replacement="", pattern = "U")
    cps <- stri_replace_first_fixed(cps, replacement="", pattern = "+")
    cps
}

## FUNCTION: assign GlyphIDs
assignGlyphID <- function (phones) {
    ids <- stri_trans_general(phones, "Any-Hex/Unicode")
    ids <- stri_replace_all_fixed(ids, replacement="", pattern = "U")
    ids <- stri_replace_first_fixed(ids, replacement="", pattern = "+")
}

## FUNCTION: remove brackets
removeBrackets <- function (x, type="square") {
    brak <- switch(type, square=c("[", "]"), angle=c("<", ">"))
    x <- stri_replace_first_fixed(x, pattern=brak[1], replacement="")
    x <- stri_replace_first_fixed(x, pattern=brak[2], replacement="")
}

## FUNCTION: add/remove asterisks from archephonemes
addStars <- function(strings) {
    stri_replace_all_fixed(strings, pattern=c("R", "N"),
                           replacement=c("*R", "*N"), vectorize_all=FALSE)
}
removeStars <- function(strings) {
    stri_replace_all_fixed(strings, pattern=c("*R", "*N"),
                           replacement=c("R", "N"), vectorize_all=FALSE)
}

## FUNCTION: mark marginal phonemes in a boolean column; remove <angle brackets>
markMarginal <- function (df) {
    df$Marginal <- stri_detect_fixed(df$Phoneme, "<")
    df$Phoneme <- removeBrackets(df$Phoneme, "angle")
    df
}

## FUNCTION: split character vectors and keep first element after split
strsplitKeepFirst <- function (x, split.on) {
    x <- sapply(stri_split_fixed(x, split.on), function (y) y[1])
}

## FUNCTION: propogate values through "long and sparse" data sources
fillCells <- function (df, cols) {
    for(col in cols) {
        if(!is.na(match(col, colnames(df)))) {
            thiscol <- df[[col]]
            if(!is.na(thiscol[1])) thiscol <- zoo::na.locf(thiscol)
            df[[col]] <- thiscol
        } else {
            stop("Bad column name passed to 'fillCells'.")
        }
    }
    df
}

## FUNCTION: helper to parse long-and-sparse type source data
parseSparse <- function (df, id.col, split.col="InventoryID", fill.cols=NULL) {
    df$InventoryID <- zoo::na.locf(df$InventoryID)
    df.split <- split(df, df[[split.col]])
    df <- unsplit(lapply(df.split, fillCells, fill.cols), df[[split.col]])
}

## FUNCTION: helper function to clean up processed input data
cleanUp <- function (df, source.id, output.cols=NULL) {
    ## mark marginal phonemes
    if (!"Marginal" %in% colnames(df)) df <- markMarginal(df)
    ## output columns
    if (is.null(output.cols)) {
        output.cols <- c("Phoneme", "Allophones", "Marginal", "InventoryID",
                         "Source", "LanguageCode", "LanguageName",
                         "SpecificDialect", "FileNames")  # Bibkey
    }
    ## add missing columns
    for (col in output.cols) if (!col %in% colnames(df)) df[[col]] <- NA
    ## apply canonical ordering of glyphs. Keep track of distribution of segment
    ## length (# of codepoints) before & after, as a sanity check: numbers
    ## should change very little
    phonemes_original <- table(nchar(df$Phoneme))
    allophones_original <- table(nchar(removeBrackets(df$Allophones)))
    df$Phoneme <- orderIPA(df$Phoneme)
    df$Allophones <- orderIPA(df$Allophones)
    phonemes_canonical <- table(nchar(df$Phoneme))
    allophones_canonical <- table(nchar(df$Allophones))
    tabs <- list("phonemes (original)"=phonemes_original,
                 "phonemes (canonical)"=phonemes_canonical,
                 "allophones (original)"=allophones_original,
                 "allophones (canonical)"=allophones_canonical)
    ## check phoneme and allophone string length for possible invalid data.
    ## If anything looks odd, can examine interactively after the fact:
    ## load("phoible-by-phoneme.RData")  # loads "final.data"
    ## with(final.data, Phoneme[nchar(Phoneme) > 7])
    ## with(final.data, Allophones[nchar(Allophones) > 11])
    ## (7 and 11 are reasonable cutoffs based on table values, edit as needed)
    tab <- do.call(rbind, lapply(lapply(tabs, unlist), "[",
                                 unique(unlist(c(sapply(tabs, names))))))
    tab[is.na(tab)] <- 0
    colnames(tab) <- seq(ncol(tab))
    total <- apply(tab, 1, sum)
    tab <- cbind(tab, "total"=total)
    cat("\nTable of codepoints per phone (", source.id, "):\n", sep="")
    print(tab)
    ## collapse allophones
    df <- collapseAllophones(df)
    ## assign source ID
    df$Source <- source.id
    ## remove blank lines
    df <- df[!is.na(df$Phoneme), output.cols]
}

## FUNCTION: collapse allophones to a single cell (make data one phoneme per row)
collapseAllophones <- function (df, split.col="InventoryID") {
    ## retain NAs in Allophones column if data source had no allophonic info
    if (all(is.na(df$Allophones))) return (df)
    ## replace NAs in Allophones column with the phoneme representation
    df$Allophones[is.na(df$Allophones)] <- df$Phoneme[is.na(df$Allophones)]
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

## FUNCTION: check for duplicate features
checkDuplicateFeatures <- function(df) {
    dups <- df$segment[duplicated(df$segment)]
    if (length(dups)) {
        for (dup in dups) {
            dup.frame <- df[df$segment %in% dup,]
            for (rnum in nrow(dups) - 1) {
                test <- identical(dup.frame[rnum,], dup.frame[rnum + 1,])
                if (!test) {
                    stop("There are duplicated entries in the feature ",
                         "table that have differing feature vectors (namely: ",
                         dup.frame$segment, " ).")
                }
            }
        }
        warning("There are duplicated entries in the feature table, but they ",
                "all have identical feature vectors so I'm just deleting the ",
                "duplicate rows before merging with the language data. The ",
                "duplicated segment(s) are: ",
                paste(df[duplicated(df$segment), "segment"], collapse=" "))
        df <- df[!duplicated(df$segment),]
    }
    df
}

## FUNCTION: compute trump status
computeTrump <- function(df) {
    df$Trump <- TRUE
    for (col in trump.tiebreaker) {
        if (all(is.na(df[df$Trump, col]))) next
        df$Trump <- df$Trump & ((df[[col]] == min(df[[col]])) == 1)
    }
    df
}

## FUNCTION: define glyph types; adds the variables to the specified environment
defineGlyphTypes <- function(..., envir=.GlobalEnv) {
    inputs <- c(...)
    if (!length(inputs)) {
        inputs <- c("tones", "modifiers", "diacritics", "stops", "nasals",
                    "fricatives", "flaps", "affricates", "implosives",
                    "approximants", "clicks", "vowels", "archephonemes",
                    "base.glyphs", "feature.contour.glyphs")
    }
    ## DEFINITION OF GLYPH TYPES
    ## Tones are not internally reordered so the order here is arbitrary.
    tones <- c("˩", "˨", "˧", "˦", "˥", "↓", "↘")
    ## Order of elements in "diacritics" and "modifiers" sets canonical order!!
    modifiers <- c(
        "˞",  # rhotic wing
        "ː",  # long
        "ˑ",  # half-long
        "ᴱ",  # epilaryngeal source
        "ⁿ",  # nasal release
        "ˡ",  # lateral release
        "ʱ",  # breathy aspirated
        "ʰ",  # aspirated
        "˭",  # unaspirated
        "ˀ",  # glottalized
        "ˤ",  # pharyngealized
        "ˠ",  # velarized
        "ʲ",  # palatalized
        "ʷ",  # labialized
        "ᶣ",  # labial-palatalized  # TODO: should use labial+palatal?
        "ᵊ",  # schwa-like release # TODO: check what this really is
        "ʼ"  # ejective
    )
    diacritics <- c(
        "̴",  # velarized/pharyngealized (combining tilde overlay)
        "̃",  # nasalized (combining tilde)
        "͊",  # denasalized (combining not tilde above)
        "͋",  # nasal emission (combining homothetic)
        "̮",  # derhoticized (combining breve below)
        "̤",  # breathy (combining diaresis below)
        "̰",  # creaky (combining tilde below)
        "̬",  # stiff (combining caron below)
        "͓",  # frictionalized (combining x below)
        "̼",  # linguolabial (combining seagull below)
        "̪",  # dental (combining bridge below)
        "̺",  # apical (combining inverted bridge below)
        "̻",  # laminal (combining square below)
        "͇",  # non-sibilant (combining equals sign below)
        "͈",  # fortis (combining double vertical line below)
        "͉",  # lenis (combining left angle below)
        "̙",  # retracted tongue root (combining right tack below)
        "̘",  # advanced tongue root (combining left tack below)
        "̞",  # lowered (combining down tack below)
        "̝",  # raised (combining up tack below)
        "̟",  # advanced (combining plus sign below)
        "̠",  # retracted (combining minus sign below)
        "̈",  # centralized (combining diaresis)
        "̽",  # mid-centralized (combining x above)
        "̹",  # more round (combining right half ring)
        "̜",  # less round (combining left half ring)
        "̩",  # syllabic (combining vertical line below)
        "̯",  # non-syllabic (combining inverted breve below)
        "̆",  # short (combining breve)
        "̥",  # devoiced (combining ring below)
        "̊",  # devoiced (combining ring above)
        "̚"   # unreleased (combining left angle above)
    )
    ## "feature.contour.glyphs" are the modifiers & diacritics that create a contour with their
    ## base glyph's feature values, rather than overwriting them. This grouping is not used for
    ## canonical ordering, but for feature vector construction.
    feature.contour.glyphs <- c("ⁿ", "ˡ")  # nasal release, lateral release
    ## The base glyphs are broken up into subtypes for convenience only;
    ## their order does not matter.
    vowels <- c("i", "y", "ɨ", "ʉ", "ɯ", "u", "ɪ", "ʏ", "ʊ", "e", "ø", "ɘ", "ɵ",
                "ɤ", "o", "ə", "ɛ", "œ", "ɜ", "ɞ", "ʌ", "ɔ", "æ", "ɐ", "a", "ɶ",
                "ɑ", "ɒ", "ɚ", "ɝ")
    stops <- c("p", "b", "t", "d", "ȶ", "ȡ", "ʈ", "ɖ", "c", "ɟ", "k", "ɡ", "q", 
               "ɢ", "ʡ", "ʔ")
    nasals <- c("m", "ɱ", "n", "ȵ", "ɳ", "ɲ", "ŋ", "ɴ")
    fricatives <- c("ɸ", "β", "f", "v", "θ", "ð", "s", "z", "ɕ", "ʑ", "ʃ", "ʒ",
                    "ʂ", "ʐ", "ç", "ʝ", "x", "ɣ", "χ", "ʁ", "ħ", "ʕ", "ʜ", "ʢ",
                    "h", "ɦ", "ɬ", "ɮ", "ɧ", "ʍ")
    flaps <- c("ʙ", "ⱱ", "r", "ɾ", "ᴅ", "ɽ", "ʀ", "ɺ")
    clicks <- c("ʘ", "ǀ", "ǁ", "ǃ", "ǂ", "‼")
    affricates <- c("ʦ", "ʣ", "ʧ", "ʤ")
    implosives <- c("ƥ", "ɓ", "ƭ", "ɗ", "ᶑ", "ƈ", "ʄ", "ƙ", "ɠ", "ʠ", "ʛ")
    approximants <- c("ʋ", "ɹ", "ɻ", "j", "ɥ", "ɰ", "l", "ɭ", "ʎ", "ʟ", "ɫ", "w")
    archephonemes <- c("R", "N")  # R = tap/trill; N = placeless nasal
    if ("stops" %in% inputs)         assign("stops", stops, envir)
    if ("flaps" %in% inputs)         assign("flaps", flaps, envir)
    if ("tones" %in% inputs)         assign("tones", tones, envir)
    if ("nasals" %in% inputs)        assign("nasals", nasals, envir)
    if ("vowels" %in% inputs)        assign("vowels", vowels, envir)
    if ("clicks" %in% inputs)        assign("clicks", clicks, envir)
    if ("modifiers" %in% inputs)     assign("modifiers", modifiers, envir)
    if ("diacritics" %in% inputs)    assign("diacritics", diacritics, envir)
    if ("fricatives" %in% inputs)    assign("fricatives", fricatives, envir)
    if ("affricates" %in% inputs)    assign("affricates", affricates, envir)
    if ("implosives" %in% inputs)    assign("implosives", implosives, envir)
    if ("approximants" %in% inputs)  assign("approximants", approximants, envir)
    if ("archephonemes" %in% inputs) assign("archephonemes", archephonemes, envir)
    if ("feature.contour.glyphs" %in% inputs) assign("feature.contour.glyphs",
                                                     feature.contour.glyphs, envir)
    if ("base.glyphs" %in% inputs) {
        assign("base.glyphs", c(vowels, stops, implosives, flaps, nasals, clicks, fricatives,
                                affricates, approximants, archephonemes), envir)
    }
}

## FUNCTION: make typestring from glyphs
makeTypestring <- function(strings, ...) {
    defineGlyphTypes(envir=environment())
    ## replace *R/*N with R/N
    strings <- removeStars(strings)
    # typestrings <- unlist(mclapply(strings, function(string) {
    typestrings <- sapply(strings, function(string) {
        if (is.na(string)) return(NA)
        chars <- strsplit(string, "")[[1]]
        codpts <- codepoints(chars)
        codpts[codpts %in% "007C"] <- "|"  # restore upsid disjuncts
        codpts[codpts %in% codepoints(base.glyphs)] <- "B"
        codpts[codpts %in% codepoints(modifiers)] <- "M"
        codpts[codpts %in% codepoints(diacritics)] <- "D"
        codpts[codpts %in% codepoints(tones)] <- "T"
        missing <- !codpts %in% c("B", "M", "D", "T", "|")
        if (any(missing)) {
            warning(paste("Unfamiliar glyph components.", "Phone:", string,
                          "Codepoint:", codpts[missing]),
                    call.=FALSE, immediate.=TRUE)
            assign("unfamiliar.glyphs", pos=.GlobalEnv,
                   rbind(unfamiliar.glyphs,
                         data.frame(phoneme=string, codepoint=codpts[missing])))
        }
        # restore original glyphs
        codpts[missing] <- chars[missing]
        paste0(codpts, collapse="")
    }, ...)
    typestrings
}

## FUNCTION: impose canonical ordering of codepoints
## (gets applied in the "CleanUp" function)
orderIPA <- function(strings, keep.stars=FALSE) {
    ## make sure we have what we need in the environment
    defineGlyphTypes(envir=environment())
    ## start by denormalizing
    strings <- denorm(strings)
    ## re-normalize c-cedilla
    strings <- fix_c_cedilla(strings)
    ## replace *R/*N with R/N
    strings <- removeStars(strings)
    ## remove whitespace, tiebars, & square brackets
    strings <- removeBrackets(strings)
    strings <- stri_trim(strings)
    strings <- stri_replace_all_fixed(strings, replacement="", pattern="͡")
    strings <- stri_replace_all_fixed(strings, replacement="", pattern="͜")
    ## construct parallel string showing character classes
    typestrings <- makeTypestring(strings, USE.NAMES=FALSE)
    canonical.strings <- sapply(seq_along(strings), function(i) {
        typestring <- typestrings[i]
        if (is.na(typestring)) return(NA)
        typstr <- strsplit(typestring, "")[[1]]
        string <- strsplit(strings[i], "")[[1]]
        lenstr <- length(string)
        ## move tones to end
        if (stri_detect_fixed(typestring, "T")) {
            ix <- stri_locate_first_fixed(typestring, "T")[1]
            rightedge <- typstr[ix:lenstr]
            while (ix < lenstr && !all(rightedge %in% "T")) {
                if (ix == 1) neworder <- c(2:lenstr, 1)
                else if (ix < lenstr) neworder <- c(1:(ix-1), (ix+1):lenstr, ix)
                string <- string[neworder]
                typstr <- typstr[neworder]
                typestring <- paste(typstr, collapse="")
                ix <- stri_locate_first_fixed(typestring, "T")[1]
                rightedge <- typstr[ix:lenstr]
            }
        }
        ## If a diacritic comes right after a modifier letter, swap their order
        while (stri_detect_fixed(typestring, "MD")) {
            ix <- stri_locate_first_fixed(typestring, "MD")[1]
            if (ix == 1) neworder <- c(2, 1, 3:lenstr)
            else if (ix == lenstr-1) neworder <- c(1:(ix-1), ix+1, ix)
            else neworder <- c(1:(ix-1), ix+1, ix, (ix+2):lenstr)
            string <- string[neworder]
            typstr <- typstr[neworder]
            typestring <- paste(typstr, collapse="")
        }
        ## Put sequences of modifier letters in canonical order
        if (stri_detect_fixed(typestring, "MM")) {
            ixs <- stri_locate_all_regex(typestring, "M+")[[1]]
            for (row in seq_len(dim(ixs)[1])) {
                span <- ixs[row,1]:ixs[row,2]
                mods <- string[span]
                string[span] <- modifiers[modifiers %in% mods]
            }   }
        ## Put sequences of diacritics in canonical order
        if (stri_detect_fixed(typestring, "DD")) {
            ixs <- stri_locate_all_regex(typestring, "D+")[[1]]
            for (row in seq_len(dim(ixs)[1])) {
                span <- ixs[row,1]:ixs[row,2]
                dcrs <- string[span]
                string[span] <- diacritics[diacritics %in% dcrs]
            }   }
        paste(string, collapse="")
    }, USE.NAMES=FALSE)
    ## restore asterisks
    if (keep.stars) canonical.strings <- addStars(canonical.strings)
    canonical.strings
}


## ## ## ## ## ##
##  LOAD DATA  ##
## ## ## ## ## ##

## TODO: load Glottocode lookup table
# mapping <- read.delim(mapping.path)

## Australian inventories from Erich Round. All columns are dense:
## InventoryID, LanguageCode, LanguageName, Phoneme
## Dialect information is (sometimes) included parenthetically in the
## LanguageName field (like in AA/SAPHON)
er.raw <- read.delim(er.path, na.strings="", blank.lines.skip=FALSE)
head(er.raw)
# ea.ipa <- read.delim(ea.ipa.path, na.strings="", quote="")
er.raw$InventoryID <- zoo::na.locf(er.raw$InventoryID)
## If the "LanguageName" column has parenthetical info, copy language name to
## "SpecificDialect" and remove parenthetical from "LanguageName"
# name.has.parens <- stri_detect_fixed(ea.raw$LanguageName, "(")
# ea.raw$SpecificDialect <- ifelse(name.has.parens, ea.raw$LanguageName, NA)
# ea.raw$LanguageName <- ifelse(name.has.parens, sapply(stri_split_fixed(ea.raw$LanguageName, " ("), function (x) x[1]), ea.raw$LanguageName)

# ea.raw <- merge(ea.raw, ea.ipa)
## clean up
er.data <- cleanUp(er.raw, "er")
if (clear.intermed.files) rm(er.raw)

## Eurasian Phonologies inventory data. All columns are dense:
## InventoryID, LanguageCode, LanguageName, Phoneme
## Dialect information is (sometimes) included parenthetically in the
## LanguageName field (like in AA/SAPHON)
ea.raw <- read.delim(ea.path, na.strings="", blank.lines.skip=FALSE)
ea.ipa <- read.delim(ea.ipa.path, na.strings="", quote="")
ea.raw$InventoryID <- zoo::na.locf(ea.raw$InventoryID)
## If the "LanguageName" column has parenthetical info, copy language name to
## "SpecificDialect" and remove parenthetical from "LanguageName"
name.has.parens <- stri_detect_fixed(ea.raw$LanguageName, "(")
ea.raw$SpecificDialect <- ifelse(name.has.parens, ea.raw$LanguageName, NA)
ea.raw$LanguageName <- ifelse(name.has.parens,
                              sapply(stri_split_fixed(ea.raw$LanguageName, " ("),
                                     function (x) x[1]), ea.raw$LanguageName)
ea.raw <- merge(ea.raw, ea.ipa)
## clean up
ea.data <- cleanUp(ea.raw, "ea")
if (clear.intermed.files) rm(ea.raw)

## PH has only first cell filled in several columns.
## Only column guaranteed unique for each inventory is FileNames
ph.raw <- read.delim(ph.path, na.strings="", blank.lines.skip=TRUE)
ph.data <- parseSparse(ph.raw, id.col="InventoryID",
                       fill.cols=c("InventoryID", "LanguageCode", "LanguageName",
                                   "Phoneme", "SpecificDialect", "FileNames"))
## clean up
ph.data <- cleanUp(ph.data, "ph")
if (clear.intermed.files) rm(ph.raw)

## UZ has only first cell filled in several columns.
## Only column guaranteed unique for each inventory is FileNames
uz.raw <- read.delim(uz.path, na.strings="", blank.lines.skip=TRUE)
uz.data <- parseSparse(uz.raw, id.col="InventoryID",
                       fill.cols=c("LanguageCode", "LanguageName", "Phoneme",
                                   "SpecificDialect", "FileNames"))
## clean up
uz.data <- cleanUp(uz.data, "uz")
if (clear.intermed.files) rm(uz.raw)

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

## AA has blank lines between languages; InventoryID is sparse and unique; all
## other columns are dense.
aa.raw <- read.delim(aa.path, na.strings="", blank.lines.skip=TRUE)
aa.raw$InventoryID <- zoo::na.locf(aa.raw$InventoryID)
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
## assign temporary integer ID and fill sparsities
spa.data <- parseSparse(spa.raw, id.col="spaLangNum",
                        fill.cols=c("spaLangNum", "LanguageName", "spaDescription"))
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
    data.frame(InventoryID=i[1], LanguageName=i[3], LanguageCode=i[4],
               Phoneme=c(t(ra.raw[2, 5:length(i)][as.logical(as.numeric(i[5:length(i)]))])),
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
saphon.ipa$IPA <- orderIPA(saphon.ipa$IPA)
saphon.raw <- read.delim(saphon.path, na.strings="", quote="", as.is=TRUE,
                         header=FALSE, row.names=NULL)
saphon.starting.row <- 3
saphon.phoneme.cols <- 18:342  # column 343: +/- tone, 344: +/- nasal harmony
## collect the list of possible phonemes and convert to IPA
saphon.phones <- as.vector(t(saphon.raw[1, saphon.phoneme.cols]))
saphon.phones <- saphon.ipa$IPA[match(saphon.phones, saphon.ipa$SAPHON)]
saphon.phones <- orderIPA(saphon.phones)
## fill in empty cells with 0
saphon.raw[is.na(saphon.raw)] <- "0"
## for each language, extract inventoryID, name, ISO code, and phonemes
## with a "1" in their column.
saphon.data <- lapply(saphon.starting.row:nrow(saphon.raw), function (i)
    data.frame(LanguageName=saphon.raw[i, 2], LanguageCode=saphon.raw[i, 6],
               Phoneme=saphon.phones[as.logical(as.integer(saphon.raw[i, saphon.phoneme.cols]))],
               InventoryID=saphon.raw[i, 1], row.names=NULL))
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
                          ra.data, gm.data, saphon.data, uz.data, ea.data, er.data)
all.data <- do.call(rbind, data.sources.list)
all.data <- all.data[with(all.data, order(LanguageCode, Source, InventoryID)),]
## should all be denormalized already, but make sure
# all.data$Phoneme <- denorm(all.data$Phoneme)
# all.data$Allophones <- denorm(all.data$Allophones)
## Assign glyph IDs
all.data$GlyphID <- assignGlyphID(all.data$Phoneme)


## ## ## ## ## ## ## ## ## ##
## LOAD THE FEATURES TABLE ##
## ## ## ## ## ## ## ## ## ##
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
## READ IN FEATURES FROM TABLE
feats <- read.delim(features.path, sep="\t")
feats$segment <- orderIPA(feats$segment)
feats <- checkDuplicateFeatures(feats)
feats$GlyphID <- assignGlyphID(feats$segment)
all.data <- merge(all.data, feats, by.x="GlyphID", by.y="GlyphID", all.x=TRUE,
                  all.y=FALSE, sort=FALSE)
## handle UPSID disjuncts
upsid.disjunct.indices <- stri_detect_fixed(all.data$Phoneme, "|")
upsid.disjuncts <- stri_split_fixed(all.data$Phoneme[upsid.disjunct.indices],
                                    pattern="|")
upsid.feats <- do.call(rbind, lapply(upsid.disjuncts, function(i) {
    left.index <- which(feats$segment == i[1])
    right.index <- which(feats$segment == i[2])
    matches <- unlist(lapply(seq_along(feats[left.index,]), function(i) {
                             feats[left.index, i] == feats[right.index, i] }))
    output <- feats[left.index,]
    output[!matches] <- 0
    output$segment <- paste(i, collapse="|")
    output
}))
all.data[upsid.disjunct.indices, feat.columns] <- upsid.feats[feat.columns]
## sort
all.data <- all.data[with(all.data, order(InventoryID, GlyphID)),]
## clean up
if (clear.intermed.files) rm(upsid.feats, upsid.disjunct.indices, upsid.disjuncts,
                             ph.data, aa.data, spa.data, upsid.data, ra.data,
                             gm.data, saphon.data, uz.data, ea.data)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## MARK DUPLICATE INVENTORIES USING TRUMP ORDERING ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
all.data$Source <- factor(all.data$Source, levels=trump.order, ordered=TRUE)
split.trump <- lapply(split(all.data[,trump.tiebreaker],
                            all.data[[trump.group]]), computeTrump)
all.data$Trump <- unsplit(split.trump, all.data[[trump.group]])$Trump
rownames(all.data) <- NULL
if (clear.intermed.files) rm(split.trump)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## CONVERT UNVALUED FEATURES FROM 0 TO NA ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
if (convert.unvalued.to.NA) {
    all.data[feat.columns][all.data[feat.columns] %in% "0"] <- NA
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
## dump debugging info to log
sink(phone.validity.log)
print(unfamiliar.glyphs)
sink()
## clean up
if (clear.intermed.files) rm(features.path, ph.path, aa.path, spa.path,
                             spa.ipa.path, spa.iso.path, upsid.segments.path,
                             upsid.language.codes.path, upsid.ipa.path, ra.path,
                             gm.afr.path, gm.sea.path, saphon.path,
                             saphon.ipa.path, unfamiliar.glyphs)
## reset options
options(stringsAsFactors=saf)
