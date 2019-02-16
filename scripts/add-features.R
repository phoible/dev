#! /usr/bin/env Rscript

## This script adds feature values to the PHOIBLE data.

library(stringi)  # for proper string handling & (de)normalization

## GLOBAL OPTIONS (restored at end)
saf <- getOption("stringsAsFactors")
options(stringsAsFactors=FALSE, width=132)

## LOAD EXTERNAL HELPER FUNCTIONS
## provides: denorm, get_codepoints, order_ipa, make_typestring,
## create_glyph_type_variables (also provides other functions not used here)
source("aggregation-helper-functions.R")
## provides: get_feat_matrix_from_codepoints, get_glyph_type_from_codepoint,
## simplify_contour_feat, simplify_disjunct, initialize_feat_vec,
## find_valued_feats, overwrite_last_feat_val
source("feature-helper-functions.R")

## LOAD PHONEME DATA
load(file.path("..", "data", "phoible-by-phoneme.RData"))

## LOAD FEATURE TABLES
feats <- read.csv(file.path("..", "raw-data", "FEATURES",
                            "component-feature-table.csv"), na.strings="")

special_feats <- read.csv(file.path("..", "raw-data", "FEATURES",
                          "special-feature-table.csv"))
## prepend glyph type column to special feature table
old_colnames <- colnames(special_feats)
special_feats$GlyphType <- strrep("B", nchar(special_feats$segment))
special_feats <- special_feats[c("GlyphType", old_colnames)]


build_features_from_id <- function(glyph_id) {
    ## prepare to handle disjuncts
    is_disjunct <- any(stri_detect_fixed(get_glyph(glyph_id), "|"))
    if (is_disjunct) {
        glyph_ids <- stri_split_fixed(glyph_id, pattern="+007C+")
    } else {
        glyph_ids <- list(glyph_id)
    }
    ## now each glyph_id is in a list. Most will be 1 element long, only the
    ## UPSID disjuncts should have length==2. Now split list members on + sign
    ## to separate into individual codepoints
    codepoint_list <- unlist(lapply(glyph_ids, stri_split_fixed, pattern="+"),
                             recursive=FALSE)
    ## get matrix of feature vectors for each glyph, in order
    feat_mat_list <- lapply(codepoint_list, get_feat_matrix_from_codepoints,
                            table=feats)
    ## apply the workhorse function
    feat_vec_list <- lapply(feat_mat_list, make_feat_vec_from_mat)
    ## handle disjuncts
    len <- length(feat_vec_list)
    if (len == 1) {
        feat_vec <- feat_vec_list[[1]]
    } else if (len == 2) {
        feat_vec <- merge_disjunct_feat_vecs(feat_vec_list)
    } else {
        stop("feat_vec_list has length ", len, ", which should not happen.")
    }
    feat_vec
}


make_feat_vec_from_mat <- function(feat_mat) {
    ## ROW/COLUMN NAMING AND ORDER HANDLING
    ## add glyph type column (to the beginning, so all the feature columns
    ## are still at the end)
    old_colnames <- colnames(feat_mat)
    feat_mat$GlyphType <- sapply(feat_mat$GlyphID,
                                 get_glyph_type_from_codepoint)
    feat_mat <- feat_mat[c("GlyphType", old_colnames)]
    rownames(feat_mat) <- NULL
    ## the first few columns aren't feature values; track which ones to ignore
    ignore_cols <- 1:(which(colnames(feat_mat) == "tone") - 1)
    feature_cols <- which(colnames(feat_mat) == "tone"):ncol(feat_mat)

    ## DISPATCH A COUPLE EASY CASES
    ## easiest case: only 1 row, nothing to combine
    if (nrow(feat_mat) == 1) return(feat_mat)
    ## next easiest case: if there are undefined features for some components
    if (any(is.na(feat_mat[feature_cols]))) {
        return(initialize_feat_vec(feat_mat, zero=TRUE))
    }

    ## KEEP TRACK OF BASE GLYPHS
    ## which row numbers are base glyphs? (for iteration)
    base_row_nums <- which(feat_mat$GlyphType %in% "B")
    ## collapse just the base glyphs into a string
    bases <- paste(feat_mat[feat_mat$GlyphType %in% "B", "segment"],
                   collapse="")
    
    ## HANDLE SPECIAL CASES (AFFRICATES, DOUBLE-ARTICULATIONS, ETC)
    special_cases <- c("pɸ", "pf", "tθ", "ts", "tʃ", "ʈʂ", "c\u00E7", "kx",
                       "qχ", "bβ", "bv", "dð", "dz", "dʒ", "ɖʐ", "ɟʝ", "ɡɣ",
                       "ɢʁ", "kp", "ɡb")
    if (any(stri_detect_fixed(bases, pattern=special_cases))) {
        ## create dummy feature vector
        feat_vec <- initialize_feat_vec(feat_mat, zero=TRUE)
        ## look up the combined base glyphs in the special_feats table
        base_vec <- special_feats[special_feats$segment == bases,]
        if (nrow(base_vec) == 0) {
            warning(bases, " is not in special_feats table.", immediate.=TRUE)
        }
        ## populate the feature vector from the special table
        feat_vec[feature_cols] <- base_vec[feature_cols]
        return(apply_diacritic_features(feat_mat, feat_vec, ignore_cols,
                                        base_row_nums, feature_cols))
    }

    ## HANDLE TONEMES
    if (nchar(bases) == 0) {
        ## probably a toneme
        if ("T" %in% feat_mat$GlyphType) {
            feat_vec <- initialize_feat_vec(feat_mat)
            ## handle mixed (tonemes + laryngeal diacritics)
            for (row_num in 2:nrow(feat_mat)) {
                post_vec <- feat_mat[row_num,]
                nonzero <- which(!post_vec %in% "0")[-ignore_cols]
                zero <- which(feat_vec %in% "0")  # don't use ignore_cols here!
                overwrite <- intersect(zero, nonzero)
                conflict <- setdiff(nonzero, zero)
                feat_vec[overwrite] <- post_vec[overwrite]
                ## check if mismatch between valued feats in current post_vec and
                ## valued feats in feat_vec (if they match, no action needed)
                for (col_num in conflict) {
                    if (feat_vec[col_num] != post_vec[col_num]) {
                        warning("feature ", colnames(feat_vec)[col_num], " got",
                                " skipped while building features for toneme ",
                                feat_vec$segment)
                    }
                }
            }
            return(feat_vec)
        } else {
            warning("Glyph ", glyph_id, " has no base glyph and is not a ",
                    "toneme, I don't know how handle it. Its feature ",
                    "vector will be all NAs.")
            return(initialize_feat_vec(feat_mat, zero=TRUE))
        }
    }
    
    ## THE BULK OF CASES: ONE OR MORE BASE GLYPHS (BUT NOT IN SPECIAL CASE TABLE)
    feat_vecs <- list()
    for (num in seq_along(base_row_nums)) {
        vec <- initialize_feat_vec(feat_mat, zero=TRUE)
        ## include pre-modifiers if first base isn't in first row
        start <- ifelse((num == 1) && (base_row_nums[num] > 1),
                        1, base_row_nums[num])
        ## include until end, if this is last base;
        ## otherwise include up to just before next base
        end <- ifelse(num == length(base_row_nums),
                      nrow(feat_mat), base_row_nums[num + 1] - 1)
        ## populate the feature vector from the base glyph
        vec[feature_cols] <- feat_mat[num, feature_cols]
        vec <- apply_diacritic_features(feat_mat[start:end,], vec, ignore_cols,
                                        base_row_nums, feature_cols)
        feat_vecs <- c(feat_vecs, list(vec))
    }
    ## combine across base glyphs + their diacritics
    feat_vec <- as.data.frame(lapply(do.call(rbind, feat_vecs), paste,
                                     collapse=","))
    ## fix up non-feature columns (they don't use comma sep)
    feat_vec$segment <- stri_replace_all_fixed(feat_vec$segment,
                                               pattern=",", replacement="")
    feat_vec$GlyphType <- stri_replace_all_fixed(feat_vec$GlyphType,
                                                 pattern=",", replacement="")
    feat_vec$GlyphID <- stri_replace_all_fixed(feat_vec$GlyphID,
                                               pattern=",", replacement="+")
    ## simplify
    feat_vec[feature_cols] <- lapply(feat_vec[feature_cols], simplify_contour_feat)
    feat_vec
}


apply_diacritic_features <- function(feat_mat, feat_vec, ignore_cols,
                                     base_row_nums, feature_cols) {
    ## NOTE: All base glyphs in `feat_mat` will be skipped by this function. The
    ## `feat_vec` argument should already include the features of the base
    ## glyph(s) in feat_mat. It's done this way to accommodate "special cases"
    ## where there are multiple base glyphs that we don't want to combine
    ## algorithmically (affricates, doubly-articulated stops, etc)

    ## append/replace features based on diacritics/modifiers
    for (row in 1:nrow(feat_mat)) {
        vec <- feat_mat[row,]
        glyph_type <- vec["GlyphType"]
        ## which features are valued for this diacritic?
        valued <- find_valued_feats(vec, ignore_cols)
        ## skip base glyph rows, they're already incorporated within the
        ## special_feats table entry
        if (glyph_type %in% "B") next
        ## pre-modifiers
        if (row < base_row_nums[1]) {
            feat_vec[feature_cols] <- paste(vec[feature_cols],
                                            feat_vec[feature_cols], sep=",")
            ## diacritics & modifier letters (overwrite base feat vals)
        } else if (glyph_type %in% c("D", "M")) {
            ## skip the retraction diacritic on t
            most_recent_base_row <- rev(base_row_nums[base_row_nums < row])[1]
            most_recent_base <- feat_mat[most_recent_base_row, "segment"]
            if (most_recent_base %in% "t" && vec$segment == "̠") next
            ## overwrite base feat vals
            feat_vec[valued] <- overwrite_last_feat_val(feat_vec[valued],
                                                        vec[valued])
            ## contour modifier letters
        } else if (glyph_type %in% "C") {
            feat_vec[valued] <- paste(feat_vec[valued], vec[valued], sep=",")
        } else {
            stop("error when merging a special_feats phoneme: unexpected ",
                 "glyph type ", glyph_type, " in phoneme ", feat_vec$segment)
        }
    }
    return(feat_vec)
}


unique_glyphs <- unique(phoible$GlyphID)
unique_feats <- do.call(rbind, lapply(unique_glyphs, build_features_from_id))
all_feats <- merge(phoible, unique_feats, by="GlyphID", all.x=TRUE, all.y=FALSE)
all_feats <- all_feats[with(all_feats, order(InventoryID, Phoneme)),]
save(all_feats, file=file.path("..", "data", "phoible-by-phoneme-with-feats.RData"))

## RESET GLOBAL OPTIONS
options(stringsAsFactors=saf)

################################################################################
## BASIC TEST
################################################################################
library(dplyr)
feature_cols <- which(colnames(all_feats) == "tone"):ncol(all_feats)
foo <- apply(is.na(all_feats[,feature_cols]), 1, any)
missing_feats <- all_feats[which(foo),]

missing_feats %>% distinct(segment, GlyphID, .keep_all=TRUE) %>%
    select(segment, GlyphID, feature_cols) %>%
    write.csv(file=file.path("..", "data", "glyphs-with-na-feats.csv"),
              row.names=FALSE, quote=FALSE)

stop()
################################################################################
## OTHER TESTS
################################################################################

## COMPUTE MISMATCHES BETWEEN LOOKED-UP AND AUTOBUILT FEATURES
segfeat <- read.delim(file.path("..", "raw-data", "FEATURES",
                                "phoible-segments-features.tsv"))
phoible %>% distinct(Phoneme, GlyphID) %>%
            merge(segfeat, by.x="Phoneme", by.y="segment") -> orig
all_feats %>% distinct(Phoneme=segment, GlyphID, .keep_all=TRUE) %>%
              filter(!is.na(tone)) -> built

baz <- intersect(orig$Phoneme, built$Phoneme)
feat_cols <- colnames(orig)[(which(colnames(orig) == "tone")):ncol(orig)]
feat_cols <- c("Phoneme", "GlyphID", feat_cols)

orig %>% filter(Phoneme %in% baz) %>% arrange(Phoneme) %>% select(feat_cols) -> orig
built %>% filter(segment %in% baz) %>% arrange(segment) %>% select(feat_cols) -> built

#all.equal(orig, built)

mismatch.orig <- orig[which(apply(orig != built, 1, all)),]
mismatch.built <- built[which(apply(orig != built, 1, all)),]

stop()

for(diacr in c("031C", "O31D", "031E", "031F", "0353")) {
    foo <- mismatch.built$Phoneme[stri_detect_fixed(mismatch.built$GlyphID, diacr)]
    bar <- unique(phoible$Phoneme[stri_detect_fixed(phoible$GlyphID, diacr)])
    cat(paste(foo, collapse=""), "/n")
    cat(paste(bar, collapse=""), "/n")
}
