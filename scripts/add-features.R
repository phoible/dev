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




combine_feat_vecs <- function(feat_mat, special=FALSE, null_feats=FALSE) {
    ## handle the easiest case first
    if (nrow(feat_mat) == 1) return(feat_mat)
    ## the first few columns aren't feature values; track which ones to ignore
    ignore_cols <- 1:(which(colnames(feat_mat) == "tone") - 1)
    feat_mat_cols <- which(colnames(feat_mat) == "tone"):ncol(feat_mat)
    ## next easiest case: undefined features for some components
    if (any(is.na(feat_mat[feat_mat_cols]))) {
        null_feats <- TRUE
    }
    ## count base glyphs; unless "special" or toneme, there should be exactly 1
    base_row <- which(feat_mat$GlyphType %in% "B")
    ## handle null phoneme "∅"
    if (nrow(feat_mat) == 1 && feat_mat$GlyphType %in% "N") {
        null_feats <- TRUE
    }
    if (null_feats) {
        feat_vec <- initialize_feat_vec(feat_mat, zero=TRUE)
        return(feat_vec)
    }
    ## handle specials (affricates & doubly-articulateds mostly)
    if (special) {
        ## create dummy feature vector
        feat_vec <- initialize_feat_vec(feat_mat, zero=TRUE)
        ## take only the base glyphs and look them up in the special_feats table
        base_mat <- feat_mat[feat_mat$GlyphType == "B",]
        base_seg <- paste(base_mat$segment, collapse="")
        base_vec <- special_feats[special_feats$segment == base_seg,]
        if (nrow(base_vec) == 0) {
            warning(base_seg, " is not in special_feats table.", immediate.=TRUE)
        }
        ## populate the feature vector from the special table
        feat_vec[feat_mat_cols] <- base_vec[feat_mat_cols]
        ## handle duplicated diacritics/modifiers
        for (post_row in (base_row[1] + 1):nrow(feat_mat)) {
            post_vec <- feat_mat[post_row,]
            glyph_type <- post_vec["GlyphType"]
            ## which features are valued?
            valued <- find_valued_feats(post_vec, ignore_cols)
            ## base glyph feat values are already included (from table)
            if (glyph_type %in% "B") {
                next
            ## diacritics & modifier letters (overwrite base feat vals)
            } else if (glyph_type %in% c("D", "M")) {
                # skip the retraction diacritic on t
                if ("t" %in% base_seg && post_vec$segment == "̠") {
                    next
                }
                feat_vec[valued] <- post_vec[valued]
            ## contour modifier letters
            } else if (glyph_type %in% "C") {
                feat_vec[valued] <- paste(feat_vec[valued],
                                          post_vec[valued], sep=",")
            }
        }
        ## now do pre-modifiers
        if (base_row[1] > 1) {
            for (pre_row in 1:(base_row[1] - 1)) {
                pre_vec <- feat_mat[pre_row,]
                ## non-feature columns
                feat_vec$segment <- paste0(pre_vec$segment, feat_vec$segment)
                feat_vec$GlyphType <- paste0(pre_vec$GlyphType, feat_vec$GlyphType)
                feat_vec$GlyphID <- paste(pre_vec$GlyphID, feat_vec$GlyphID,
                                          sep="+")
                ## prepend feat values in columns that are valued in pre_vec
                valued <- find_valued_feats(pre_vec, ignore_cols)
                feat_vec[valued] <- paste(pre_vec[valued], feat_vec[valued],
                                          sep=",")
                ## duplicate feat values that the pre-modifier doesn't specify
                ## (will get simplified later when possible)
                zero_cols <- which(pre_vec %in% "0")
                feat_vec[zero_cols] <- paste(base_vec[zero_cols],
                                             feat_vec[zero_cols], sep=",")
            }
        }
        return(feat_vec)
    }
    if (length(base_row) > 1) {
        warning("combine_feat_vecs got a matrix with multiple base glyphs. ",
                "That should never happen; please contact PHOIBLE developers.")
        return(NULL)
    } else if (length(base_row) == 0) {
        ## should be a toneme, but it may have laryngeal diacritics before it
        feat_vec <- initialize_feat_vec(feat_mat)
        ## handle pure tonemes; no feature merging needed
        if (all(feat_mat$GlyphType %in% "T")) return(feat_vec)
        ## merge feature columns
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
                    stop("some feature values got skipped when building a toneme")
                }
            }
        }
        return(feat_vec)
    }
    ## if we got this far, we have only 1 base row (the normal case)
    stopifnot(length(base_row) == 1)
    base_vec <- feat_mat[base_row,]
    feat_vec <- feat_mat[base_row,]
    if (base_row < nrow(feat_mat)) {
        ## do diacritics first, since they overwrite base glyph feature values
        for (post_row in (base_row + 1):nrow(feat_mat)) {
            post_vec <- feat_mat[post_row,]
            glyph_type <- post_vec$GlyphType
            nonzero <- which(!post_vec %in% "0")[-ignore_cols]
            ## non-feature columns
            feat_vec$segment <- paste0(feat_vec$segment, post_vec$segment)
            feat_vec$GlyphType <- paste0(feat_vec$GlyphType,
                                         post_vec$GlyphType)
            feat_vec$GlyphID <- paste(feat_vec$GlyphID, post_vec$GlyphID,
                                      sep="+")
            ## diacritics & modifier letters
            if (glyph_type %in% c("D", "M")) {
                feat_vec[nonzero] <- post_vec[nonzero]
            ## contour modifier letters
            } else if (glyph_type %in% "C") {
                feat_vec[nonzero] <- paste(feat_vec[nonzero],
                                           post_vec[nonzero], sep=",")
            } else {
                ## NB: this will catch valid typestring "N" (if it occurs with
                ## diacritics or other base glyphs)
                #print(feat_mat)
                #assign("feat_mat", feat_mat, envir=.GlobalEnv)
                warning("encountered unexpected glyph type: ", glyph_type,
                        " in phoneme ", paste(feat_mat$segment, collapse=""))
                feat_vec[feat_mat_cols] <- NA_character_
            }
        }
    }
    ## now do pre-modifiers
    if (base_row > 1) {
        for (pre_row in 1:(base_row - 1)) {
            pre_vec <- feat_mat[pre_row,]
            ## non-feature columns
            feat_vec$segment <- paste0(pre_vec$segment, feat_vec$segment)
            feat_vec$GlyphType <- paste0(pre_vec$GlyphType, feat_vec$GlyphType)
            feat_vec$GlyphID <- paste(pre_vec$GlyphID, feat_vec$GlyphID,
                                      sep="+")
            ## prepend feat values in columns that are valued in pre_vec
            commasep <- paste(pre_vec, feat_vec, sep=",")
            nonzero <- which(!pre_vec %in% "0")[-ignore_cols]
            feat_vec[nonzero] <- paste(pre_vec[nonzero], feat_vec[nonzero],
                                       sep=",")
            ## duplicate feat values that the pre-modifier doesn't specify
            ## (will get simplified later when possible)
            zero_cols <- which(pre_vec %in% "0")
            feat_vec[zero_cols] <- paste(base_vec[zero_cols],
                                         feat_vec[zero_cols], sep=",")
        }
    }
    ## simplify
    feat_vec[feat_mat_cols] <- lapply(feat_vec[feat_mat_cols],
                                      simplify_contour_feat)
    feat_vec
}

## OLD
################################################################################
## NEW

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
        ## append/replace features based on diacritics/modifiers
        for (row in 1:nrow(feat_mat)) {
            vec <- feat_mat[row,]
            glyph_type <- vec["GlyphType"]
            ## which features are valued for this diacritic?
            valued <- find_valued_feats(post_vec, ignore_cols)
            ## skip base glyph rows, they're already incorporated within the
            ## special_feats table entry
            if (glyph_type %in% "B") next
            ## pre-modifiers
            if (row < base_row_nums[1]) {
                feat_vec[feat_cols] <- paste(vec[feat_cols],
                                             feat_vec[feat_cols], sep=",")
            ## diacritics & modifier letters (overwrite base feat vals)
            } else if (glyph_type %in% c("D", "M")) {
                ## skip the retraction diacritic on t
                most_recent_base_row <- rev(base_row_nums[base_row_nums < row])[1]
                most_recent_base <- feat_mat[most_recent_base_row, "segment"]
                if (most_recent_base %in% "t" && vec$segment == "̠") next
                ## overwrite base feat vals
                feat_vec[valued] <- overwrite_last_feat_val(feat_vec[valued],
                                                            post_vec[valued])
            ## contour modifier letters
            } else if (glyph_type %in% "C") {
                feat_vec[valued] <- paste(feat_vec[valued], post_vec[valued],
                                          sep=",")
            } else {
                stop("error when merging a special_feats phoneme: unexpected ",
                     "glyph type ", glyph_type, " in phoneme ", feat_vec$segment)
            }
        }
        return(feat_vec)
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

    ## TODO: resume here XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    
    ## the bulk of cases: one or more base glyphs (but not special cases)
    feat_vecs <- list()
    for (base_num in seq_along(base_row_nums)) {
        ## include pre-modifiers if first base isn't in first row
        start <- ifelse((base_num == 1) && (base_row_nums[base_num] > 1),
                        1, base_row_nums[base_num])
        ## include until end, if this is last base;
        ## otherwise include up to just before next base
        end <- ifelse(base_num == length(base_row_nums),
                      nrow(feat_mat), base_row_nums[base_num + 1] - 1)
        ## pass to feature combiner
        feat_vec <- combine_feat_vecs(feat_mat[start:end,])
        feat_vecs <- c(feat_vecs, list(feat_vec))
    }
    ## combine across base glyphs
    feat_vec <- as.data.frame(lapply(do.call(rbind, feat_vecs), paste,
                                     collapse=","))
    ## fix up non-feature columns (they don't use comma sep)
    feat_vec$segment <- stri_replace_all_fixed(feat_vec$segment,
                                               pattern=",",
                                               replacement="")
    feat_vec$GlyphType <- stri_replace_all_fixed(feat_vec$GlyphType,
                                                 pattern=",",
                                                 replacement="")
    feat_vec$GlyphID <- stri_replace_all_fixed(feat_vec$GlyphID,
                                               pattern=",",
                                               replacement="+")
    ## simplify
    ## TODO: remove hard-coded column numbers
    feat_vec[-3:-1] <- lapply(feat_vec[-3:-1], simplify_contour_feat)
    feat_vec
}



build_features_from_id <- function(glyph_id) {
    ## prepare to handle special cases
    special_cases <- c("pɸ", "pf", "tθ", "ts", "tʃ", "ʈʂ", "c\u00E7", "kx",
                       "qχ", "bβ", "bv", "dð", "dz", "dʒ", "ɖʐ", "ɟʝ", "ɡɣ",
                       "ɢʁ", "kp", "ɡb")
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


stop()

## build feature vectors from component glyphs
build_features <- function(glyph_id) {
    ## populate function environment with glyph types
    create_glyph_type_variables(envir=environment())
    ## prepare to handle special cases
    special_cases <- c("pɸ", "pf", "tθ", "ts", "tʃ", "ʈʂ", "c\u00E7", "kx",
                       "qχ", "bβ", "bv", "dð", "dz", "dʒ", "ɖʐ", "ɟʝ", "ɡɣ",
                       "ɢʁ", "kp", "ɡb")
    ## prepare to handle disjuncts
    is_disjunct <- any(stri_detect_fixed(get_glyph(glyph_id), "|"))
    if (is_disjunct) {
        glyph_ids <- stri_split_fixed(glyph_id, pattern="+007C+")
    } else {
        glyph_ids <- list(glyph_id)
    }
    ## split glyph_ids into individual codepoints
    codepoints <- unlist(lapply(glyph_ids, stri_split_fixed, pattern="+"),
                         recursive=FALSE)
    ## get matrix of feature vectors for each glyph, in order
    these_feat_mats <- lapply(codepoints, get_feat_matrix_from_codepoints,
                              table=feats)
    ## do the heavy lifting...
    these_feat_vecs <- lapply(these_feat_mats, function(this_feat_mat) {
        ## add glyph type column (to the beginning, so all the feature columns
        ## are still at the end)
        old_colnames <- colnames(this_feat_mat)
        this_feat_mat$GlyphType <- sapply(this_feat_mat$GlyphID,
                                          get_glyph_type_from_codepoint)
        this_feat_mat <- this_feat_mat[c("GlyphType", old_colnames)]
        rownames(this_feat_mat) <- NULL
        ## handle the easy case first
        if (dim(this_feat_mat)[1] == 1) return(this_feat_mat)
        ## next handle case where there are no base glyphs
        bases <- paste(this_feat_mat[this_feat_mat$GlyphType %in% "B",
                                     "segment"], collapse="")
        if (nchar(bases) == 0) {
            ## probably a toneme
            if ("T" %in% this_feat_mat$GlyphType) {
                return(combine_feat_vecs(this_feat_mat))
            } else {
                warning("Glyph ", glyph_id, " has no base glyph and is not a ",
                        "toneme, I don't know how handle it. Its feature ",
                        "vector will be all zeros.")
                return(combine_feat_vecs(this_feat_mat, null_feats=TRUE))
            }
        }
        ## check bases against special_cases & handle accordingly
        is_special <- any(stri_detect_fixed(bases, pattern=special_cases))
        if (is_special) return(combine_feat_vecs(this_feat_mat, special=TRUE))

        ## the bulk of cases: one or more base glyphs (but not special cases)
        base_row_nums <- which(this_feat_mat$GlyphType == "B")
        feat_vecs <- list()
        for (base_num in seq_along(base_row_nums)) {
            ## include pre-modifiers if first base isn't in first row
            start <- ifelse((base_num == 1) && (base_row_nums[base_num] > 1),
                            1, base_row_nums[base_num])
            ## include until end, if this is last base;
            ## otherwise include up to just before next base
            end <- ifelse(base_num == length(base_row_nums),
                          nrow(this_feat_mat), base_row_nums[base_num + 1] - 1)
            ## pass to feature combiner
            feat_vec <- combine_feat_vecs(this_feat_mat[start:end,])
            feat_vecs <- c(feat_vecs, list(feat_vec))
        }
        ## combine across base glyphs
        feat_vec <- as.data.frame(lapply(do.call(rbind, feat_vecs), paste,
                                        collapse=","))
        ## fix up non-feature columns (they don't use comma sep)
        feat_vec$segment <- stri_replace_all_fixed(feat_vec$segment,
                                                   pattern=",",
                                                   replacement="")
        feat_vec$GlyphType <- stri_replace_all_fixed(feat_vec$GlyphType,
                                                    pattern=",",
                                                    replacement="")
        feat_vec$GlyphID <- stri_replace_all_fixed(feat_vec$GlyphID,
                                                   pattern=",",
                                                   replacement="+")
        ## simplify
        ## TODO: remove hard-coded column numbers
        feat_vec[-3:-1] <- lapply(feat_vec[-3:-1], simplify_contour_feat)
        feat_vec
    })
    ## finish
    len <- length(these_feat_vecs)
    this_feat_vec <- these_feat_vecs[[1]]
    if (len == 2) {
        ## combine across disjuncts
        codept_sep <- paste0("+", get_codepoints("|"), "+")
        these_feat_vecs <- do.call(rbind, these_feat_vecs)
        this_feat_vec$GlyphType <- paste(these_feat_vecs$GlyphType,
                                         collapse="|")
        this_feat_vec$segment <- paste(these_feat_vecs$segment, collapse="|")
        this_feat_vec$GlyphID <- paste(these_feat_vecs$GlyphID, 
                                       collapse=codept_sep)
        ## TODO: remove hard-coded column numbers
        this_feat_vec[-3:-1] <- lapply(these_feat_vecs[-3:-1],
                                       simplify_disjunct)
    } else if (len != 1) {
        stop("these_feat_vecs has length ", len, ", which should not happen.")
    }
    this_feat_vec
}

all_feats <- do.call(rbind, lapply(phoible$GlyphID, build_features))
save(all_feats, file=file.path("..", "data", "phoible-by-phoneme-with-feats.RData"))

## RESET GLOBAL OPTIONS
options(stringsAsFactors=saf)

## CRUFT ######################################################################
stop()

## COMPUTE MISMATCHES BETWEEN LOOKED-UP AND AUTOBUILT FEATURES
library(dplyr)
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