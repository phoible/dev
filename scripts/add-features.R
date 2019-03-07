#! /usr/bin/env Rscript

## This script adds feature values to the PHOIBLE data.

library(stringi)  # for proper string handling & (de)normalization

## GLOBAL OPTIONS (restored at end)
saf <- getOption("stringsAsFactors")
options(stringsAsFactors=FALSE, width=132, warn=2)

## LOAD EXTERNAL HELPER FUNCTIONS
## provides: denorm, get_codepoints, order_ipa, make_typestring,
## create_glyph_type_variables (also provides other functions not used here)
source("aggregation-helper-functions.R")
## provides: get_feat_matrix_from_codepoints, get_glyph_type_from_codepoint,
## simplify_contour_feat, simplify_disjunct, initialize_feat_vec,
## find_valued_feats, overwrite_last_feat_val
source("feature-helper-functions.R")

## LOAD PHONEME DATA
load(file.path("..", "data", "phoible-nofeats.RData"))

## LOAD FEATURE TABLES
feats <- read.csv(file.path("..", "raw-data", "FEATURES",
                            "component-feature-table.csv"), na.strings="")
feature_colnames <- colnames(feats[-2:-1])

special_feats <- read.csv(file.path("..", "raw-data", "FEATURES",
                          "special-feature-table.csv"))
## add glyph type column to special feature table
special_feats$GlyphType <- strrep("B", nchar(special_feats$segment))


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
    if (glyph_id %in% "0272+031F+0064+0291") {
        NULL
    }
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
    ## add glyph type column
    feat_mat$GlyphType <- sapply(feat_mat$GlyphID,
                                 get_glyph_type_from_codepoint)
    rownames(feat_mat) <- NULL
    ## the first few columns aren't feature values; track which ones to ignore
    ignore_cols <- which(colnames(feat_mat) %in% c("segment", "GlyphID",
                                                   "GlyphType"))
    feature_cols <- which(colnames(feat_mat) %in% feature_colnames)

    ## DISPATCH EASIEST CASE: only 1 row, nothing to combine
    if (nrow(feat_mat) == 1) return(feat_mat)

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
        feat_vec <- initialize_feat_vec(feat_mat, feature_colnames, zero=TRUE)
        ## look up the combined base glyphs in the special_feats table
        base_vec <- special_feats[special_feats$segment == bases,]
        if (nrow(base_vec) == 0) {
            warning(bases, " is not in special_feats table.", immediate.=TRUE)
        }
        ## populate the feature vector from the special table
        feat_vec[feature_cols] <- base_vec[feature_cols]
        feat_vec <- apply_diacritic_features(feat_mat, feat_vec, base_row_nums,
                                             ignore_cols, feature_cols)
        ## simplify
        feat_vec[feature_cols] <- simplify_contour_feats(feat_vec[feature_cols])
        return(feat_vec)
    }

    ## HANDLE TONEMES
    if (nchar(bases) == 0) {
        ## probably a toneme
        if ("T" %in% feat_mat$GlyphType) {
            feat_vec <- initialize_feat_vec(feat_mat, feature_colnames)
            ## handle mixed (tonemes + laryngeal diacritics)
            for (row_num in 2:nrow(feat_mat)) {
                post_vec <- feat_mat[row_num,]
                nonzero <- which(!is.na(post_vec) & !post_vec %in% "0")
                nonzero <- setdiff(nonzero, ignore_cols)
                zero <- which(feat_vec %in% "0")
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
            return(initialize_feat_vec(feat_mat, feature_colnames, zero=TRUE))
        }
    }
    
    ## THE BULK OF CASES: ONE OR MORE BASE GLYPHS (BUT NOT IN SPECIAL CASE TABLE)
    feat_vecs <- list()
    for (num in seq_along(base_row_nums)) {
        vec <- initialize_feat_vec(feat_mat, feature_colnames,
                                   rownum=base_row_nums[num])
        ## include pre-modifiers if first base isn't in first row
        start <- ifelse((num == 1) && (base_row_nums[num] > 1),
                        1, base_row_nums[num])
        ## include until end, if this is last base;
        ## otherwise include up to just before next base
        end <- ifelse(num == length(base_row_nums),
                      nrow(feat_mat), base_row_nums[num + 1] - 1)
        ## populate the feature vector from the base glyph
        vec <- apply_diacritic_features(feat_mat[start:end,], vec,
                                        base_row_nums, ignore_cols,
                                        feature_cols)
        feat_vecs <- c(feat_vecs, list(vec))
    }
    ## combine across base glyphs + their diacritics
    feat_vec <- as.data.frame(lapply(do.call(rbind, feat_vecs), paste,
                                     collapse=","))
    ## if non-feat columns are identical, they've already been merged:
    if (length(unique(sapply(feat_vecs, function(i) i$segment))) == 1) {
        feat_vec[ignore_cols] <- feat_vecs[[1]][ignore_cols]
    ## otherwise, fix up non-feature columns (they don't use comma sep)
    } else {
        feat_vec$segment <- stri_replace_all_fixed(feat_vec$segment,
                                                   pattern=",", replacement="")
        feat_vec$GlyphType <- stri_replace_all_fixed(feat_vec$GlyphType,
                                                     pattern=",", replacement="")
        feat_vec$GlyphID <- stri_replace_all_fixed(feat_vec$GlyphID,
                                                   pattern=",", replacement="+")
    }
    ## simplify
    feat_vec[feature_cols] <- simplify_contour_feats(feat_vec[feature_cols])
    feat_vec
}


apply_diacritic_features <- function(feat_mat, feat_vec, base_row_nums,
                                     ignore_cols, feature_cols) {
    ## NOTE: All base glyphs in `feat_mat` will be skipped by this function. The
    ## `feat_vec` argument should already include the features of the base
    ## glyph(s) in feat_mat. It's done this way to accommodate "special cases"
    ## where there are multiple base glyphs that we don't want to combine
    ## algorithmically (affricates, doubly-articulated stops, etc)

    ## contextual diacritics; value depends on base glyph
    contextuals <- c("031D",  # uptack
                     "031E",  # downtack
                     "031F",  # advanced
                     "0308",  # centralized
                     "033D",  # midcentralized
                     "0353",  # frictionalized
                     "0339",  # more rounded
                     "031C")  # less rounded
    ## append/replace features based on diacritics/modifiers
    for (row in row.names(feat_mat)) {
        vec <- feat_mat[row,]
        glyph_type <- vec["GlyphType"]
        ## skip base glyph rows, they're already incorporated within the
        ## special_feats table entry
        if (glyph_type %in% "B") next
        ## which features are valued for this diacritic?
        valued <- find_valued_feats(vec, ignore_cols)
        ## pre-modifiers
        if (row < base_row_nums[1]) {
            feat_vec[feature_cols] <- paste(vec[feature_cols],
                                            feat_vec[feature_cols], sep=",")
        ## diacritics & modifier letters (overwrite base feat vals)
        } else if (glyph_type %in% c("D", "M")) {
            ## get the most recent base row
            most_recent_base_row <- rev(base_row_nums[base_row_nums < row])[1]
            # convert it to a string because it's a row name, not an index
            most_recent_base <- feat_mat[as.character(most_recent_base_row),
                                         "segment"]
            ## skip the retraction diacritic on t, d
            if (most_recent_base %in% c("t", "d") && vec$segment == "̠") next
            ## handle contextual diacritics
            if (vec$GlyphID %in% contextuals) {
                vec <- handle_contextual_diacritics(vec, most_recent_base)
                valued <- find_valued_feats(vec, ignore_cols, keep_zeros=TRUE)
            }
            ## test if there are undefined features for some components
            if (all(is.na(vec[feature_cols]))) {
                return(initialize_feat_vec(feat_mat, feature_colnames,
                                           zero=TRUE))
            }
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


handle_contextual_diacritics <- function(vec, base_glyph) {
    create_glyph_type_variables(envir=environment())
    # uptack
    if (vec$GlyphID %in% "031D") {
        if (base_glyph %in% c("ɹ", "r")) {
            # make it fricative-like, but not strident
            vec$delayedRelease <- "+"
        }
        else if (base_glyph %in% "ʕ") {
            # here we make it a stop... that's supposed to be impossible
            vec$continuant <- "-"
            vec$delayedRelease <- "-"
        }
        else if (base_glyph %in% c("æ", "a", "ɶ", "ɑ", "ɒ")) {
            # ambiguous between open and mid-open
            vec$low <- "0"
        }
        else if (base_glyph %in% c("e", "o", "ɘ", "ɵ", "ø", "ɤ")) {
            # this is not great...
            vec$high <- "0"
        }
        else if (base_glyph %in% c("ɪ", "ɛ", "ɔ", "œ", "ʌ")) {
            # ambiguous between close and mid-close (ɪ)
            # or between mid-close and mid-open (ɛ, ɔ, œ, ʌ)
            vec$tense <- "0"
        }
        else if (base_glyph %in% c("i", "y", "ɨ", "ʉ", "ɯ", "u")) {
            # this is vacuous
            vec$high <- "+"
        }
    }
    # downtack
    else if (vec$GlyphID %in% "031E") {
        if (base_glyph %in% c("ŋ", "d")) {
            # downtack turns stop/nasal into "non-continuant fricative"
            vec$delayedRelease <- "+"
        }
        else if (base_glyph %in% c("β", "ð", "z", "ʝ", "ʁ")) {
            # downtack turns voiced fricative into approximant
            vec$consonantal <- "-"
            vec$sonorant <- "+"
            vec$delayedRelease <- "0"
            vec$approximant <- "+"
        }
        else if (base_glyph %in% c("ɸ", "ʃ")) {
            # downtack turns voiceless fricative into non-sonorant-approximant?
            vec$approximant <- "+"
        }
        else if (base_glyph %in% c("ɾ")) {
            # assuming this means the flap doesn't hit the passive articulator
            vec$tap <- "0"
        }
        else if (base_glyph %in% c("w")) {
            # make it like an approximant version of ʊ
            vec$tense <- "-"
        }
        else if (base_glyph %in% c("e", "o", "ɘ", "ɵ", "ø", "ɤ",
                              "i", "u", "ɨ", "ʉ", "ɯ", "y")) {
            # ambiguous between mid-close and mid-open (e o ɘ ɵ ø ɤ)
            # or between close and mid-close (i u ɨ ʉ ɯ y)
            vec$tense <- "0"
        }
        else if (base_glyph %in% c("ɪ", "ʊ")) {
            # between ɪ and ɛ
            vec$high <- "0"
        }
        else if (base_glyph %in% c("ɛ", "œ", "ə", "ɜ", "ɞ", "ʌ", "ɔ")) {
            # between mid-open and open
            vec$low <- "0"
        }
        else if (base_glyph %in% c("æ", "a")) {
            # assigning +low is vacuous; little else makes sense ???
            vec$low <- "+"
        }
    }
    # advanced
    else if (vec$GlyphID %in% "031F") {
        if (base_glyph %in% "ⱱ") {
            # presumably a bilabial flap?
            vec$labiodental <- "-"
        }
        else if (base_glyph %in% c("ɨ", "ʉ", "ɘ", "ɵ", "ə", "ɐ", "a")) {
            # in between central and front
            vec$front <- "0"
        }
        else if (base_glyph %in% c("ɯ", "u", "ʊ", "ɤ", "o", "ʌ", "ɔ", "ɑ", "ɒ")) {
            # in between back and central
            vec$back <- "0"
        }
        else if (base_glyph %in% c("c", "ɟ", "ç", "ʝ", "ʎ", "ɲ")) {
            # collapses c -> ȶ, ʎ -> ȴ, ɲ -> ȵ
            vec$anterior <- "+"
        }
        else if (base_glyph %in% c("j")) {
            # like the other palatals, but we need to add +coronal
            # this leaves 0distributed and 0strident alone...
            vec$coronal <- "+"
            vec$anterior <- "+"
        }
        else if (base_glyph %in% c("ʈ", "ɖ", "ɳ", "ɭ", "ɻ")) {
            # between retroflex and alveolar
            vec$anterior <- "0"
        }
        else if (base_glyph %in% c("t", "d", "s", "z", "l", "r", "ɕ", "ʑ",
                              "ʃ", "ʒ")) {
            # assigning +anterior is vacuous; should this just be dental ???
            # for ʃ and ʒ this yields the same as s̪ or z̪
            vec$anterior <- "+"
        }
        else if (base_glyph %in% c("k", "ɡ", "x", "ɣ", "ŋ")) {
            # fronted velar
            vec$front <- "+"
        }
    }
    # mid-centralized
    else if (vec$GlyphID %in% "033D") {
        if (base_glyph %in% "i") {
            vec$high <- "0"
            vec$front <- "0"
            vec$tense <- "0"
        } else if (base_glyph %in% c("u", "ɯ")) {
            vec$high <- "0"
            vec$back <- "0"
            vec$tense <- "0"
        } else if (base_glyph %in% "ʊ") {
            vec$high <- "0"
            vec$back <- "0"
        } else if (base_glyph %in% "ɔ") {
            vec$back <- "0"
        }
    }
    # centralized
    else if (vec$GlyphID %in% "0308") {
        if (base_glyph %in% c("ɪ")) {
            vec$front <- "0"
        }
        else if (base_glyph %in% c("ʊ", "ɑ")) {
            vec$back <- "0"
        }
        else if (base_glyph %in% c("i", "e")) {
            vec$front <- "0"
            vec$tense <- "0"
        }
        else if (base_glyph %in% c("o", "u", "w")) {
            vec$back <- "0"
            vec$tense <- "0"
        }
        else if (base_glyph %in% "a") {
            # vacuous; already central in our feat. sys.
            vec$front <- "-"
        }
    }
    # frictionalized
    else if (vec$GlyphID %in% "0353") {
        if (base_glyph %in% c("ɭ", "ʟ", "r", "ɾ")) {
            vec$delayedRelease <- "+"
        }
        else if (base_glyph %in% vowels) {
            # not sure delrel is the right thing to do for vowels...
            vec$delayedRelease <- "+"
        }
        else if (base_glyph %in% clicks) {
            # would +strident be better?
            vec$delayedRelease <- "+"
        }
    }
    # more rounded
    else if (vec$GlyphID %in% "0339") {
        if (base_glyph %in% c("i", "ɪ", "e", "ɛ", "æ", "a", "ɨ", "ɘ", "ɜ",
                              "ɯ", "ɤ", "ʌ", "ɑ", "ə", "z", "ʐ")) {
            # "more round" when unround means half-round, I guess
            vec$round <- "0"
        }
        else if (base_glyph %in% c("y", "ʏ", "ø", "œ", "ɶ", "ʉ", "ɵ", "ɞ", "ɐ",
                              "u", "ʊ", "o", "ɔ", "ɒ")) {
            # vacuous: "more round" when already round
            vec$round <- "+"
        }
    }
    # less rounded
    else if (vec$GlyphID %in% "031C") {
        if (base_glyph %in% c("i", "ɪ", "e", "ɛ", "æ", "a", "ɨ", "ɘ", "ɜ",
                              "ɯ", "ɤ", "ʌ", "ɑ")) {
            # "less round" when already unround must mean lip-compressed (?)
            vec$labial <- "+"
            vec$round <- "-"
            vec$labiodental <- "-"
        }
        else if (base_glyph %in% c("y", "ʏ", "ø", "œ", "ɶ", "ʉ", "ɵ", "ɞ", "ɐ",
                              "u", "ʊ", "o", "ɔ", "ɒ", "w")) {
            # "less round" when round is "ambiguously round"
            vec$round <- "0"
        }
    }
    vec
}


## BUILD FEATURES
unique_glyphs <- unique(phoible_nofeats$GlyphID)
unique_feats <- do.call(rbind, lapply(unique_glyphs, build_features_from_id))

## TESTS
stopifnot(!any(is.na(unique_feats[feature_colnames])))
stopifnot(all(sort(unique(phoible_nofeats$GlyphID)) ==
              sort(unique(unique_feats$GlyphID))))

## MERGE IN FEATURES
phoible <- merge(phoible_nofeats, unique_feats, by="GlyphID", all.x=TRUE,
                 all.y=FALSE)
phoible <- phoible[with(phoible, order(InventoryID, Phoneme)),]

## CLEAN UP COLUMNS
output_cols <- c("InventoryID", "Glottocode", "ISO6393", "LanguageName",
                 "SpecificDialect", "GlyphID", "Phoneme", "Allophones",
                 "Marginal", "SegmentClass", "Source", feature_colnames)
phoible <- phoible[output_cols]

## SAVE
csv_path <- file.path("..", "data", "phoible.csv")
rda_path <- file.path("..", "data", "phoible.RData")
write.csv(phoible, file=csv_path, row.names=FALSE, quote=TRUE, eol="\n",
          fileEncoding="UTF-8")
save(phoible, file=rda_path)

## RESET GLOBAL OPTIONS
options(stringsAsFactors=saf)
