#! /usr/bin/env Rscript

get_feat_matrix_from_codepoints <- function(codepoints, table=feats) {
    rows <- sapply(codepoints, function(i) which(table$GlyphID == i))
    if (class(rows) != "integer") {
        missing_ix <- which(!as.logical(sapply(rows, length)))
        stop("missing features for codepoint(s) ", codepoints[missing_ix])
    }
    table[rows,]
}


get_glyph_type_from_codepoint <- function(codepoint) {
    glyph <- stri_trans_general(paste0("U+", codepoint), "Hex-Any/Unicode")
    create_glyph_type_variables(envir=environment())
    glyph_types <- list(base=base_glyphs, tone=tones, modifier=modifiers,
                        diacritic=diacritics, null_phone=null_phone,
                        disjunct=disjunct)
    matches <- sapply(glyph_types, function(gtype) glyph %in% gtype)
    n_matches <- sum(as.numeric(matches))
    if (n_matches > 1) {
        stop("Glyph ", glyph, " (", codepoint, ") matched multiple glyph types: ",
             paste(names(which(matches)), collapse=", "))
    } else if (n_matches == 0) {
        stop("Glyph ", glyph, " (", codepoint, ") matched no glyph types")
    }
    glyph_codes <- c(base="B", tone="T", modifier="M", diacritic="D",
                     null_phone="N", disjunct="|")
    glyph_codes[names(which(matches))]
}


simplify_contour_feat <- function(feat_val) {
    if (length(unique(stri_split_fixed(feat_val, pattern=",")[[1]])) == 1) {
        return(stri_sub(feat_val, from=1, to=1))
    }
    feat_val
}


simplify_disjunct_feat <- function(feat_column) {
    if (length(unique(feat_column)) == 1) return(feat_column[1])
    else                                  return("0")
}


merge_disjunct_feat_vecs <- function(feat_vec_list) {
    ## arbitrarily start with first disjunct
    feat_vec <- feat_vec_list[[1]]
    ## rbind
    feat_vec_mat <- do.call(rbind, feat_vec_list)
    ## define the separator
    codept_separator <- paste0("+", get_codepoints("|"), "+")
    ## rebuild non-feat columns
    feat_vec$GlyphType <- paste(feat_vec_mat$GlyphType, collapse="|")
    feat_vec$segment <- paste(feat_vec_mat$segment, collapse="|")
    feat_vec$GlyphID <- paste(feat_vec_mat$GlyphID, collapse=codept_separator)
    ## zero out features that mismatch between disjuncts
    ignore_cols <- which(colnames(feat_vec_mat) %in% c("GlyphType", "segment",
                                                       "GlyphID"))
    feat_vec[-ignore_cols] <- lapply(feat_vec_mat[-ignore_cols],
                                     simplify_disjunct_feat)
    feat_vec
}


initialize_feat_vec <- function(feat_mat, zero=FALSE) {
    feat_vec <- feat_mat[1,]
    feat_vec$GlyphID <- paste(feat_mat$GlyphID, collapse="+")
    feat_vec$GlyphType <- paste(feat_mat$GlyphType, collapse="")
    feat_vec$segment <- paste(feat_mat$segment, collapse="")
    if (zero) {
        feat_vec[which(colnames(feat_mat) == "tone"):ncol(feat_mat)] <- NA_character_
    }
    feat_vec
}

find_valued_feats <- function(feat_vec, ignore_cols) {
    nonzero <- !feat_vec %in% "0"
    nonmissing <- !is.na(feat_vec)
    valued <- which(nonzero & nonmissing)[-ignore_cols]
}


overwrite_last_feat_val <- function(old_feats, new_feats) {
    for (i in seq_along(old_feats)) {
        idx <- nchar(old_feats[i])
        stri_sub(old_feats[i], from=idx, to=idx) <- new_feats[i]
    }
    old_feats
}
