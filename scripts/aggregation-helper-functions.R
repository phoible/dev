#! /usr/bin/env Rscript

parse_sparse <- function(dataframe, id_col="InventoryID", fill_cols=NULL) {
    ## helper function to parse long-and-sparse source data
    dataframe[[id_col]] <- zoo::na.locf(dataframe[[id_col]])
    split_frame <- split(dataframe, dataframe[[id_col]])
    dataframe <- unsplit(lapply(split_frame, fill_cells, fill_cols),
                         dataframe[[id_col]])
}


fill_cells <- function(dataframe, cols) {
    ## propogate values down columns in long-and-sparse data sources
    for(col in cols) {
        if(!is.na(match(col, colnames(dataframe)))) {
            thiscol <- dataframe[[col]]
            if(!is.na(thiscol[1])) thiscol <- zoo::na.locf(thiscol)
            dataframe[[col]] <- thiscol
        } else {
            stop(paste("Bad column name passed to 'fill_cells':", col))
        }
    }
    dataframe
}


collapse_allophones <- function (dataframe, split_col="InventoryID") {
    ## collapse allophones to a single cell (make data one phoneme per row)
    ## retain NAs in Allophones column if data source had no allophonic info
    which_na <- is.na(dataframe$Allophones)
    if (all(which_na)) return (dataframe)
    ## replace NAs in Allophones column with the phoneme representation
    dataframe$Allophones[which_na] <- dataframe$Phoneme[which_na]
    ## remove brackets
    dataframe$Allophones <- remove_brackets(dataframe$Allophones,
                                            type="square")
    ## split data by inventory
    by_inv <- split(dataframe, dataframe[[split_col]])
    ## loop through inventories...
    by_inv <- lapply(by_inv, function (i) {
        ## split inventories by phoneme
        by_pho <- split(i, i$Phoneme)
        ## loop through phonemes...
        by_pho <- lapply(by_pho, function (j) {
            allo <- j$Allophones
            if (!j$Phoneme[1] %in% allo) allo <- c(j$Phoneme[1], allo)
            j[1, "Allophones"] <- paste(allo, collapse=" ")
            j[1,]
        })
        ## unsplit inventory
        inv <- do.call(rbind, by_pho)
    })
    ## unsplit data
    dataframe <- do.call(rbind, by_inv)
    dataframe
}


validate_data <- function(dataframe, source_id,
                          output_cols=c("Phoneme", "Allophones", "Marginal",
                                        "SegmentClass", "InventoryID", "Source",
                                        "LanguageCode", "LanguageName",
                                        "SpecificDialect", "FileNames"),
                          debug=FALSE) {
    ## clean up and standardize imported data
    ## mark marginal phonemes
    if (!"Marginal" %in% colnames(dataframe)) {
        dataframe <- mark_marginal(dataframe)
    }
    ## add missing columns
    for (col in output_cols) {
        if (!col %in% colnames(dataframe)) dataframe[[col]] <- NA
    }
    ## apply canonical ordering of glyphs
    if(debug) {
        ## Keep track of distribution of segment length (# of codepoints)
        ## before & after, as a sanity check: numbers should change very little
        phonemes_original <- table(nchar(dataframe$Phoneme))
        allophones_original <- table(nchar(remove_brackets(dataframe$Allophones,
                                                           "square")))
    }
    dataframe$Phoneme <- order_ipa(dataframe$Phoneme)
    dataframe$Allophones <- order_ipa(dataframe$Allophones)
    dataframe$SegmentClass <- assign_seg_class(dataframe$Phoneme)
    if(debug) {
        phonemes_canonical <- table(nchar(dataframe$Phoneme))
        allophones_canonical <- table(nchar(dataframe$Allophones))
        tabs <- list("phonemes (original)"=phonemes_original,
                     "phonemes (canonical)"=phonemes_canonical,
                     "allophones (original)"=allophones_original,
                     "allophones (canonical)"=allophones_canonical)
        ## check phoneme and allophone string length for possible invalid data.
        ## If anything looks odd, can examine interactively after the fact:
        ## load("phoible-by-phoneme.RData")  # loads "final.data"
        ## with(final.data, Phoneme[nchar(Phoneme) > 7])
        ## with(final.data, Allophones[nchar(Allophones) > 11])
        ## (7 and 11 are reasonable cutoffs based on table values, edit as
        ## needed)
        tab <- do.call(rbind, lapply(lapply(tabs, unlist), "[",
                                    unique(unlist(c(sapply(tabs, names))))))
        tab[is.na(tab)] <- 0
        colnames(tab) <- seq(ncol(tab))
        total <- apply(tab, 1, sum)
        tab <- cbind(tab, "total"=total)
        cat("Table of codepoints per phone (", source_id, "):\n", sep="")
        print(tab)
        cat("\n")
    }
    ## collapse allophones
    dataframe <- collapse_allophones(dataframe)
    ## assign source ID
    dataframe$Source <- source_id
    ## remove blank lines & extraneous columns
    dataframe <- dataframe[!is.na(dataframe$Phoneme), output_cols]
    ## trim whitespace
    dataframe <- data.frame(lapply(dataframe, stringi::stri_trim_both))
}


assign_seg_class <- function(segment) {
    create_glyph_type_variables(envir=environment())
    consonants <- c(stops, nasals, fricatives, flaps, affricates, implosives,
                    approximants, clicks, archephonemes)
    # make the regex patterns
    tone_pattern <- paste0("[", paste(tones, collapse=""), "]")
    vowel_pattern <- paste0("[", paste(vowels, collapse=""), "]")
    consonant_pattern <- paste0("[", paste(consonants, collapse=""), "]")
    # detect the segment classes
    is_null <- stri_detect_fixed(segment, pattern="∅")
    is_tone <- stri_detect_regex(segment, pattern=tone_pattern)
    is_vowel <- stri_detect_regex(segment, pattern=vowel_pattern)
    is_consonant <- stri_detect_regex(segment, pattern=consonant_pattern)
    # make sure full coverage
    stopifnot(all(is_null | is_consonant | is_vowel | is_tone))
    # make sure no overlaps
    stopifnot(all((is_null + is_consonant + is_vowel + is_tone) == 1))
    # populate vector
    segment_class <- character(length(segment))
    segment_class[is_null] <- "null"
    segment_class[is_tone] <- "tone"
    segment_class[is_vowel] <- "vowel"
    segment_class[is_consonant] <- "consonant"
    # return
    segment_class
}


denorm <- function(strings, fix_c_cedilla=TRUE) {
    ## denormalize strings
    denormed <- stri_trans_general(strings, "Any-NFD")
    if (fix_c_cedilla) {
        # renormalize c-cedilla
        c_cedilla <- "\u00E7"
        c_cedilla_denormed <- "\u0063\u0327"
        denormed <- stri_replace_all_fixed(denormed, pattern=c_cedilla_denormed,
                                           replacement=c_cedilla)
    }
    denormed
}


get_codepoints <- function(strings) {
    ## convert a sequence of glyphs in a string into plus-separated codepoints
    codepts <- stri_trans_general(strings, "Any-Hex/Unicode")
    codepts <- stri_replace_all_fixed(codepts, replacement="", pattern = "U")
    stri_replace_first_fixed(codepts, replacement="", pattern = "+")
}


get_glyph <- function(glyph_id, sep="+") {
    ## convert a (sequence of) plus-separated glyph ID(s) into a (character
    ## vector of) glyph string(s)
    codepts <- stri_split_fixed(glyph_id, pattern=sep)
    codepts <- lapply(codepts, function(x) paste0("U+", x))
    glyphs <- lapply(codepts, stri_trans_general, "Hex/Unicode-Any")
    sapply(glyphs, function(x) paste(x, collapse=""))
}


add_arch_stars <- function(strings) {
    ## add asterisks to archephonemes
    stri_replace_all_fixed(strings, pattern=c("R", "N"),
                           replacement=c("*R", "*N"), vectorize_all=FALSE)
}


remove_arch_stars <- function(strings) {
    ## remove asterisks from archephonemes
    stri_replace_all_fixed(strings, pattern=c("*R", "*N"),
                           replacement=c("R", "N"), vectorize_all=FALSE)
}


add_brackets <- function(strings, type) {
    ## add square or angle brackets. type must be "square" or "angle"
    braks <- switch(type, square=c("[", "]"), angle=c("<", ">"))
    strings <- paste0(braks[1], strings, braks[2])
}


remove_brackets <- function(strings, type=c("square", "angle", "tiebar")) {
    ## remove square or angle brackets and/or tiebars
    for(brak_type in type) {
        braks <- switch(brak_type, square=c("[", "]"), angle=c("<", ">"),
                        tiebar=c("͡", "͜"))
        for(brak in braks) {
            strings <- stri_replace_first_fixed(strings, pattern=brak,
                                                replacement="")
        }
    }
    strings
}


mark_marginal <- function (dataframe) {
    ## mark marginal phonemes in a boolean column; remove <angle brackets>
    dataframe$Marginal <- stri_detect_fixed(dataframe$Phoneme, "<")
    dataframe$Phoneme <- remove_brackets(dataframe$Phoneme, "angle")
    dataframe
}


keep_first <- function(strings, split.on) {
    strings <- sapply(stri_split_fixed(strings, split.on), function(x) x[1])
}


make_typestring <- function(strings, ...) {
    if(!"USE.NAMES" %in% names(list(...))) USE.NAMES <- FALSE
    ## make sure we have what we need within the function environment
    create_glyph_type_variables(envir=environment())
    typestrings <- sapply(strings, function(string) {
        if (is.na(string)) return(NA)
        chars <- strsplit(string, "")[[1]]
        codepts <- get_codepoints(chars)
        codepts[codepts %in% get_codepoints(base_glyphs)] <- "B"
        codepts[codepts %in% get_codepoints(modifiers)] <- "M"
        codepts[codepts %in% get_codepoints(contour_glyphs)] <- "C"
        codepts[codepts %in% get_codepoints(diacritics)] <- "D"
        codepts[codepts %in% get_codepoints(tones)] <- "T"
        codepts[codepts %in% get_codepoints(null_phone)] <- "N"
        codepts[codepts %in% get_codepoints(disjunct)] <- "|"
        missed <- !codepts %in% c("B", "M", "C", "D", "T", "N", "|")
        if (any(missed)) {
            warning(paste("Unfamiliar glyph components.", "Phone:", string,
                          "Codepoint:", codepts[missed]),
                    call.=FALSE, immediate.=TRUE)
            unfamiliar <- data.frame(phoneme=string, codepoint=codepts[missed])
            if(exists("unfamiliar_glyphs")) {
                assign("unfamiliar_glyphs", pos=.GlobalEnv,
                       rbind(unfamiliar_glyphs, unfamiliar))
            } else {
                assign("unfamiliar_glyphs", pos=.GlobalEnv, unfamiliar)
            }
        }
        # restore original glyphs
        codepts[missed] <- chars[missed]
        paste0(codepts, collapse="")
    }, USE.NAMES=USE.NAMES, ...)
    typestrings
}


order_ipa <- function(strings, keep_stars=FALSE, keep_brackets=TRUE) {
    if(all(is.na(strings))) return(strings)
    ## make sure we have what we need within the function environment
    create_glyph_type_variables(envir=environment())
    ## start by denormalizing
    strings <- denorm(strings)
    ## replace *R/*N with R/N
    strings <- remove_arch_stars(strings)
    ## if square or angle brackets exist, set them aside for later. Ignore NAs.
    has_square <- stri_detect_fixed(strings, "[")
    has_angle <- stri_detect_fixed(strings, "<")
    has_square[is.na(has_square)] <- FALSE
    has_angle[is.na(has_angle)] <- FALSE
    strings <- remove_brackets(strings, type=c("square", "angle"))
    ## remove whitespace & tiebars
    strings <- remove_brackets(strings, type="tiebar")
    strings <- stri_trim(strings)
    ## construct parallel string showing character classes
    typestrings <- make_typestring(strings)
    canonical_strings <- sapply(seq_along(strings), function(i) {
        typestring <- typestrings[i]
        if (is.na(typestring)) return(NA)
        typstr <- strsplit(typestring, "")[[1]]
        string <- strsplit(strings[i], "")[[1]]
        lenstr <- length(string)
        ## move tones to beginning
        if (stri_detect_fixed(typestring, "T")) {
            indices <- seq_len(lenstr)
            tone_indices <- stri_locate_all_fixed(typestring, "T")[[1]][,"start"]
            neworder <- c(tone_indices, setdiff(indices, tone_indices))
            string <- string[neworder]
            typstr <- typstr[neworder]
            typestring <- paste(typstr, collapse="")
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
                ## debugging
                if (sum(diacritics %in% dcrs) != length(dcrs)) {
                    print(typestring)
                    print(paste(string, collapse=""))
                    print(string)
                    print(get_codepoints(dcrs))
                    print(get_codepoints(diacritics[diacritics %in% dcrs]))
                    stop("FOO BAR BAZ")
                }
                string[span] <- diacritics[diacritics %in% dcrs]
        }   }
        paste(string, collapse="")
    }, USE.NAMES=FALSE)
    ## restore asterisks
    if (keep_stars) canonical_strings <- add_stars(canonical_strings)
    ## restore brackets
    if (keep_brackets) {
        canonical_strings[has_square] <- add_brackets(canonical_strings[has_square],
                                                    type="square")
        canonical_strings[has_angle] <- add_brackets(canonical_strings[has_angle],
                                                    type="angle")
    }
    canonical_strings
}


create_glyph_type_variables <- function(..., envir=.GlobalEnv) {
    ## define glyph types and add the variables to the specified environment
    inputs <- c(...)
    if (!length(inputs)) {
        inputs <- c("tones", "modifiers", "diacritics", "stops", "nasals",
                    "fricatives", "flaps", "affricates", "implosives",
                    "approximants", "clicks", "vowels", "archephonemes",
                    "base_glyphs", "contour_glyphs", "null_phone", "disjunct")
    }
    ## DEFINITION OF GLYPH TYPES
    ## Tones are not internally reordered so the order here is arbitrary.
    tones <- c("˩", "˨", "˧", "˦", "˥", "↓", "↘")
    ## Order of elements in "diacritics" and "modifiers" sets canonical order!!
    modifiers <- c(
        "˞",  # rhotic wing
        "ⁿ",  # nasal release
        "ˡ",  # lateral release
        "ʷ",  # labialized
        "ʲ",  # palatalized
        "ᶣ",  # labial-palatalized
        "ˠ",  # velarized
        "ˤ",  # pharyngealized
        "ˀ",  # glottalized
        "ᵊ",  # schwa-like release
        "ᴱ",  # epilaryngeal source
        "ʰ",  # aspirated
        "ʱ",  # breathy aspirated
        ## NOT USED "˭",  # unaspirated
        "ʼ",  # ejective
        "ː",  # long
        "ˑ"   # half-long
    )
    diacritics <- c(
        "̴",  # velarized/pharyngealized (combining tilde overlay)
        "̼",  # linguolabial (combining seagull below)
        "̪",  # dental (combining bridge below)
        "̺",  # apical (combining inverted bridge below)
        "̻",  # laminal (combining square below)
        "̟",  # advanced (combining plus sign below)
        "̠",  # retracted (combining minus sign below)
        "͇",  # non-sibilant (combining equals sign below)
        "̝",  # raised (combining up tack below)
        "̞",  # lowered (combining down tack below)
        "̘",  # advanced tongue root (combining left tack below)
        "̙",  # retracted tongue root (combining right tack below)
        "͓",  # frictionalized (combining x below)
        "̹",  # more round (combining right half ring)
        "̜",  # less round (combining left half ring)
        ## NOT USED "̮",  # derhoticized (combining breve below)
        "̰",  # creaky (combining tilde below)
        "̤",  # breathy (combining diaresis below)
        "̬",  # stiff (combining caron below)
        "̥",  # devoiced (combining ring below)
        "̊",  # devoiced (combining ring above)
        "͈",  # fortis (combining double vertical line below)
        "͉",  # lenis (combining left angle below)
        "̆",  # short (combining breve)
        "̩",  # syllabic (combining vertical line below)
        "̯",  # non-syllabic (combining inverted breve below)
        "̃",  # nasalized (combining tilde)
        "͊",  # denasalized (combining not tilde above)
        ## NOT USED "͋",  # nasal emission (combining homothetic)
        "̈",  # centralized (combining diaresis)
        "̽",  # mid-centralized (combining x above)
        "̚"   # unreleased (combining left angle above)
    )
    ## "contour_glyphs" are the modifiers & diacritics that create a contour
    ## with their base glyph's feature values, rather than overwriting them.
    ## This grouping is not used for canonical ordering, but for feature vector
    ## construction.
    contour_glyphs <- c("ⁿ", "ˡ")  # nasal release, lateral release
    ## the null phone *should* only occur in allophones
    null_phone <- "∅"
    ## disjunct is used in UPSID to signify insufficient information to
    ## distinguish a phoneme precisely (usually arises as "dental|alveolar")
    disjunct <- "|"
    ## The base glyphs are broken up into subtypes for convenience only;
    ## their order does not matter.
    vowels <- c("i", "y", "ɨ", "ʉ", "ɯ", "u", "ɪ", "ʏ", "ʊ", "e", "ø", "ɘ",
                "ɵ", "ɤ", "o", "ə", "ɛ", "œ", "ɜ", "ɞ", "ʌ", "ɔ", "æ", "ɐ",
                "a", "ɶ", "ɑ", "ɒ", "ɚ", "ɝ")
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
    approximants <- c("ʋ", "ɹ", "ɻ", "j", "ɥ", "ɰ", "l", "ȴ", "ɭ", "ʎ", "ʟ", "ɫ",
                      "w")
    archephonemes <- c("R", "N")  # R = tap/trill; N = placeless nasal
    ## assign variables to the specified environment
    if ("stops" %in% inputs)          assign("stops", stops, envir)
    if ("flaps" %in% inputs)          assign("flaps", flaps, envir)
    if ("tones" %in% inputs)          assign("tones", tones, envir)
    if ("nasals" %in% inputs)         assign("nasals", nasals, envir)
    if ("vowels" %in% inputs)         assign("vowels", vowels, envir)
    if ("clicks" %in% inputs)         assign("clicks", clicks, envir)
    if ("disjunct" %in% inputs)       assign("disjunct", disjunct, envir)
    if ("modifiers" %in% inputs)      assign("modifiers", modifiers, envir)
    if ("null_phone" %in% inputs)     assign("null_phone", null_phone, envir)
    if ("diacritics" %in% inputs)     assign("diacritics", diacritics, envir)
    if ("fricatives" %in% inputs)     assign("fricatives", fricatives, envir)
    if ("affricates" %in% inputs)     assign("affricates", affricates, envir)
    if ("implosives" %in% inputs)     assign("implosives", implosives, envir)
    if ("approximants" %in% inputs)   assign("approximants", approximants,
                                             envir)
    if ("archephonemes" %in% inputs)  assign("archephonemes", archephonemes,
                                             envir)
    if ("contour_glyphs" %in% inputs) assign("contour_glyphs", contour_glyphs,
                                             envir)
    if ("base_glyphs" %in% inputs) {
        assign("base_glyphs", c(vowels, stops, implosives, flaps, nasals, 
                                clicks, fricatives, affricates, approximants,
                                archephonemes), envir)
    }
}
