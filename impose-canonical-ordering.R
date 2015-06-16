#! /usr/bin/env Rscript

## This script provides a canonical order for diacritics and modifier letters in
## IPA strings. Strings are denormalized, then reordered such that in between
## base glyphs, codepoints are reordered such that all diacritics come before
## all modifier letters. In a sequence of multiple adjacent diacritics, the
## diacritics are reordered to appear in the order they occur in the
## "diacritics" variable (likewise for adjacent modifier letters and the
## "modifiers" variable).

## FUNCTION: denormalize strings
denorm <- function(x) stringi::stri_trans_general(x, "Any-NFD")

## FUNCTION: convert strings to plus-separated codepoints
codepoints <- function(strings) {
    cps <- stringi::stri_trans_general(strings, "Any-Hex/Unicode")
    cps <- stringi::stri_replace_all_fixed(cps, replacement="", pattern = "U")
    cps <- stringi::stri_replace_first_fixed(cps, replacement="", pattern = "+")
    cps
}

## DEFINITION OF GLYPH TYPES. Order of elements in "diacritics" and "modifiers"
## determines canonical order!
modifiers  <- c("˞", "ː", "ˑ", "ⁿ", "ˡ", "ʱ", "ʰ", "˭", "ˀ", "ˤ", "ˠ", "ʲ", "ʷ",
                "ᵊ", "ʼ")
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
## The base glyphs are broken up into types for convenience only; at present
## their order does not matter.
vowels       <- c("i", "y", "ɨ", "ʉ", "ɯ", "u", "ɪ", "ʏ", "ʊ", "e", "ø", "ɘ",
                  "ɵ", "ɤ", "o", "ə", "ɛ", "œ", "ɜ", "ɞ", "ʌ", "ɔ", "æ", "ɐ",
                  "a", "ɶ", "ɑ", "ɒ", "ɚ", "ɝ")
stops        <- c("p", "b", "t", "d", "ʈ", "ɖ", "c", "ɟ", "k", "ɡ", "q", "ɢ",
                  "ʡ", "ʔ")
nasals       <- c("m", "ɱ", "n", "ɳ", "ɲ", "ŋ", "ɴ")
fricatives   <- c("ɸ", "β", "f", "v", "θ", "ð", "s", "z", "ɕ", "ʑ", "ʃ", "ʒ",
                  "ʂ", "ʐ", "ç", "ʝ", "x", "ɣ", "χ", "ʁ", "ħ", "ʕ", "ʜ", "ʢ",
                  "h", "ɦ", "ɬ", "ɮ", "ɧ", "ʍ")
flaps        <- c("ʙ", "ⱱ", "r", "ɾ", "ɽ", "ʀ", "ɺ")
affricates   <- c("ʦ", "ʣ", "ʧ", "ʤ")
implosives   <- c("ƥ", "ɓ", "ƭ", "ɗ", "ƈ", "ʄ", "ƙ", "ɠ", "ʠ", "ʛ")
approximants <- c("ʋ", "ɹ", "ɻ", "j", "ɰ", "l", "ɭ", "ʎ", "ʟ", "ɫ", "w")
clicks       <- c("ʘ", "ǀ", "ǁ", "ǃ", "ǂ", "‼")
consonants <- c(stops, nasals, fricatives, flaps, affricates, implosives,
                approximants, clicks)
base_glyphs <- c(consonants, vowels)


test_strings <- c("aːʰˡʷˠb˭ː", "aː̰̃", "ʱɢǁʲãʷb̤ˡ̥ː")

order_ipa <- function(strings) {
    strings <- denorm(strings)
    chars <- sapply(strings, function(i) {
        typ <- chr <- strsplit(i, "")[[1]]
        typ[typ %in% base_glyphs] <- "B"
        typ[typ %in% modifiers] <- "M"
        typ[typ %in% diacritics] <- "D"
        typ[!typ %in% c("B", "M", "D")] <- NA
        typstr <- paste(typ, collapse="")
        ## If a diacritic comes right after a modifier letter, swap their order
        while (stringi::stri_detect_fixed(typstr, "MD")) {
            ix <- stringi::stri_locate_first_fixed(typstr, "MD")[1]
            if (ix == 1) neworder <- c(2, 1, 3:length(chr))
            else if (ix > length(chr)-2) neworder <- c(1:(ix-1), ix+1, ix)
            else neworder <- c(1:(ix-1), ix+1, ix, (ix+2):length(chr))
            chr <- chr[neworder]
            typ <- typ[neworder]
            typstr <- paste(typ, collapse="")
        }
        ## Put sequences of modifier letters in canonical order
        if (stringi::stri_detect_fixed(typstr, "MM")) {
            ixs <- stringi::stri_locate_all_regex(typstr, "M+")[[1]]
            for (row in seq_len(dim(ixs)[1])) {
                span <- ixs[row,1]:ixs[row,2]
                mods <- chr[span]
                chr[span] <- modifiers[modifiers %in% mods]
        }   }
        ## Put sequences of diacritics in canonical order
        if (stringi::stri_detect_fixed(typstr, "DD")) {
            ixs <- stringi::stri_locate_all_regex(typstr, "D+")[[1]]
            for (row in seq_len(dim(ixs)[1])) {
                span <- ixs[row,1]:ixs[row,2]
                dcrs <- chr[span]
                chr[span] <- diacritics[diacritics %in% dcrs]
        }   }
        paste(chr, collapse="")
    }, USE.NAMES=FALSE)
    chars
}

print(test_strings)
print(order_ipa(test_strings))
print(codepoints(test_strings))
print(codepoints(order_ipa(test_strings)))
