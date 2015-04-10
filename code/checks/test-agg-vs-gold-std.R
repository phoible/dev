#! /usr/bin/env Rscript

library(stringi)

root.dir <- file.path("..", "..")

load(file.path(root.dir, "phoible-phoneme-level.RData"))  # final.data
gold.standard <- read.delim(file.path(root.dir, "phoible-phonemes.tsv"))

agg.langs <- unique(final.data$LanguageCode)
gs.langs <- unique(gold.standard$LanguageCode)

# # # # # # # # # # # # # # # # # # # # # # # # # # #
# DENORMALIZE AND THEN RE-NORMALIZE UNICODE STRINGS #
# # # # # # # # # # # # # # # # # # # # # # # # # # #
denormRenorm <- function(x) {
    s <- stri_trans_general(stri_trans_general(x, "Any-NFD"), "Any-NFC")
}

# # # # # # # # # # # # # # # # # # # # # # # #
# languages in agg-final not in Gold Standard #
# # # # # # # # # # # # # # # # # # # # # # # #
setdiff(agg.langs, gs.langs)
## "eza" <- izi
## "iqw" <- izi
## "izz" <- izi
## "zoh" PH, san miguel chimalapa zoque; gold standard has wrong ISO code (zoc)

# # # # # # # # # # # # # # # # # # # # # # # #
# languages in Gold Standard not in agg-final #
# # # # # # # # # # # # # # # # # # # # # # # #
setdiff(gs.langs, agg.langs)
## "izi" -> izz, eza, iqw, gmz
## "idn" (Indonesian, GM): Rdata has correct code (ind)
## "azb" (South Azerbaijani, SPA): Rdata has correct code (azj)

## other mismatches (gold standard vs aggregate)
## SPA ayr vs jqr
## SPA btx vs bbc
## SPA yux vs ykg

## fix up ISO code mismatches so we can check if phonemes match
gold.standard$LanguageCode[gold.standard$LanguageCode == "zoc" &
                           gold.standard$Source == "PH"] <- "zoh"
gold.standard$LanguageCode[gold.standard$LanguageCode == "idn"] <- "ind"
gold.standard$LanguageCode[gold.standard$LanguageCode == "azb"] <- "azj"
gold.standard$LanguageName <- denormRenorm(gold.standard$LanguageName)
ezaa <- denormRenorm(c("ezaa", "ẹzaa"))
gold.standard$LanguageCode[gold.standard$LanguageName == "izi"] <- "izz"
gold.standard$LanguageCode[gold.standard$LanguageName %in% ezaa] <- "eza"
gold.standard$LanguageCode[gold.standard$LanguageName == "ikwo"] <- "iqw"
gold.standard$LanguageCode[gold.standard$LanguageCode == "yux" & gold.standard$Source == "SPA"] <- "ykg"
gold.standard$LanguageCode[gold.standard$LanguageCode == "btx" & gold.standard$Source == "SPA"] <- "bbc"
gold.standard$LanguageCode[gold.standard$LanguageCode == "ayr" & gold.standard$Source == "SPA"] <- "jqr"
final.data$InventoryID <- with(final.data, paste(LanguageCode, Source, sep="-"))
gold.standard$InventoryID <- with(gold.standard, paste(LanguageCode, tolower(Source), sep="-"))

## Prep data frames for comparing phonemes
common.columns <- c("LanguageCode", "LanguageName", "Source", "Phoneme", "InventoryID")
compare.gs <- gold.standard[with(gold.standard, stri_order(Phoneme)), common.columns]
compare.gs <- compare.gs[stri_order(compare.gs$Source),]
compare.gs <- compare.gs[stri_order(compare.gs$LanguageName),]
compare.gs <- compare.gs[stri_order(compare.gs$LanguageCode),]
compare.gs$Source <- tolower(compare.gs$Source)
compare.gs$Phoneme <- denormRenorm(compare.gs$Phoneme)
rownames(compare.gs) <- NULL

compare.fd <- final.data[stri_order(final.data$Phoneme), common.columns]
compare.fd <- compare.fd[stri_order(compare.fd$Source),]
compare.fd <- compare.fd[stri_order(compare.fd$LanguageName),]
compare.fd <- compare.fd[stri_order(compare.fd$LanguageCode),]
compare.fd$Phoneme <- denormRenorm(compare.fd$Phoneme)
rownames(compare.fd) <- NULL
## Get rid of UPSID dental|alveolar disjuncts
compare.fd$Phoneme <- sapply(strsplit(compare.fd$Phoneme, "|", fixed=TRUE),
                             function(i) i[1])  # takes the first half (the dental)

## split by InventoryID
split.fd <- split(compare.fd, compare.fd$InventoryID)
split.gs <- split(compare.gs, compare.gs$InventoryID)

## gather list of mismatches for each language
in.eith <- union(names(split.fd), names(split.gs))
#in.both <- intersect(names(split.fd), names(split.gs))
## use "in.both" below to ignore languages that are missing from either FD or GS
phoneme.mismatches <- t(sapply(in.eith, function(i)
                        data.frame(agg=paste(setdiff(split.fd[[i]]$Phoneme,
                                                     split.gs[[i]]$Phoneme),
                                             collapse=" "),
                                   gold=paste(setdiff(split.gs[[i]]$Phoneme,
                                                      split.fd[[i]]$Phoneme),
                                              collapse=" "))))
## get rid of null mismatches
phoneme.mismatches <- phoneme.mismatches[apply(phoneme.mismatches, 1,
                                               paste, collapse="") != "",]

## write out results
write.table(phoneme.mismatches, file.path(root.dir, "agg-vs-gold-mismatches.tsv"), sep="\t",
            row.names=TRUE)
