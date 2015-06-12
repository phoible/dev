#! /usr/bin/env Rscript

## This script tests the generated aggregated data file against the last data
## dump from the SQL version of PHOIBLE.

library(stringi)

## set global options (restored at end)
saf <- getOption("stringsAsFactors")
options(stringsAsFactors=FALSE)

## I/O
root.dir <- file.path("..")
results.dir <- file.path(root.dir, "results")
out.file <- file.path(results.dir, "agg-vs-gold-mismatches.tsv")
load(file.path(root.dir, "data", "phoible-by-phoneme.RData"))  # final.data
gold.standard <- read.delim(file.path(root.dir, "gold-standard", "phoible-phonemes.tsv"))

# # # # # # # # # # # # # # # # # # # # # # # # # # #
# DENORMALIZE AND THEN RE-NORMALIZE UNICODE STRINGS #
# # # # # # # # # # # # # # # # # # # # # # # # # # #
denormRenorm <- function(x) {
    s <- stri_trans_general(stri_trans_general(x, "Any-NFD"), "Any-NFC")
}

## which languages are we dealing with?
agg.langs <- unique(final.data$LanguageCode)
gs.langs <- unique(gold.standard$LanguageCode)

# # # # # # # # # # # # # # # # # # # # # # # #
# languages in agg-final not in Gold Standard #
# # # # # # # # # # # # # # # # # # # # # # # #
setdiff(agg.langs, gs.langs)

# # # # # # # # # # # # # # # # # # # # # # # #
# languages in Gold Standard not in agg-final #
# # # # # # # # # # # # # # # # # # # # # # # #
setdiff(gs.langs, agg.langs)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# fix up ISO code mismatches so we can check if phonemes match  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
replace.iso.code <- function(df, oldcode, newcode,
                             sources=c("PH", "GM", "SPA", "AA",
                                       "UPSID", "RA", "SAPHON")) {
    df[with(df, LanguageCode == oldcode & Source %in% sources), "LanguageCode"] <- newcode
    df
}

gold.standard <- replace.iso.code(gold.standard, "idn", "ind")
gold.standard <- replace.iso.code(gold.standard, "azb", "azj")
gold.standard <- replace.iso.code(gold.standard, "noo", "nuk")
gold.standard <- replace.iso.code(gold.standard, "zoc", "zoh", "PH")
gold.standard <- replace.iso.code(gold.standard, "jar", "izr", "PH")
gold.standard <- replace.iso.code(gold.standard, "yux", "ykg", "SPA")
gold.standard <- replace.iso.code(gold.standard, "btx", "bbc", "SPA")
gold.standard <- replace.iso.code(gold.standard, "ayr", "jqr", "SPA")
gold.standard <- replace.iso.code(gold.standard, "psx", "tuo", "SAPHON")
gold.standard <- replace.iso.code(gold.standard, "qpt", "gvp", "SAPHON") # Parkateje, Gavião do Pará
gold.standard <- replace.iso.code(gold.standard, "gny", "pue", "SAPHON") # Gununa Yajich
gold.standard <- replace.iso.code(gold.standard, "adc", "mcd", "SAPHON") # Arara do Acre
gold.standard <- replace.iso.code(gold.standard, "wit", "wnw", "UPSID")
gold.standard <- replace.iso.code(gold.standard, "stc", "ntu", "UPSID")
gold.standard <- replace.iso.code(gold.standard, "nbf", "nxq", "UPSID")
gold.standard <- replace.iso.code(gold.standard, "gbc", "wrk", "UPSID")
gold.standard <- replace.iso.code(gold.standard, "gio", "gqu", "UPSID")
gold.standard <- replace.iso.code(gold.standard, "gmo", "dwr", "UPSID")
gold.standard <- replace.iso.code(gold.standard, "gmo", "gmv", "GM")
gold.standard <- replace.iso.code(gold.standard, "baz", "tvu", "GM")
gold.standard <- replace.iso.code(gold.standard, "daf", "dnj", "AA")
gold.standard <- replace.iso.code(gold.standard, "daf", "lda", c("UPSID", "GM"))
gold.standard <- replace.iso.code(gold.standard, "dap", "njz", c("UPSID", "SPA"))

gold.standard$LanguageName <- denormRenorm(gold.standard$LanguageName)
ezaa <- denormRenorm(c("ezaa", "ẹzaa"))
gold.standard$LanguageCode[gold.standard$LanguageName == "izi"] <- "izz"
gold.standard$LanguageCode[gold.standard$LanguageName %in% ezaa] <- "eza"
gold.standard$LanguageCode[gold.standard$LanguageName == "ikwo"] <- "iqw"

final.data$InvID <- with(final.data, paste(LanguageCode, Source, sep="-"))
gold.standard$InvID <- with(gold.standard, paste(LanguageCode, tolower(Source), sep="-"))
gold.standard$InventoryID <- paste(gold.standard$InvID, "00", sep="-")

## Prep data frames for comparing phonemes
common.columns <- c("LanguageCode", "LanguageName", "Source", "Phoneme", "InvID",
                    "InventoryID")
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
split.fd <- split(compare.fd, compare.fd$InvID)
split.gs <- split(compare.gs, compare.gs$InvID)

## gather list of mismatches for each language
in.eith <- union(names(split.fd), names(split.gs))
#in.both <- intersect(names(split.fd), names(split.gs))
## use "in.both" below to ignore languages that are missing from either FD or GS
phoneme.mismatches <- t(sapply(in.eith, function(i)
                        data.frame(ID=paste(unique(split.fd[[i]]$InventoryID), collapse=" "),
                                   agg=paste(setdiff(split.fd[[i]]$Phoneme,
                                                     split.gs[[i]]$Phoneme),
                                             collapse=" "),
                                   gold=paste(setdiff(split.gs[[i]]$Phoneme,
                                                      split.fd[[i]]$Phoneme),
                                              collapse=" "))))
## get rid of null mismatches
nonnull.mismatch.indices <- apply(phoneme.mismatches[,2:3], 1, paste, collapse="") != ""
phoneme.mismatches <- phoneme.mismatches[nonnull.mismatch.indices,]
## write out results
write.table(phoneme.mismatches, out.file, sep="\t", row.names=TRUE)

## reset options
options(stringsAsFactors=saf)
