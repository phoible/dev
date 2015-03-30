#! /usr/bin/env Rscript

## This script checks that the phoible language codes are valid ISO 639-3 codes

## set global options (to be restored at end)
saf <- getOption("stringsAsFactors")
options(stringsAsFactors=FALSE)

## file I/O
root.dir <- file.path("..", "..")
in.file  <- file.path(root.dir, "phoible-phoneme-level.RData")
out.file <- file.path(root.dir, "bad-iso-codes.tsv")

## URLs
iso.url <- "http://www-01.sil.org/iso639-3/iso-639-3.tab"

## load data
load(in.file)  # final.data
iso639.3 <- read.delim(iso.url, stringsAsFactors=FALSE)

## find bad language codes
iso.codes <- iso639.3[,1]
phoible.codes <- final.data$LanguageCode

## filter phoible data on bad codes (so we can see Source)
bad.isos <- final.data[!phoible.codes %in% iso.codes,]
bad.isos <- bad.data[!duplicated(bad.data$InventoryID),
                     c("LanguageCode", "LanguageName", "Source")]

## write results
write.table(bad.isos, out.file, row.names=FALSE, col.names=TRUE,
            sep="\t", eol="\n")

## reset options
options(stringsAsFactors=saf)
