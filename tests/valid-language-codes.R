#! /usr/bin/env Rscript

## This script checks that the phoible language codes are valid ISO 639-3 codes.
## The output goes into the root folder. Note that bad codes can be easily
## looked up using the following URL patterns (replace XXX with desired code):
## Main ethnologue entry:
## http://www.ethnologue.com/language/XXX
## ISO 639-3 change history for code:
## http://www-01.sil.org/iso639-3/documentation.asp?id=XXX
## Full list of retired codes:
## http://www-01.sil.org/iso639-3/codes_retired.asp

## set global options (restored at end)
saf <- getOption("stringsAsFactors")
options(stringsAsFactors=FALSE)

## file I/O
root.dir <- file.path("..")
results.dir <- file.path(root.dir, "results")
in.file  <- file.path(root.dir, "data", "phoible-by-phoneme.RData")
out.file <- file.path(results.dir, "bad-iso-codes.tsv")

## URLs
iso.url <- "http://www-01.sil.org/iso639-3/iso-639-3.tab"

## load data
load(in.file)  # final.data
iso639.3 <- read.delim(iso.url, stringsAsFactors=FALSE)

## pull out language codes
iso.codes <- iso639.3[,1]
phoible.codes <- final.data$LanguageCode

## filter phoible data on bad codes
bad.isos <- final.data[!phoible.codes %in% iso.codes,]
bad.isos <- bad.isos[!duplicated(bad.isos$InventoryID),
                     c("LanguageCode", "LanguageName", "Source")]

## write results
write.table(bad.isos, out.file, row.names=FALSE, col.names=TRUE,
            sep="\t", eol="\n")

## reset options
options(stringsAsFactors=saf)
