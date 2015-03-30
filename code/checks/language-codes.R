#! /usr/bin/env Rscript

# This script checks that the phoible ISO 639-3 codes are valid

library(RCurl)

## set global options (to be restored at end)
saf <- getOption("stringsAsFactors")
options(stringsAsFactors=FALSE)

## file I/O
out.file <- file.path("..", "..", "bad-iso-codes.tsv")

## URLs
#eth.url <- "http://www.ethnologue.com/sites/default/files/LanguageCodes.tab"
iso.url <- "http://www-01.sil.org/iso639-3/iso-639-3.tab"
agg.url <- "https://raw.githubusercontent.com/phoible/phoible/master/phoible-aggregated.tsv"

## load data
#ethnologue <- read.delim(eth.url, stringsAsFactors=FALSE)
iso639.3 <- read.delim(iso.url, stringsAsFactors=FALSE)
aggregated <- read.delim(text=getURL(agg.url), stringsAsFactors=FALSE)

## find bad language codes
#eth.codes <- ethnologue[,1]
iso.codes <- iso639.3[,1]
agg.codes <- aggregated$LanguageCode
bad.iso.codes <- agg.codes[!agg.codes %in% iso.codes]
#bad.eth.codes <- agg.codes[!agg.codes %in% eth.codes]

## write results
write.table(bad.iso.codes, out.file, row.names=FALSE, col.names=FALSE, sep="\t")

## reset options
options(stringsAsFactors=saf)
