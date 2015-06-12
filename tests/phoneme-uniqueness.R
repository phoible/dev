#! /usr/bin/env Rscript

## This script tests to make sure that each ISO code in the aggregated data file
## has fully unique phoneme entries (i.e., no duplicate records).

root.dir <- file.path("..")
results.dir <- file.path(root.dir, "results")
results.file <- file.path(results.dir, "phoneme-uniqueness-results.txt")
load(file.path(root.dir, "data", "phoible-by-phoneme.RData"))  # "final.data"
by.id <- split(final.data, final.data$InventoryID)

checkUniqueness <- function(inventory) {
    length(unique(inventory$GlyphID)) < length(inventory$GlyphID)
}

showSources <- function(inventory) {
    paste(unique(inventory$Source), collapse=" ")
}

countDiscrepancy <- function(inventory) {
    length(inventory$GlyphID) - length(unique(inventory$GlyphID))
}

showDiscrepancy <- function(inventory) {
    discrepants <- table(inventory$Phoneme) > 1
    names(discrepants[discrepants])
}

result <- sapply(by.id, checkUniqueness)
sources <- sapply(by.id, showSources)
discrepancy <- sapply(by.id, countDiscrepancy)
discrepants <- sapply(by.id, showDiscrepancy)

sink(results.file)
cat("If all languages have fully unique phoneme inventories,",
    "there should be nothing below this paragraph.\nOtherwise, the ISO codes",
    "of problematic languages will appear below, along with the number of",
    "duplicates, and finally a list of the duplicates.\n\n")
cat(paste(names(result[result]), sources[result], discrepancy[result],
          lapply(discrepants[result], paste, collapse=" "),
          sep="\t", collapse="\n"))
sink()
