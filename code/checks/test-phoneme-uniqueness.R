#! /usr/bin/env Rscript

# This script tests to make sure that each ISO code in the aggregated data file
# has fully unique phoneme entries (i.e., no duplicate records).

root.dir <- file.path("..", "..")
load(file.path(root.dir, "phoible-phoneme-level.RData"))  # "final.data"
by.iso <- split(final.data, final.data$LanguageCode)

checkUniqueness <- function(inventory) {
    length(unique(inventory$Phoneme)) < length(inventory$Phoneme)
}

result <- sapply(by.iso, checkUniqueness)

sink("test-phoneme-uniqueness-results.txt")
cat("If all languages have fully unique phoneme inventories, the result should be 'character(0)'.\n")
cat("Otherwise, the ISO codes of problematic languages will appear below.\n\n")
cat(names(result[result]))
sink()
