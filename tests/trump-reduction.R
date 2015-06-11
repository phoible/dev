#! /usr/bin/env Rscript

# This script tests to make sure that each ISO code in the aggregated data file
# represents only one doculect (i.e., has information from only one Source).
# Otherwise, we would end up with LanguageCode == "xxx" with Phoneme entries
# from both "upsid" and "spa" (for example). If the two sources listed exactly 
# the same phonemes, this would lead to language xxx having two entries for each
# of its phonemes.

root.dir <- file.path("..", "..")
load(file.path(root.dir, "phoible-phoneme-level.RData"))  # "final.data"
by.iso <- split(final.data, final.data$LanguageCode)

checkTrump <- function(inventory) {
    length(unique(inventory$Source)) > 1
}

result <- sapply(by.iso, checkTrump)

sink("test-trump-reduction-results.txt")
cat("If trump reduction succeeded, the result should be 'character(0)'.\n")
cat("Otherwise, the ISO codes of problematic languages will appear below.\n\n")
print(names(result[result]))
sink()
