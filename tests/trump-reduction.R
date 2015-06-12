#! /usr/bin/env Rscript

# This script tests to make sure that each ISO code in the aggregated data file
# represents only one doculect (i.e., has information from only one Source).
# Otherwise, we would end up with LanguageCode == "xxx" with Phoneme entries
# from both "upsid" and "spa" (for example). If the two sources listed exactly 
# the same phonemes, this would lead to language xxx having two entries for each
# of its phonemes.

root.dir <- file.path("..")
data.dir <- file.path(root.dir, "data")
results.dir <- file.path(root.dir, "results")
results.file <- file.path(results.dir, "trump-reduction-results.txt")
load(file.path(data.dir, "phoible-by-phoneme.RData"))  # "final.data"
by.iso <- split(final.data, final.data$LanguageCode)

checkTrump <- function(inventory) {
    length(unique(inventory$Source)) > 1
}

result <- sapply(by.iso, checkTrump)

sink(results.file)
cat("Run this script on a file that has undergone trump reduction. If trump",
	"reduction succeeded, the result should be 'character(0)' and nothing will",
	"appear below this paragraph. Otherwise, the ISO codes of problematic",
	"languages will appear below.\n\n")
print(names(result[result]))
sink()
