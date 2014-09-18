#! /usr/bin/R

# This script reads in the output from aggregate-raw-data.R and adds feature
# vectors for each phoneme. It writes te resulting data.frame to the root
# directory of the repository.

library(stringi)  # for proper string handling & (de)normalization

input.fname <- file.path("..", "..", "phoible-alldata-phonemes-features.tsv")
output.fname <- file.path("..", "..", "phoible-trump-phonemes-features.tsv")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION TO REMOVE DUPLICATE LANGUAGE DATA (RESPECTING TRUMP ORDER) #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
remove.duplicate.langs <- function(x, col) {
    if(length(unique(x[col])) > 1) x <- x[x[col] == min(x[col]),]
    return(x)
}


# TRUMP ORDERING: more preferred data sources come earlier in the list
trump.order <- c("uw", "spa", "aa", "upsid", "ramaswami")  # "casl", "saphon"

# LOAD THE LANGUAGE DATA
all.data <- read.delim(input.fname)

all.data$Source <- factor(all.data$Source, levels=trump.order, ordered=TRUE)
split.data <- split(all.data, all.data$LanguageCode)
reduced.data <- lapply(split.data, remove.duplicate.langs, "Source")
reduced.data <- do.call(rbind, reduced.data)
rownames(reduced.data) <- NULL

# WRITE OUT DATA
write.table(reduced.data, file=output.fname, sep="\t", eol="\n",
            row.names=FALSE, quote=FALSE, fileEncoding="UTF-8")
