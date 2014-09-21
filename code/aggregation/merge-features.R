#! /usr/bin/R

# This script reads in the output from aggregate-raw-data.R and adds feature
# vectors for each phoneme. It writes te resulting data.frame to the root
# directory of the repository.

library(stringi)  # for proper string handling & (de)normalization

input.fname <- file.path("..", "..", "phoible-alldata-phonemes.tsv")
output.fname <- file.path("..", "..", "phoible-alldata-phonemes-features.tsv")

# LOAD THE LANGUAGE DATA
all.data <- read.delim(input.fname)

# LOAD THE FEATURE TABLE
feats <- read.delim(features.path, sep='\t', stringsAsFactors=TRUE)
feats$segment <- denorm.renorm(feats$segment)
feat.columns <- c("tone", "stress", "syllabic", "short", "long", 
                  "consonantal", "sonorant", "continuant", 
                  "delayedRelease", "approximant", "tap", "trill", 
                  "nasal", "lateral", "labial", "round", "labiodental", 
                  "coronal", "anterior", "distributed", "strident", 
                  "dorsal", "high", "low", "front", "back", "tense", 
                  "retractedTongueRoot", "advancedTongueRoot", 
                  "periodicGlottalSource", "epilaryngealSource", 
                  "spreadGlottis", "constrictedGlottis", "fortis", 
                  "raisedLarynxEjective", "loweredLarynxImplosive", 
                  "click")
all.data <- merge(all.data, feats, by.x="Phoneme", by.y="segment", all.x=TRUE,
                  all.y=FALSE, sort=FALSE)

# HANDLE UPSID DISJUNCTS
upsid.disjunct.indices <- grepl("|", all.data$Phoneme, fixed=TRUE)
upsid.disjuncts <- strsplit(as.character(all.data$Phoneme[upsid.disjunct.indices]),
                            split="|", fixed=TRUE)
upsid.feats <- do.call(rbind, lapply(upsid.disjuncts, function(i) {
    left.index <- which(feats$segment == i[1])
    right.index <- which(feats$segment == i[2])
    matches <- unlist(lapply(seq_along(feats[left.index,]), 
                             function(i) feats[left.index, i] == feats[right.index, i]))
    output <- feats[left.index,]
    output[!matches] <- 0
    output$segment <- paste(i, collapse="|")
    return(output)
}))
all.data[upsid.disjunct.indices, feat.columns] <- upsid.feats[feat.columns]

# WRITE OUT DATA
output.columns <- c(colnames(all.data), feat.columns)
write.table(all.data[,output.columns], file=output.fname, sep="\t", eol="\n",
            row.names=FALSE, quote=FALSE, fileEncoding="UTF-8")
