# Check that the phoible ISO 639-3 codes are valid

library(RCurl)
library(dplyr)

# load data and standardize code column names
iso639.3 <- read.csv("http://www-01.sil.org/iso639-3/iso-639-3.tab", sep="\t")
colnames(iso639.3)[1] <- "LanguageCode"

ethnologue <- read.csv("http://www.ethnologue.com/sites/default/files/LanguageCodes.tab", sep="\t")
colnames(ethnologue)[1] <- "LanguageCode"

x <- getURL("https://raw.githubusercontent.com/phoible/phoible/master/phoible-aggregated.tsv")
aggregated <- read.csv(text = x, sep='\t')

aggregated <- read.csv("../../phoible-aggregated.tsv", sep="\t")
head(aggregated)

codes <- aggregated$LanguageCode
c <- data.frame(codes %in% iso639.3$LanguageCode)
results <- cbind(codes, c)
names(results) <- c("LanguageCode", "Missing")
filter(results, Missing==FALSE)

# write results
write.table(results, "temp.tsv", sep="\t")