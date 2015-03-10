# Check phoible contents against ISO 639-3 language codes

library(RCurl)

checkCodes <- function(df) {
	codes <- df$LanguageCode

	# takes a df and checks for the column name for the language codes - replace with local copy if needed
	iso639.3 <- read.csv("http://www-01.sil.org/iso639-3/iso-639-3.tab", sep="\t")
	ethnologue <- read.csv("http://www.ethnologue.com/sites/default/files/LanguageCodes.tab", sep="\t")

	# standardize code column names
	colnames(iso639.3)[1] <- "LanguageCode"
	colnames(ethnologue)[1] <- "LanguageCode"

	# get sets
	union.input.iso6393.3 <- union(iso639.3 $LanguageCode, aggregated$LanguageCode)
	intersect.input.iso6393.3 <- intersect(iso639.3 $LanguageCode, aggregated$LanguageCode)
	setdiff.input.iso639.3 <- setdiff(iso639.3 $LanguageCode, aggregated$LanguageCode)
	union.input.ethnologue <- union(ethnologue$LanguageCode, aggregated$LanguageCode)
	intersect.input.ethnologue <- intersect(ethnologue$LanguageCode, aggregated$LanguageCode)
	setdiff.input.ethnologue <- setdiff(ethnologue$LanguageCode, aggregated$LanguageCode)


	# deal with some r(rr) types
	results <- list(union.input.ethnologue, union.input.iso6393.3, intersect.input.ethnologue, intersect.input.iso6393.3, setdiff.input.ethnologue, setdiff.input.iso639.3)
	number.codes <- length(iso639.3$LanguageCode) # length of (presumaably) longest input
	results <- sapply(results,'[',1:number.codes)

	results <- data.frame(results)
	colnames(results) <- c("union.input.ethnologue","union.input.iso6393.3", "intersect.input.ethnologue", "intersect.input.iso6393.3", "setdiff.input.ethnologue", "setdiff.input.iso639.3")

	return(results)
}

# assume we are in the working directory and reading from within this git repo

x <- getURL("https://raw.githubusercontent.com/phoible/phoible/master/phoible-aggregated.tsv")
aggregated <- read.csv(text = x, sep='\t')
head(aggregated)
dim(aggregated)

results <- checkCodes(aggregated)
head(results)
write.table(results, "temp.tsv", sep="\t")