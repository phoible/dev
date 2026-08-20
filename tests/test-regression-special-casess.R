#! /usr/bin/env Rscript

## Regression test for two special-cased exceptions in the R pipeline:
## c-cedilla staying precomposed in order_ipa() (aggregation-helper-functions.R) and
## the `special_cases` affricate list in add-features.R having a
## matching row in special-feature-table.csv for each pair.

library(stringi, warn.conflicts=FALSE)
library(testthat, warn.conflicts=FALSE)

source(file.path("..", "scripts", "aggregation-helper-functions.R"))

context("Special-case exceptions regression")

test_that("known special-cased exceptions in the pipeline still work", {
    ## order_ipa() leaves c-cedilla unchanged, rather than decomposing it
    ordered <- order_ipa("ç")
    expect(identical(ordered, "ç"),
           paste("Expected c-cedilla phoneme to be unchanged by order_ipa(), got",
                 shQuote(ordered)))

    ## every add-features.R `special_cases` pair has a matching row in special-feature-table.csv 
    special_cases <- c("pɸ", "pf", "tθ", "ts", "tʃ", "ʈʂ", "cç", "kx",
                       "qχ", "bβ", "bv", "dð", "dz", "dʒ", "ɖʐ", "ɟʝ", "ɡɣ",
                       "ɢʁ", "kp", "ɡb")
    special_feats <- read.csv(file.path("..", "raw-data", "FEATURES",
                              "special-feature-table.csv"),
                              fileEncoding="UTF-8")
    missing <- setdiff(special_cases, special_feats$segment)
    expect(length(missing) == 0,
           paste("special_cases pair(s) missing from special-feature-table.csv:",
                 paste(missing, collapse=", ")))
})
