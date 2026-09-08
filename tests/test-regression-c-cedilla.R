#! /usr/bin/env Rscript

## Regression test for c-cedilla staying precomposed in order_ipa()
## (aggregation-helper-functions.R), rather than decomposing it.

library(stringi, warn.conflicts=FALSE)
library(testthat, warn.conflicts=FALSE)

source(file.path("..", "scripts", "aggregation-helper-functions.R"))

context("Special-case exceptions regression")

test_that("c-cedilla is unchanged by order_ipa()", {
    ordered <- order_ipa("ç")
    expect(identical(ordered, "ç"),
           paste("Expected c-cedilla phoneme to be unchanged by order_ipa(), got",
                 shQuote(ordered)))
})
