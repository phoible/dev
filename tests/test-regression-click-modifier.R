#! /usr/bin/env Rscript

## Regression test for the click/modifier typing in order_ipa() (aggregation-helper-functions.R). 
## Clicks are deliberately typed "M", the same glyph-type code as real modifier letters
## so they factor into feature-vector assignment the same way 
## (see create_glyph_type_variables()/make_typestring()). 

library(stringi, warn.conflicts=FALSE)
library(testthat, warn.conflicts=FALSE)

source(file.path("..", "scripts", "aggregation-helper-functions.R"))

context("Click/modifier regression")

test_that("a click is not deleted when followed by a modifier letter", {
    ## dental click U+01C0 + aspirated U+02B0 both typed "M"
    ## triggering the "MM" canonical-reordering block
    result <- order_ipa("ǀʰ")
    expect(stri_detect_fixed(result, "ǀ"),
           paste("Click character was lost from", shQuote(result)))
    expect(nchar(result) == 2,
           paste("Expected a 2-character result, got", shQuote(result),
                 "(", nchar(result), "characters)"))
})

test_that("a click is not deleted when preceding a modifier letter", {
    ## same "MM" span, reversed input order
    result <- order_ipa("ʰǀ")
    expect(stri_detect_fixed(result, "ǀ"),
           paste("Click character was lost from", shQuote(result)))
    expect(nchar(result) == 2,
           paste("Expected a 2-character result, got", shQuote(result),
                 "(", nchar(result), "characters)"))
})

test_that("a click followed by a diacritic is not swapped the way a modifier would be", {
    ## dental click + devoiced/ring-below diacritic (U+0325) - Typed "MD"
    ## Should not be swapped to "DM" because the click is not a real modifier letter.
    result <- order_ipa("ǀ̥")
    expect(stri_detect_fixed(result, "ǀ"),
           paste("Click character was lost from", shQuote(result)))
    expect(identical(stri_sub(result, 1, 1), "ǀ"),
           paste("Expected click to remain first, got", shQuote(result)))
})

test_that("multiple clicks in sequence with a modifier are all preserved", {
    ## two different clicks + one modifier, all typed "M".
    result <- order_ipa("ǀǁʰ")
    expect(stri_detect_fixed(result, "ǀ"),
           paste("First click character was lost from", shQuote(result)))
    expect(stri_detect_fixed(result, "ǁ"),
           paste("Second click character was lost from", shQuote(result)))
    expect(nchar(result) == 3,
           paste("Expected a 3-character result, got", shQuote(result),
                 "(", nchar(result), "characters)"))
})

test_that("two separate click+diacritic pairs in one string are both skipped", {
    ## typestring "MDMD" both with clicks, so both should be skipped
    result <- order_ipa("ǀ̥ǁ̥")
    expect(stri_detect_fixed(result, "ǀ"),
           paste("First click character was lost from", shQuote(result)))
    expect(stri_detect_fixed(result, "ǁ"),
           paste("Second click character was lost from", shQuote(result)))
    expect(nchar(result) == 4,
           paste("Expected a 4-character result, got", shQuote(result),
                 "(", nchar(result), "characters)"))
})

test_that("a click preceding a real modifier+diacritic doesn't block their swap", {
    ## typestring "MMD" with a click first, then a modifier letter, then a diacritic. 
    ## The modifier+diacritic pair should still be swapped, but the click should remain first.
    result <- order_ipa("ǀʰ̥")
    expect(stri_detect_fixed(result, "ǀ"),
           paste("Click character was lost from", shQuote(result)))
    expect(identical(stri_sub(result, 1, 1), "ǀ"),
           paste("Expected click to remain first, got", shQuote(result)))
    expect(identical(result, "ǀ̥ʰ"),
           paste("Expected the modifier+diacritic pair to still be swapped",
                 "(click skip should not suppress it), got", shQuote(result)))
})
