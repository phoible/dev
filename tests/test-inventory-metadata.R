#! /usr/bin/env Rscript

## This script checks that the phoible language codes are valid ISO 639-3
## codes. Bad codes can be looked up using the following URL patterns (replace
## XXX with desired code):
## Main ethnologue entry:
## http://www.ethnologue.com/language/XXX
## ISO 639-3 change history for code:
## http://www-01.sil.org/iso639-3/documentation.asp?id=XXX
## Full list of retired codes:
## http://www-01.sil.org/iso639-3/codes_retired.asp

library(dplyr, warn.conflicts=FALSE)
library(testthat)

context("Inventory metadata")

## load PHOIBLE data
phoible_data_file  <- file.path("..", "data", "phoible.csv")
phoible_col_types <- readr::cols(InventoryID="i", Marginal="l", .default="c")
phoible <- readr::read_csv(phoible_data_file, col_types=phoible_col_types)


test_that("all ISO codes are valid", {
    ## set global options (restored automatically on exit)
    withr::local_options(list(stringsAsFactors=FALSE))

    default_char_cols <- readr::cols(.default="c")

    ## load official ISO table
    iso_url <- "http://www-01.sil.org/iso639-3/iso-639-3.tab"
    iso_table <- readr::read_tsv(iso_url, col_types=default_char_cols)
    iso_valid <- pull(iso_table, Id)

    ## pull out the relevant columns and compare to reference list
    iso_phoible <- pull(phoible, ISO6393)
    iso_invalid <- setdiff(iso_phoible, iso_valid)
    if(length(iso_invalid)) {
        withr::deferred_run({
            cat("\n\n==================")
            cat("\nINVALID ISO CODES:\n")
            cat(iso_invalid)
            cat("\n==================\n")
        })
    }
    expect_length(iso_invalid, 0)
})


test_that("all glottocodes are valid", {
    ## set global options (restored automatically on exit)
    withr::local_options(list(stringsAsFactors=FALSE))

    default_char_cols <- readr::cols(.default="c")

    ## load glottolog
    glotto_url <- "https://github.com/glottolog/glottolog-cldf/blob/master/cldf/languages.csv?raw=true"
    glotto_table <- readr::read_csv(glotto_url, col_types=default_char_cols)
    glotto_valid <- pull(glotto_table, ID)

    ## pull out the relevant columns and compare to reference list
    glotto_phoible <- pull(phoible, Glottocode)
    glotto_invalid <- setdiff(glotto_phoible, glotto_valid)
    if(length(glotto_invalid)) {
        withr::deferred_run({
            cat("\n\n====================")
            cat("\nINVALID GLOTTOCODES:\n")
            cat(glotto_invalid)
            cat("\n====================\n")
        })
    }
    expect_length(glotto_invalid, 0)
})


test_that("metadata is unique within each inventory", {
    ## extract metadata columns and count unique rows
    phoible %>%
        group_by(InventoryID) %>%
        select(Glottocode, ISO6393, LanguageName, SpecificDialect, Source) %>%
        n_distinct() ->
        n_distinct_metadata_rows
    ## count inventories
    phoible %>%
        select(InventoryID) %>%
        n_distinct() ->
        n_inventories
    ## test
    expect_equal(n_distinct_metadata_rows, n_inventories)
    })
