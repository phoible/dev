#! /usr/bin/env Rscript

## This script tests to make sure that each ISO code in the aggregated data file
## has fully unique phoneme entries (i.e., no duplicate records).

library(dplyr, warn.conflicts=FALSE)
library(testthat)

context("Phonemes")

## load PHOIBLE data
phoible_data_file  <- file.path("..", "data", "phoible.csv")
phoible_col_types <- readr::cols(InventoryID="i", Marginal="l", .default="c")
phoible <- readr::read_csv(phoible_data_file, col_types=phoible_col_types)

test_that("inventories don't have duplicate phonemes", {
    ## set global options (restored automatically on exit)
    withr::local_options(list(stringsAsFactors=FALSE))

    ## count 'em up
    phoible %>%
        group_by(InventoryID) %>%
        summarise(all_unique=n() == n_distinct(Phoneme), .groups="drop") ->
        results_table
    ## show the failures
    if(any(!results_table$all_unique)) {
        withr::deferred_run({
            cat("\n\n====================================")
            cat("\nINVENTORIES WITH DUPLICATE PHONEMES:\n")
            results_table %>%
                filter(!all_unique) %>%
                pull(InventoryID) %>%
                print()
            cat("\n====================================\n")
        })
    }

    expect_true(all(pull(results_table, all_unique)))
    })
