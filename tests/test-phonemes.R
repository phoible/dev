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
    ## show the failures
    phoible %>%
        group_by(InventoryID, Phoneme) %>%
        select(InventoryID, Phoneme) %>%
        filter(n() > 1) ->
        duplicates

    expect(nrow(duplicates) == 0,
           paste(c("INVENTORIES WITH DUPLICATE PHONEMES:",
                   capture.output(print(duplicates))), sep="\n")
           )
    }
)

context("Features")

test_that("feature vectors are consistently assigned", {
    phoible %>%
        group_by(Phoneme) %>%
        select(Phoneme, tone:click) %>%
        distinct() %>%
        filter(n() > 1) ->
        inconsistents

    expect(nrow(inconsistents) == 0,
           paste(c("PHONEMES WITH MULTIPLE DISTINCT FEATURE VECTORS:",
                   capture.output(print(inconsistents))), sep="\n")
           )
    }
)

test_that("within inventories, phonemes have distinct feature vectors", {
    phoible %>%
        group_by(InventoryID) %>%
        count(name="n_phonemes") ->
        n_phonemes
    phoible %>%
        group_by(InventoryID) %>%
        select(InventoryID, tone:click) %>%
        distinct() %>%
        count(name="n_distinct_feature_vectors") ->
        n_vectors
    n_phonemes %>%
        full_join(n_vectors, by="InventoryID") %>%
        filter(n_phonemes != n_distinct_feature_vectors) ->
        mismatches

    expect(nrow(mismatches) == 0,
           paste(c("INVENTORIES WITH INDISTINCT FEATURE VECTORS:",
                   capture.output(print(mismatches))), sep="\n")
           )
    }
)
