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
