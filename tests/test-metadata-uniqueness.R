#! /usr/bin/env Rscript

## This script checks that each metadata field associated with each inventory
## has only one value within each inventory.

testthat::context("Metadata consistency")

library(dplyr)

## load PHOIBLE data
phoible_data_file  <- file.path("..", "data", "phoible.csv")
phoible_col_types <- readr::cols(InventoryID="i", Marginal="l", .default="c")
phoible <- readr::read_csv(phoible_data_file, col_types=phoible_col_types)

## should be one distinct row
phoible %>%
    group_by(InventoryID) %>%
    select(Glottocode, ISO6393, LanguageName, SpecificDialect, Source) %>%
    n_distinct() ->
    n_distinct_metadata

testthat::expect_equal(n_distinct_metadata, n_distinct(phoible$InventoryID))
