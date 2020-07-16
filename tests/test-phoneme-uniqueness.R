#! /usr/bin/env Rscript

## This script tests to make sure that each ISO code in the aggregated data file
## has fully unique phoneme entries (i.e., no duplicate records).

testthat::context("Phoneme uniqueness")

library(dplyr)

## set global options (restored at end)
saf <- getOption("stringsAsFactors")
options(stringsAsFactors=FALSE)

## load PHOIBLE data
phoible_data_file  <- file.path("..", "data", "phoible.csv")
phoible_col_types <- readr::cols(InventoryID="i", Marginal="l", .default="c")
phoible <- readr::read_csv(phoible_data_file, col_types=phoible_col_types)

## count 'em up
phoible %>%
    group_by(InventoryID) %>%
    summarise(all_unique=n() == n_distinct(Phoneme)) ->
    results_table

testthat::expect_true(all(pull(results_table, all_unique)))

## display the failures
results_table %>%
    filter(!all_unique) %>%
    knitr::kable()

## reset options
options(stringsAsFactors=saf)
