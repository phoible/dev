#! /usr/bin/env Rscript

## This script checks that the phoible language codes are valid ISO 639-3 codes.
## The output goes into the root folder. Note that bad codes can be easily
## looked up using the following URL patterns (replace XXX with desired code):
## Main ethnologue entry:
## http://www.ethnologue.com/language/XXX
## ISO 639-3 change history for code:
## http://www-01.sil.org/iso639-3/documentation.asp?id=XXX
## Full list of retired codes:
## http://www-01.sil.org/iso639-3/codes_retired.asp

## set global options (restored at end)
saf <- getOption("stringsAsFactors")
options(stringsAsFactors=FALSE)

def_char_cols <- readr::cols(.default="c")

## load official ISO table
iso_url <- "http://www-01.sil.org/iso639-3/iso-639-3.tab"
iso_table <- readr::read_tsv(iso_url, col_types=def_char_cols)
iso_valid <- dplyr::pull(iso_table, Id)

## load glottolog
glotto_url <- "https://github.com/glottolog/glottolog-cldf/blob/master/cldf/languages.csv?raw=true"
glotto_table <- readr::read_csv(glotto_url, col_types=def_char_cols)
glotto_valid <- dplyr::pull(glotto_table, ID)

## load PHOIBLE data
phoible_data_file  <- file.path("..", "data", "phoible.csv")
phoible_col_types <- readr::cols(InventoryID="i", Marginal="l", .default="c")
phoible <- readr::read_csv(phoible_data_file, col_types=phoible_col_types)

## pull out the relevant columns and compare to reference lists
iso_phoible <- dplyr::pull(phoible, ISO6393)
iso_invalid <- dplyr::setdiff(iso_phoible, iso_valid)

glotto_phoible <- dplyr::pull(phoible, Glottocode)
glotto_invalid <- dplyr::setdiff(glotto_phoible, glotto_valid)

## only one invalid value is expected: NA
testthat::expect_length(iso_invalid, 1)
testthat::expect_length(glotto_invalid, 1)

testthat::expect_length(is.na(iso_invalid), 1)
testthat::expect_length(is.na(glotto_invalid), 1)

## reset options
options(stringsAsFactors=saf)
