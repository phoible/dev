#! /usr/bin/env Rscript

## Tests for cross-inventory integrity. Checks that:
##   1. No source accidentally split one language (Glottocode, LanguageName, SpecificDialect, BibtexKey) across two InventoryIDs
##   2. Every BibtexKey in the mapping file resolves to a real entry in
##      phoible-references.bib.
##   3. Every InventoryID in the mapping files exists in the aggregated
##      data, and vice versa.

library(dplyr, warn.conflicts=FALSE)
library(testthat, warn.conflicts=FALSE)

## load PHOIBLE data
phoible_data_file <- file.path("..", "data", "phoible.csv")
phoible_col_types <- readr::cols(InventoryID="c", Marginal="l", .default="c")
phoible <- readr::read_csv(phoible_data_file, col_types=phoible_col_types)

bibtex_mapping_file <- file.path("..", "mappings", "InventoryID-Bibtex.csv")
bibtex_mapping <- readr::read_csv(bibtex_mapping_file, col_types=readr::cols(.default="c"))

# Join the phoible data with the mapping file to get a combined dataset for testing
phoible_with_bibtex <- phoible %>%
    left_join(bibtex_mapping, by="InventoryID", relationship="many-to-many")

references_bib_file <- file.path("..", "mappings", "phoible-references.bib")

context("Cross-inventory integrity")

test_that("no source accidentally split one language across two InventoryIDs", {
    ## Checks whether the same (Source, Glottocode, LanguageName, SpecificDialect, BibtexKey) tuple
    ## appears in more than one InventoryID.
    phoible_with_bibtex %>%
        filter(!is.na(Glottocode)) %>%
        distinct(InventoryID, Source, Glottocode, LanguageName, SpecificDialect, BibtexKey) %>%
        group_by(Source, Glottocode, LanguageName, SpecificDialect, BibtexKey) %>%
        filter(n_distinct(InventoryID) > 1) %>%
        ungroup() ->
        split_duplicates

    expect(nrow(split_duplicates) == 0,
           paste(c("SAME (Source, Glottocode, LanguageName, SpecificDialect, BibtexKey) APPEARS IN MULTIPLE INVENTORYIDS:",
                   capture.output(print(split_duplicates, n=Inf))),
                 sep="\n")
           )
    }
)

test_that("every BibtexKey resolves to a real bib entry", {
    ## Check that every BibtexKey in the mapping file resolves to a real entry in phoible-references.bib.  

    ## regex to match BibTeX keys in the .bib file  
    citekey_re <- "(?m)^@\\w+\\s*\\{\\s*([^,]+?)\\s*,"

    bib_keys <- stringr::str_match_all(
        readr::read_file(references_bib_file), citekey_re
    )[[1]][, 2]

    mapping_keys <- bibtex_mapping %>% pull(BibtexKey) %>% unique()
    missing_keys <- setdiff(mapping_keys, bib_keys)

    # Exclude special case of InventoryID 201 (Arrente), which has "NO SOURCE GIVEN" as its BibtexKey
    missing_keys <- setdiff(missing_keys, bibtex_mapping %>% filter(InventoryID == 201) %>% pull(BibtexKey))

    ## report which InventoryID(s) cite each missing key
    offending_rows <- bibtex_mapping %>%
        filter(BibtexKey %in% missing_keys) %>%
        arrange(BibtexKey, as.integer(InventoryID))
    offending_summary <- offending_rows %>%
        group_by(BibtexKey) %>%
        summarise(InventoryIDs = paste(InventoryID, collapse=", "), .groups="drop")

    expect(length(missing_keys) == 0,
           paste(c(paste("BIBTEXKEYS WITH NO MATCHING ENTRY IN",
                         basename(references_bib_file)),
                   paste0(offending_summary$BibtexKey, " (InventoryID ",
                          offending_summary$InventoryIDs, ")")),
                 sep="\n")
           )
    }
)

test_that("mapping-file and aggregated-data InventoryIDs match in both directions", {
    phoible_ids <- phoible %>% pull(InventoryID) %>% unique()
    mapping_ids <- bibtex_mapping %>% pull(InventoryID) %>% as.integer() %>% unique()

    only_in_mapping <- setdiff(mapping_ids, phoible_ids)
    only_in_phoible <- setdiff(phoible_ids, mapping_ids)

    expect(length(only_in_mapping) == 0,
           paste(c("INVENTORYIDS IN InventoryID-Bibtex.csv BUT MISSING FROM",
                   "AGGREGATED DATA:", paste(only_in_mapping, collapse=" ")),
                 sep="\n")
           )
    expect(length(only_in_phoible) == 0,
           paste(c("INVENTORYIDS IN AGGREGATED DATA BUT MISSING FROM",
                   "InventoryID-Bibtex.csv:", paste(only_in_phoible, collapse=" ")),
                 sep="\n")
           )
    }
)
