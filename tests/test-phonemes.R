#! /usr/bin/env Rscript

## This script tests to make sure that each ISO code in the aggregated data file
## has fully unique phoneme entries (i.e., no duplicate records).

library(dplyr, warn.conflicts=FALSE)
library(testthat)

## load PHOIBLE data
phoible_data_file  <- file.path("..", "data", "phoible.csv")
phoible_col_types <- readr::cols(InventoryID="i", Marginal="l", .default="c")
phoible <- readr::read_csv(phoible_data_file, col_types=phoible_col_types)

context("Phonemes")

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
        distinct(pick(tone:click)) %>%
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
        filter(tone == "0") %>%
        group_by(InventoryID) ->
        grouped

    # count distinct phonemes per inventory
    grouped %>% count(name="n_phonemes") -> n_phonemes

    # count distinct feature vectors per inventory
    grouped %>%
        distinct(pick(tone:click)) %>%
        count(name="n_distinct_feature_vectors") ->
        n_vectors

    # tally mismatches between n_phonemes and n_feature_vectors
    n_phonemes %>%
        full_join(n_vectors, by="InventoryID") %>%
        filter(n_phonemes != n_distinct_feature_vectors) %>%
        mutate(difference=n_phonemes - n_distinct_feature_vectors) ->
        mismatches

    if (nrow(mismatches)) {
        # get DF with one row per collapsed phoneme contrast per inventory
        "Phoneme" -> prefix
        phoible %>%
            filter(tone == "0", InventoryID %in% mismatches$InventoryID) %>%
            group_by(pick(InventoryID, tone:click)) %>%
            filter(n() > 1) %>%
            mutate(id=row_number(),
                   PhonemeList=list(stringr::str_sort(Phoneme))) %>%
            tidyr::pivot_wider(
                id_cols=c(InventoryID, tone:click, PhonemeList),
                names_from=id, names_prefix=prefix, values_from=Phoneme) %>%
            ungroup() %>%
            select(InventoryID, starts_with(prefix), tone:click) ->
            indistinct_phoneme_sets

        # collapse across inventories to see just which contrasts need fixing
        indistinct_phoneme_sets %>%
            group_by(PhonemeList) %>%
            mutate(InventoryIDs=list(InventoryID)) %>%
            select(-InventoryID) %>%
            distinct() %>%
            ungroup() %>%
            arrange(pick(starts_with(prefix))) %>%
            relocate(InventoryIDs) ->
            unique_indistinct_phoneme_sets

        # ignore "known bugs"
        list(
            # voiced ejectives (check sources)
            c("dʼ", "tʼ"),
            c("dʼ", "tʰʼ"),
            c("dzʼ", "tsʼ"),
            c("d̠ʒʼ", "t̠ʃʼ"),
            c("dʼkxʼ", "tʼkxʼ"),
            c("d̪ʼkxʼ", "t̪ʼkxʼ"),
            c("ɡʼ", "kʼ"),
            # diacritic redundant / collapses contrast (check source)
            c("kǀ", "kǀ̪", "kǃ̪"),
            # doubled vowel (check source)
            c("o", "oo"),
            c("i", "ii"),
            # glottalized vs creaky (check sources)
            c("aˀ", "a̰"),
            c("eˀ", "ḛ "),
            c("iˀ", "ḭ"),
            c("oˀ", "o̰"),
            c("uˀ", "ṵ"),
            # palatalized palatal (check sources)
            c("c", "cʲ"),
            c("ɟ", "ɟʲ"),
            # voiceles aspiration on voiced glyph (fix)
            c("d̠ʒʰ", "t̠ʃʰ")
        ) -> known_bugs

        # make print-friendly output
        purrr::partial(stringr::str_c, collapse=" ") -> stringify
        unique_indistinct_phoneme_sets %>%
            filter(!PhonemeList %in% known_bugs) %>%
            mutate(InventoryIDs=purrr::map_chr(InventoryIDs, stringify),
                   Phonemes=purrr::map_chr(PhonemeList, stringify)) %>%
            select(Phonemes, InventoryIDs) ->
            unhandled_indistinct_phoneme_sets
    }

    expect(nrow(mismatches) == 0,
           paste(c("PHONEMES WITH INDISTINCT FEATURE VECTORS:",
                   capture.output(print(unhandled_indistinct_phoneme_sets,
                                        n=Inf))),
                 sep="\n")
           )
    }
)
