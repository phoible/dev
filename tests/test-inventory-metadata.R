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
library(testthat, warn.conflicts=FALSE)

context("Inventory metadata")

## load PHOIBLE data
phoible_data_file  <- file.path("..", "data", "phoible.csv")
phoible_col_types <- readr::cols(InventoryID="i", Marginal="l", .default="c")
phoible <- readr::read_csv(phoible_data_file, col_types=phoible_col_types)


test_that("language codes are valid", {
    ## set global options (restored automatically on exit)
    withr::local_options(list(stringsAsFactors=FALSE))

    default_char_cols <- readr::cols(.default="c")

    ## load official ISO table
    iso_url <- "http://www-01.sil.org/iso639-3/iso-639-3.tab"
    iso_table <- readr::read_tsv(iso_url, col_types=default_char_cols, trim_ws=FALSE)
    iso_valid <- pull(iso_table, Id)

    ## load glottolog
    glotto_url <- "https://github.com/glottolog/glottolog-cldf/blob/master/cldf/languages.csv?raw=true"
    glotto_table <- readr::read_csv(glotto_url, col_types=default_char_cols)
    glotto_valid <- pull(glotto_table, ID)

    ## list exceptions (invalid isocodes that we don't consider errors)
    iso_exceptions <- c(
        "daf",  # split: https://iso639-3.sil.org/request/2012-083
                # checked; not clear which lx. the source describes

        "duj",  # split: https://iso639-3.sil.org/request/2015-053
                # source: https://glottolog.org/resource/reference/id/136243
                # langoid: https://glottolog.org/resource/languoid/id/dhuw1249
                # TODO: source title suggests this is `dwu`, but current
                #     glottocode indicates that the source isn't actually that
                #     specific. Check source to disambiguate if possible.

        "ggr",  # split: https://iso639-3.sil.org/request/2012-138
                # source: https://glottolog.org/resource/reference/id/73578
                # langoid: https://glottolog.org/resource/languoid/id/aghu1254
                # TODO: source title suggests this is `gtu` (one of the 3
                #     possibilities from the split). Check source to
                #     disambiguate if possible.

        "kxu",  # split: http://www.iso639-3.sil.org/code/kxu
                # source: https://glottolog.org/resource/reference/id/579765
                # langoid: https://glottolog.org/resource/languoid/id/kuii1252
                # TODO: see if source disambiguates: does it specifically
                #     mention "Dawik region of Gajapati district"? -> `dwk`.
                #     if it excludes that region, then `uki`.

        "lno",  # split: https://iso639-3.sil.org/request/2021-027
                # source: https://glottolog.org/resource/reference/id/13298
                # source langoid:
                #     https://glottolog.org/resource/languoid/id/lang1324
                # our glottocode:
                #     https://glottolog.org/resource/languoid/id/lang1322
                # TODO: where did we get `lang1322`? If it's correct then we're
                #     probably stuck with `lno` (unless the source
                #     disambiguates); if glottolog is right then the ISO should
                #     be `laj`

        "mwd",  # split: http://www.iso639-3.sil.org/code/mwd
                # source: https://glottolog.org/resource/reference/id/1921
                # langoid: none associated with source nor current ISO code:
                #     https://glottolog.org/glottolog?iso=mwd
                # our current glottocode `mudb1240` is associated with one half
                # of the split (`dmw`). The other half `xrq` (Karranga) has no
                # associated langoid:
                #     https://glottolog.org/glottolog?iso=xrq
                # TODO: Double-check that the source really describes Mudburra
                #     and not Karranga. If it describes both, change nothing
                #     and keep this exception. If it describes only one, change
                #     ISO (and possibly glottocode) accordingly.

        "qgu",  # Extinct. https://glottolog.org/resource/languoid/id/wulg1239

        "wit",  # split; https://iso639-3.sil.org/request/2012-144
                # no glottocode for source:
                #     https://glottolog.org/resource/reference/id/468848
                # TODO check if source disambiguates

        "wya",  # split: https://iso639-3.sil.org/request/2021-041
                # source: https://glottolog.org/resource/reference/id/67646
                # source languoid:
                #     https://glottolog.org/resource/languoid/id/wyan1247
                # our glottocode:
                #     https://glottolog.org/resource/languoid/id/huro1249
                # TODO: where did we get `huro1249`? If it's correct, then we
                #     should update the ISO to `wdt`

        "yiy"   # split: https://iso639-3.sil.org/request/2012-117
                # source: https://glottolog.org/resource/reference/id/151393
                # langoid: https://glottolog.org/resource/languoid/id/yiry1245
                #
                # NOTE: see note box on these two pages
                #     https://glottolog.org/resource/languoid/id/jirj1239
                #     https://glottolog.org/resource/languoid/id/dang1262
                #     stating that according to *our source* the two lects that
                #     this ISO was split into are "easily intelligible". It
                #     might be worth looking at the source to see if it
                #     actually reports data from both communities; if not then
                #     we could justifiably update the ISO to one of the new
                #     splits (`yyr` or `yrm`) and update the glottocode
                #     accordingly. But it's likely that Glottolog already
                #     checked this, so low priority.
    )

    ## pull out the relevant columns and compare to reference list
    iso_phoible <- pull(phoible, ISO6393)
    iso_invalid <- setdiff(iso_phoible, iso_valid)
    glotto_phoible <- pull(phoible, Glottocode)
    glotto_invalid <- setdiff(glotto_phoible, glotto_valid)

    ## test
    iso_invalids_not_caught <- setdiff(iso_invalid, iso_exceptions)
    iso_exceptions_not_needed <- setdiff(iso_exceptions, iso_invalid)
    expect(length(iso_invalids_not_caught) == 0,
           paste("INVALID ISO CODES NOT HANDLED:",
                 paste(iso_invalids_not_caught, collapse=" "),
                 sep="\n")
           )
    expect(length(iso_exceptions_not_needed) == 0,
           paste("UNNECESSARY ISO CODE EXCEPTIONS:",
                 paste(iso_exceptions_not_needed, collapse=" "),
                 sep="\n")
    )

    expect(length(glotto_invalid) == 0,
           paste("INVALID GLOTTOCODES:", paste(glotto_invalid, collapse=" "),
                 sep="\n")
           )
    }
)


test_that("metadata is unique within each inventory", {
    ## count inventories
    phoible %>%
        select(InventoryID) %>%
        n_distinct() ->
        n_inventories

    ## extract metadata columns. We need the intermediate dataframe for the
    ## the error message, which is why we don't just count with n_distinct().
    phoible %>%
        group_by(InventoryID) %>%
        select(Glottocode, ISO6393, LanguageName, SpecificDialect, Source) %>%
        unique() ->
        distinct_metadata_rows

    distinct_metadata_rows %>%
        filter(n() > 1) %>%
        pull(InventoryID) %>%
        paste(collapse=" ") ->
        errors

    ## test
    expect(nrow(distinct_metadata_rows) == n_inventories,
           paste("INVENTORIES WITH NON-UNIQUE METADATA:", errors, sep="\n")
           )
    }
)
