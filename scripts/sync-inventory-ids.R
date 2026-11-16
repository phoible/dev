#! /usr/bin/env Rscript

## Creates a fresh hex InventoryID (using generate-inventory-ids.R) for any
## new language block in a raw-data source file and bakes it in.
## A block that already has an ID is left alone.

## To add a new raw-data source, add it to one of the lists below which matches its format.
## The source file must have a column named "InventoryID" (case-sensitive) for this script to work.

source("generate-inventory-ids.R")

running_ids <- load_existing_inventory_ids()

## The raw-data sources are of different formats, so we have to handle them differently.
## AA, PH, UZ, GM, SPA are sparse.

id_cols <- c("LanguageCode", "LanguageName", "SpecificDialect")

sparse_sources <- list(
    AA=list(path=file.path("..", "raw-data", "AA", "AA_inventories.tsv"),
            nonblank_col="LanguageCode", id_cols=id_cols),
    PH=list(path=file.path("..", "raw-data", "PH", "phoible_inventories.tsv"),
            nonblank_col="LanguageCode", id_cols=id_cols),
    UZ=list(path=file.path("..", "raw-data", "PH", "UZ_inventories.tsv"),
            nonblank_col="LanguageCode", id_cols=id_cols),
    GM_AFR=list(path=file.path("..", "raw-data", "PH", "gm-afr-inventories.tsv"),
                nonblank_col="LanguageCode", id_cols=id_cols),
    GM_SEA=list(path=file.path("..", "raw-data", "PH", "gm-sea-inventories.tsv"),
                nonblank_col="LanguageCode", id_cols=id_cols),
    SPA=list(path=file.path("..", "raw-data", "SPA", "SPA_Phones.tsv"),
             nonblank_col="spaLangNum", id_cols=c("spaLangNum", "LanguageName"))
)

for (name in names(sparse_sources)) {
    path <- sparse_sources[[name]]$path
    nonblank_col <- sparse_sources[[name]]$nonblank_col
    cols <- sparse_sources[[name]]$id_cols
    cat("processing", name, "\n")
    raw_lines <- readLines(path, encoding="UTF-8", warn=FALSE)
    header <- raw_lines[1]
    lines <- raw_lines[-1]

    d <- read.delim(path, colClasses="character", na.strings=character(0),
                     blank.lines.skip=FALSE, fill=TRUE, check.names=FALSE)
    d[is.na(d)] <- ""

    ## Identify non-blank rows and their corresponding blocks

    nonblank <- nzchar(d[[nonblank_col]])
    nonblank_idx <- which(nonblank)
    pair <- do.call(paste, d[intersect(cols, colnames(d))])[nonblank]
    block_heads <- nonblank_idx[pair != c("", head(pair, -1))]
    block_ends <- c(block_heads[-1] - 1, nrow(d))

    n_new <- 0

    for (i in seq_along(block_heads)) {
        rows <- block_heads[i]:block_ends[i]
        ids <- unique(d$InventoryID[rows][nzchar(d$InventoryID[rows])])
        if (length(ids) > 1) {
            warning("block '", d$LanguageName[rows[1]], "' has multiple IDs: ",
                    paste(ids, collapse=", "), " -- skipping")
            next
        }
        if (length(ids) == 1) next  # known block, nothing to do

        new_id <- generate_new_inventory_ids(1, existing_ids=running_ids)
        running_ids <- c(running_ids, new_id)
        n_new <- n_new + 1
        li <- block_heads[i]

        delim_pos <- regexpr("\t", lines[li], fixed=TRUE)
        lines[li] <- paste0(new_id, substr(lines[li], delim_pos, nchar(lines[li])))
    }

    cat("  new blocks:", n_new, "\n")
    if (n_new > 0) {
        ## binary write to avoid CRLF -> LF conversion on Windows, which would break the file's line endings
        con <- file(path, "wb")
        writeLines(c(header, lines), con, useBytes=TRUE, sep="\r\n")
        close(con)
        cat("  wrote", n_new, "new ID(s) to", path, "\n")
    }
}

## EA and ER are dense: every row of a block repeats LanguageCode and
## LanguageName, so a block boundary is just where that pair changes. A
## new block gets the ID written onto every row; a known block only gets
## any blank rows filled in with its existing ID.
dense_sources <- list(
    EA=file.path("..", "raw-data", "EA", "EA_inventories.tsv"),
    ER=file.path("..", "raw-data", "ER", "ER_inventories.tsv")
)

for (name in names(dense_sources)) {
    path <- dense_sources[[name]]
    cat("processing", name, "\n")
    raw_lines <- readLines(path, encoding="UTF-8", warn=FALSE)
    header <- raw_lines[1]
    lines <- raw_lines[-1]

    d <- read.delim(path, colClasses="character", na.strings=character(0),
                     blank.lines.skip=FALSE, fill=TRUE, check.names=FALSE)
    d[is.na(d)] <- ""

    pair <- paste(d$LanguageCode, d$LanguageName)
    block_heads <- which(pair != c("", head(pair, -1)))
    block_ends <- c(block_heads[-1] - 1, nrow(d))

    n_new <- 0
    n_written <- 0

    for (i in seq_along(block_heads)) {
        rows <- block_heads[i]:block_ends[i]
        ids <- unique(d$InventoryID[rows][nzchar(d$InventoryID[rows])])
        if (length(ids) > 1) {
            warning("block '", d$LanguageName[rows[1]], "' has multiple IDs: ",
                    paste(ids, collapse=", "), " -- skipping")
            next
        }

        if (length(ids) == 1) {
            id <- ids
        } else {
            id <- generate_new_inventory_ids(1, existing_ids=running_ids)
            running_ids <- c(running_ids, id)
            n_new <- n_new + 1
        }

        for (li in rows) {
            if (!nzchar(d$InventoryID[li])) {
                delim_pos <- regexpr("\t", lines[li], fixed=TRUE)
                lines[li] <- paste0(id, substr(lines[li], delim_pos, nchar(lines[li])))
                n_written <- n_written + 1
            }
        }
    }

    cat("  new blocks:", n_new, " | rows filled in:", n_written, "\n")
    if (n_written > 0) {
        con <- file(path, "wb")
        writeLines(c(header, lines), con, useBytes=TRUE, sep="\r\n")
        close(con)
        cat("  wrote", n_written, "row(s) to", path, "\n")
    }
}

## UPSID is one row per inventory
upsid_path <- file.path("..", "raw-data", "UPSID", "UPSID_LanguageCodes.tsv")

cat("processing UPSID\n")
raw_lines <- readLines(upsid_path, encoding="UTF-8", warn=FALSE)
header <- raw_lines[1]
lines <- raw_lines[-1]

d <- read.delim(upsid_path, colClasses="character", na.strings=character(0),
                 blank.lines.skip=FALSE, fill=TRUE, check.names=FALSE)
d[is.na(d)] <- ""

n_new <- 0

for (li in seq_len(nrow(d))) {
    if (nzchar(d$InventoryID[li])) next

    new_id <- generate_new_inventory_ids(1, existing_ids=running_ids)
    running_ids <- c(running_ids, new_id)
    n_new <- n_new + 1

    delim_pos <- regexpr("\t", lines[li], fixed=TRUE)
    lines[li] <- paste0(new_id, substr(lines[li], delim_pos, nchar(lines[li])))
}

cat("  new rows:", n_new, "\n")
if (n_new > 0) {
    con <- file(upsid_path, "wb")
    writeLines(c(header, lines), con, useBytes=TRUE, sep="\r\n")
    close(con)
    cat("  wrote", n_new, "new ID(s) to", upsid_path, "\n")
}

## SAPHON and RA are also one row per inventory
## But they have a different format, so we handle them separately.
one_row_sources <- list(
    SAPHON=list(path=file.path("..", "raw-data", "SAPHON", "saphon20121031.tsv"),
                delim="\t", data_start=3),
    RA=list(path=file.path("..", "raw-data", "RA", "Ramaswami1999.csv"),
            delim=",", data_start=4)
)

for (name in names(one_row_sources)) {
    cfg <- one_row_sources[[name]]
    cat("processing", name, "\n")
    raw_lines <- readLines(cfg$path, encoding="UTF-8", warn=FALSE)

    n_new <- 0

    for (li in cfg$data_start:length(raw_lines)) {
        delim_pos <- regexpr(cfg$delim, raw_lines[li], fixed=TRUE)
        if (nzchar(substr(raw_lines[li], 1, delim_pos - 1))) next

        new_id <- generate_new_inventory_ids(1, existing_ids=running_ids)
        running_ids <- c(running_ids, new_id)
        n_new <- n_new + 1
        raw_lines[li] <- paste0(new_id, substr(raw_lines[li], delim_pos, nchar(raw_lines[li])))
    }

    cat("  new rows:", n_new, "\n")
    if (n_new > 0) {
        con <- file(cfg$path, "wb")
        writeLines(raw_lines, con, useBytes=TRUE, sep="\r\n")
        close(con)
        cat("  wrote", n_new, "new ID(s) to", cfg$path, "\n")
    }
}
