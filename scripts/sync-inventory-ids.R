#! /usr/bin/env Rscript

## Creates a fresh hex InventoryID (using generate-inventory-ids.R) for any
## new language block in a raw-data source file and bakes it in.
## A block that already has an ID is left alone.

## TEMP: Script is currently only for AA_inventories.tsv

source("generate-inventory-ids.R")

aa_path <- file.path("..", "raw-data", "AA", "AA_inventories.tsv")

cat("processing AA\n")
raw_lines <- readLines(aa_path, encoding="UTF-8", warn=FALSE)
header <- raw_lines[1]
lines <- raw_lines[-1]

aa <- read.delim(aa_path, colClasses="character", na.strings=character(0),
                 blank.lines.skip=FALSE, fill=TRUE, check.names=FALSE)
aa[is.na(aa)] <- ""

## AA has blank lines between languages, and within a block LanguageCode
## and LanguageName are always both present or both blank together.
nonblank <- nzchar(aa$LanguageCode)
nonblank_idx <- which(nonblank)
pair <- paste(aa$LanguageCode, aa$LanguageName)[nonblank]
block_heads <- nonblank_idx[pair != c("", head(pair, -1))]
block_ends <- c(block_heads[-1] - 1, nrow(aa))

running_ids <- load_existing_inventory_ids()
n_new <- 0

for (i in seq_along(block_heads)) {
    rows <- block_heads[i]:block_ends[i]
    ids <- unique(aa$InventoryID[rows][nzchar(aa$InventoryID[rows])])
    if (length(ids) > 1) {
        warning("block '", aa$LanguageName[rows[1]], "' has multiple IDs: ",
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
    con <- file(aa_path, "wb")
    writeLines(c(header, lines), con, useBytes=TRUE, sep="\r\n")
    close(con)
    cat("  wrote", n_new, "new ID(s) to", aa_path, "\n")
}
