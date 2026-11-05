#! /usr/bin/env Rscript

## InventoryID generation. Format: "p" + 4 lowercase hex digits (e.g. "p1a2b").
## The "p" prefix guarantees an ID can never be parsed as an integer
## The 4 digit hex suffix gives 65536 possible IDs.
## The IDs are generated deterministically from a counter, so that the same inputs always produce the same outputs.
## The IDs are non-sequential, and deleted IDs are not reused (stored in mappings/InventoryID-Retired.csv).

is_valid_inventory_id <- function(x) {
    grepl(INVENTORY_ID_PATTERN, x)
}

## Reads InventoryID out of a CSV file's column of that name, if the file exists.
read_inventory_ids <- function(path) {
    if (!file.exists(path)) return(character(0))
    data <- read.csv(path, colClasses="character")
    if (!"InventoryID" %in% colnames(data)) return(character(0))
    data$InventoryID
}

## Every InventoryID currently in use anywhere, so new IDs can be checked
## against the full set, not just one file.
load_existing_inventory_ids <- function() {
    paths <- c(
        file.path("..", "data", "phoible-nofeats.csv"),
        file.path("..", "mappings", "InventoryID-LanguageCodes.csv"),
        file.path("..", "mappings", "InventoryID-Bibtex.csv"),
        file.path("..", "mappings", "InventoryID-Filenames.csv"),
        file.path("..", "mappings", "InventoryID-Legacy.csv"),
        file.path("..", "mappings", "InventoryID-Retired.csv")
    )
    unique(unlist(lapply(paths, read_inventory_ids)))
}

## Multiplier = floor(65536 / 1.61803398875) for a bijective mapping of 0 - 65535 to 0 - 65535 in hex that is not sequential.
INVENTORY_ID_PERMUTE_MULTIPLIER <- 40503L

## Maps a counter value to a hex string in a non-sequential way, using a
## fixed multiplier to permute the counter values.
permute_counter_to_hex <- function(counter) {
    scrambled <- (as.double(counter) * INVENTORY_ID_PERMUTE_MULTIPLIER) %% 65536
    sprintf("%04x", as.integer(scrambled))
}

## Generates n new InventoryIDs that are not in the existing_ids vector.
generate_new_inventory_ids <- function(n, existing_ids=load_existing_inventory_ids()) {
    new_ids <- character(0)
    counter <- 0L
    while (length(new_ids) < n && counter < 65536L) {
        candidate <- paste0("p", permute_counter_to_hex(counter))
        counter <- counter + 1L
        if (!(candidate %in% existing_ids) && !(candidate %in% new_ids)) {
            new_ids <- c(new_ids, candidate)
        }
    }
    if (length(new_ids) < n) {
        stop("Could not generate ", n, " unique InventoryIDs: keyspace exhausted.")
    }
    new_ids
}
