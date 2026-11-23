#! /usr/bin/env Rscript

## Compresses data/phoible.csv into data/phoible.csv.zip.
## Run after add-features.R has (re)written phoible.csv.

csv_path <- file.path("..", "data", "phoible.csv")
zip_path <- file.path("..", "data", "phoible.csv.zip")

zip::zip(zip_path, basename(csv_path), root=dirname(csv_path))
cat("wrote", zip_path, "\n")
