# phoible data tests

library(dplyr)
library(testthat)

load('../data/phoible.RData')

# Metadata categories should be unique within inventories
ids <- phoible %>% select(InventoryID) %>% distinct()
metadata <- phoible %>% select(InventoryID, Glottocode, ISO6393, LanguageName, SpecificDialect, Source) %>% distinct()
expect_equal(nrow(ids), nrow(metadata))