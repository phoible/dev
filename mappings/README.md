# Mappings

This folder contains files that map InventoryID to metadata not stored directly in [phoible.csv](../data/phoible.csv).

- [InventoryID-LanguageCodes.csv](InventoryID-LanguageCodes.csv): InventoryID, Glottocode, ISO6393, LanguageName, SpecificDialect. One row per InventoryID.
- [InventoryID-Bibtex.csv](InventoryID-Bibtex.csv): InventoryID, BibtexKey. An InventoryID can have several citations, so this is one to many.
- [InventoryID-Filenames.csv](InventoryID-Filenames.csv): InventoryID, Filename, URI. The source document each inventory was drawn from. A few InventoryIDs cite more than one source document and so have more than one row.
- [phoible-references.bib](phoible-references.bib): BibTeX entries for the keys used in InventoryID-Bibtex.csv.

## License

The data in this directory is licensed under the [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/). See [../LICENSE-DATA](../LICENSE-DATA) for the full license text, and the root [README.md](../README.md#citing-phoible) for how to cite PHOIBLE.

## Known issues

- InventoryID 201 (Arrernte, `aer`) has `NO SOURCE GIVEN` as its literal BibtexKey, a placeholder rather than a real citation. See [raw-data/UPSID/README.md](../raw-data/UPSID/README.md) for details.
- These InventoryIDs cite more than one source document and have more than one row in InventoryID-Filenames.csv: 1050, 1147, 1273, 1300, 1403, 1446, 1496, 1531, 1554, 1578, 1735.
