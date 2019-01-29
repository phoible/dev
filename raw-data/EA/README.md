# EA

The `EA` source data comes from The database of Eurasian phonological inventories (beta version), online: [http://eurasianphonology.info/](http://eurasianphonology.info/).

This source is cited as:

> Nikolaev, Dmitry; Andrey Nikulin; and Anton Kukhto. 2015. The database of Eurasian phonological inventories. Available online at [http://eurasianphonology.info/](http://eurasianphonology.info/).

The raw data and source code are available at: [https://github.com/macleginn/eurasian-phonologies](https://github.com/macleginn/eurasian-phonologies).

`EA` contains a fresh take on phonological inventories from Eurasia. The data were extracted from grammatical descriptions of individual language varieties. It contains only phonemes, with no information on allophones or linguistic tone.

The data are available in phoible long format in [EA_inventories.tsv](EA_inventories.tsv). We have converted IPA symbols in the raw data in line with the [phoible conventions](http://phoible.github.io/conventions/) and [Unicode IPA](http://langsci-press.org/catalog/book/176) as specified in the [EA_IPA_correspondences.tsv](EA_IPA_correspondences.tsv) correspondences file. We have also collected for each citation a BibTeX reference, available in the [phoible-references.bib](../../data/phoible-references.bib) file.

Note that ISO 639-3 codes in may be out of date with the current ISO 639-3 standard because change requests. For more info, see: [https://iso639-3.sil.org/](https://iso639-3.sil.org/).

For up-to-date language codes for each inventory, we maintain a phoible index here:
[InventoryID-LanguageCodes.tsv](../../mappings/InventoryID-LanguageCodes.tsv).
