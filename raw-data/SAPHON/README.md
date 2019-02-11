# SAPHON

The `SAPHON` folder contains the SAPHON data (v. 20121031) and glyph-to-PHOIBLE Unicode IPA mappings from South American Phonological Inventory Database

This source should be cited as:

> Michael, Lev, Tammy Stark, and Will Chang. (2012) South American Phonological Inventory Database. University of California. Online: [http://linguistics.berkeley.edu/~saphon/en/](http://linguistics.berkeley.edu/~saphon/en/).

The data are available in phoible wide format in the [saphon20121031.tsv](saphon20121031.tsv) file and contain only phonemes, with no information on allophones, and a simple boolean indicator for the presence/absence of linguistic tone. **This boolean "tone/no tone" information is not incorporated into PHOIBLE.**  The raw data also contains information about where the languages are spoken.

We have converted IPA symbols in the raw data in line with the [phoible conventions](http://phoible.github.io/conventions/) and [Unicode IPA](http://langsci-press.org/catalog/book/176) as specified in the [saphon_ipa_correspondences.tsv](saphon_ipa_correspondences.tsv) correspondences file. 

Note that the ISO 639-3 codes in the SAPHON source may be out of date with the current ISO 639-3 standard. For more info, see: [https://iso639-3.sil.org/](https://iso639-3.sil.org/).

For up-to-date language codes for each inventory, we maintain a phoible index here:
[InventoryID-LanguageCodes.tsv](../../mappings/InventoryID-LanguageCodes.tsv).
