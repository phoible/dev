# UPSID

The `UPSID` folder contains data from the UCLA Phonological Segment 
Inventory Database:

> Maddieson, I., & Precoda, K. (1990). Updating UPSID. _UCLA Working Papers in Phonetics_, 74, 104–111.

The contents and extraction pipeline for these data are described in (chapter 4):

> Moran, Steven. (2012). Phonetics Information Base and Lexicon. PhD thesis, University of Washington. Online: [https://digital.lib.washington.edu/researchworks/handle/1773/22452](https://digital.lib.washington.edu/researchworks/handle/1773/22452).

The data are available in several files in this directory from the original ASCII dump. These inventories contain only phonemes, with no information on allophones or linguistic tone.

We have converted IPA symbols in the raw data in line with the [phoible conventions](http://phoible.github.io/conventions/) and [Unicode IPA](http://langsci-press.org/catalog/book/176) as described in the [UPSID_IPA_correspondences.tsv](UPSID_IPA_correspondences.tsv) file.

We have also collected for each citation a BibTeX reference, available in the [phoible-references.bib](../../data/phoible-references.bib) file. See the [InventoryID-Bibtex.csv](../../mappings/InventoryID-Bibtex.csv) mapping file for details.

Note that the ISO 639-3 codes in the UPSID source may be out of date with the current ISO 639-3 standard. For more info, see: [https://iso639-3.sil.org/](https://iso639-3.sil.org/).

For up-to-date language codes for each inventory, we maintain a phoible index here:
[InventoryID-LanguageCodes.csv](../../mappings/InventoryID-LanguageCodes.csv).

Note that [Henning Reetz](http://menzerath.phonetik.uni-frankfurt.de/staff/reetz/reetz.html) has put online a [simple user interface to the UPSID data](http://web.phonetik.uni-frankfurt.de/upsid.html), which can be used for browsing and quick queries.

