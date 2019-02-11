# SPA

The `SPA` folder contains data from the Stanford Phonology Archive:

> Crothers, J. H., Lorentz, J. P., Sherman, D. A., & Vihman, M. M. (1979). Handbook of phonological data from a sample of the world’s languages: A report of the Stanford Phonology Archive. Palo Alto, CA: Department of Linguistics, Stanford University.

The contents and extraction pipeline for these data are described in (chapter 4):

> Moran, Steven. (2012). Phonetics Information Base and Lexicon. PhD thesis, University of Washington. Online: [https://digital.lib.washington.edu/researchworks/handle/1773/22452](https://digital.lib.washington.edu/researchworks/handle/1773/22452).

The data are available in phoible long format in [SPA_Phones.tsv](SPA_Phones.tsv). The inventories contain the language name given in the source and their phonemes, allophones and tones. The inventories file also contains additional information in the form of footnotes, which are explained in detail in the original source (Crothers et al., 1979)

We have converted IPA symbols in the raw data in line with the [phoible conventions](http://phoible.github.io/conventions/) and [Unicode IPA](http://langsci-press.org/catalog/book/176) as described in the [SPA_IPA_correspondences.tsv](SPA_IPA_correspondences.tsv) file.

Note that the ISO 639-3 codes in the SPA source may be out of date with the current ISO 639-3 standard. For more info, see: [https://iso639-3.sil.org/](https://iso639-3.sil.org/).

For up-to-date language codes for each inventory, we maintain a phoible index here:
[InventoryID-LanguageCodes.tsv](../../mappings/InventoryID-LanguageCodes.tsv).
