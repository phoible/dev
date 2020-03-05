[![DOI](https://zenodo.org/badge/19120525.svg)](https://zenodo.org/badge/latestdoi/19120525)

# PHOIBLE

PHOIBLE is a database of phonological inventories and distinctive features,
encompassing more than 3000 phonological inventories (doculects), representing
more than 2100 ISO 639-3 language codes. PHOIBLE data is published in
browsable form online at [PHOIBLE Online](http://phoible.org), which
corresponds with the most recent
[release](https://github.com/phoible/phoible/releases) of this repository.

Data in machine-readable form is available in this repository. It is not
guaranteed to exactly match what is published at [PHOIBLE
Online](http://phoible.org), due to the occasional discovery and correction of
errors, and the addition of new languages to the database. For this reason,
**it is recommended that you make use of the most recent release** in
your own analyses, rather than working from the tip of the master branch.

Documentation for PHOIBLE is hosted at at http://phoible.github.io/, including
notational conventions, departures from official IPA usage, citation
information, etc.

## How to use this repository

Most people will not need to look beyond the [data
folder](https://github.com/phoible/dev/tree/master/data) of this repository,
which contains a phoneme-level data file (one row per languoid-phoneme pair)
and a BibTeX file
of all the data sources. The rest of the repo contains scripts used in the
development and testing of PHOIBLE, such as code to aggregate the raw data
files from the various donor databases. These are probably not of general
interest or utility. The `raw-data` directory contains the *raw* data from the
various donor data sources, as well as the feature mapping tables. This is
also probably not what you want, so if in doubt, stick to the
[data directory](https://github.com/phoible/dev/tree/master/data).

## Citing PHOIBLE

If you are citing the database as a whole, or making use of the phonological
distinctive feature systems in PHOIBLE, please cite as follows:

> Moran, Steven & McCloy, Daniel (eds.) 2019. PHOIBLE 2.0. Jena: Max Planck
  Institute for the Science of Human History. (Available online at
  http://phoible.org). DOI: 10.5281/zenodo.2626687

If you are citing phoneme inventory data for a particular language or
languages, please use the name of the language as the title, and include the
original data source as an element within PHOIBLE. For example:

> UCLA Phonological Segment Inventory Database. 2019. Lelemi sound inventory
  (UPSID). In: Moran, Steven & McCloy, Daniel (eds.) PHOIBLE 2.0. Jena: Max
  Planck Institute for the Science of Human History. (Available online at
  http://phoible.org/inventories/view/441)

If you are using the raw data from this repository but are *not* using a
[labeled release](https://github.com/phoible/phoible/releases), we recommend
citing using the last commit hash at the time of your most recent
cloning/forking of the repository, so that others can reproduce your work
starting from the same snapshot of the repository that you are using. For 
example:

> Moran, Steven & McCloy, Daniel (eds.) 2019. PHOIBLE.
  https://github.com/phoible/phoible/commit/444a46c9a94641d6c99f5c8bbe85b8ae1c6ce65f

## History

PHOIBLE was originally developed as an SQL database and RDF knowledgebase for
Moran’s dissertation, which explains many of the technical details and
developmental challenges:

> Moran, Steven. 2012. Phonetics Information Base and Lexicon. PhD thesis,
  University of Washington. http://hdl.handle.net/1773/22452

Here is a brief list of some publications that we have used PHOIBLE data for:

- Blasi, Damián, Steven Moran, Scott Moisik, Paul Widmer, Dan Dediu,
  & Balthasar Bickel. 2019. Human sound systems are shaped by post-Neolithic
  changes in bite configuration. *Science* 363(6432), eaav3218.
  [doi:10.1126/science.aav3218](http://dx.doi.org/10.1126/science.aav3218)

- Cysouw, Michael, Dan Dediu and Steven Moran. 2012. Still No Evidence for an
  Ancient Language Expansion from Africa. *Science*, 335(6069):657. [doi:10.1126/science.1208841](https://doi.org/10.1126/science.1208841)

- Moran, Steven. 2012. Using Linked Data to Create a Typological Knowledge
  Base. In Christian Chiarcos, Sebastian Nordhoff and Sebastian Hellmann
  (eds), *Linked Data in Linguistics: Representing and Connecting Language
  Data and Language Metadata*. Springer, Heidelberg.

- Moran, Steven, Daniel McCloy, and Richard Wright. 2012. Revisiting
  Population Size vs. Phoneme Inventory Size. *Language* 88(4): 877-893. [doi:10.1353/lan.2012.0087](https://doi.org/10.1353/lan.2012.0087)

- Moran, Steven and Damián Blasi. 2014. Cross-linguistic Comparison of
  Complexity Measures in Phonological Systems. In Frederick J. Newmeyer and
  Laurel Preston (eds), *Measuring Grammatical Complexity*. Oxford UP,
  Oxford.

A more complete list of research papers using PHOIBLE can be found on [Google Scholar](https://scholar.google.com/scholar?oi=bibs&hl=en&cites=576981116309388928&as_sdt=5).
