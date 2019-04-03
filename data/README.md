# PHOIBLE data

PHOIBLE is a database of phonological inventories and distinctive features,
encompassing more than 3000 inventories (and growing). Here are links to
various parts of the project:

- The web-browsable published form of PHOIBLE is called “PHOIBLE Online” and
  is located at https://phoible.org. The data published there corresponds with
  the most recent [release](https://github.com/phoible/data/releases) of this
  repository. The data on https://phoible.org is also available as a
  [CLDF StructureDataset](https://phoible.org/download).

- This repository is https://github.com/phoible/dev. Data in machine-readable
  form (`csv` and `RData`) is available here. It contains the cleaned and
  aggregated data. It is not guaranteed to exactly match what
  is published at PHOIBLE Online, due to the occasional discovery and
  correction of errors and the addition of new languages. For this reason,
  **it is recommended that you make use of the most recent release in
  your own analyses**, rather than working from the tip of the master branch.

  Because this is the development repository, it also contains scripts for
  aggregating the raw data into a single file, testing the consistency
  and integrity of the data, etc. That is where we make sure the proper IPA
  symbols are used, or that every unique phoneme has a unique representation
  in the distinctive feature system. Most users will never need to interact
  with these scripts, and can either browse [the online
  version](https://phoible.org), or work with either the [CLDF
  StructureDataset](https://phoible.org/download) or the `csv` or `RData`
  files in [the data
  directory](https://github.com/phoible/dev/tree/master/data).

- The documentation is at http://phoible.github.io. This includes notational
  conventions, departures from official IPA usage, citation information, etc.
  If you have a question about PHOIBLE, chances are you should seek answers
  there first.

_All of these sites should be considered works-in-progress._ The “published”
form of the data at https://phoible.org should be considered the most stable.
