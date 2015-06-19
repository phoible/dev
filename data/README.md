# PHOIBLE data
PHOIBLE is a database of phonological inventories and distinctive features, encompassing more than 1600 languages (and growing). Here are links to various parts of the project:

- The web-browsable published form of PHOIBLE is called “PHOIBLE Online” and is located at http://phoible.org. The data published there corresponds with the most recent [year-numbered release](https://github.com/phoible/data/releases) of this repository.

- This repository is https://github.com/phoible/data. Data in machine-readable form is available here. It contains the cleaned and aggregated data as a tab-delimited text file, and in binary form as an .Rdata file (for use with [R](http://www.r-project.org/)). It is not guaranteed to exactly match what is published at PHOIBLE Online, due to the occasional discovery and correction of errors and the addition of new languages. For this reason, **it is recommended that you make use of the most recent dated release in your own analyses**, rather than working from the tip of the master branch.

- The development repository is at https://github.com/phoible/dev. It contains scripts for aggregating the data into a single file, testing the consistency and integrity of the data, etc. That is where we make sure the proper IPA symbols are used, or that every unique phoneme has a unique representation in the distinctive feature system. Most users will never need to visit the development repository.

- The documentation is at http://phoible.github.io. This includes notational conventions, departures from official IPA usage, citation information, etc. If you have a question about PHOIBLE, chances are you should seek answers there first.

_All of these sites should be considered works-in-progress._ The “published” form of the data is the most stable.
