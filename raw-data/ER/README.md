# COMPARATIVE NOTES ON THE ANALYSIS OF AUSTRALIAN PHONEME INVENTORIES

The `ER` source data comes from [Erich
Round](https://languages-cultures.uq.edu.au/profile/1160/erich-round) and
should be cited as:

> Round, Erich R. 2019. Phonemic inventories of Australia.

Notes on the inventories and their compilation from Erich are below. The data
are available in phoible long format in
[ER_inventories.tsv](ER_inventories.tsv) and contain only phonemes, with no
information on allophones or linguistic tone.


## OVERVIEW

Phonemic analysis is a valuable prelude to cross-linguistic comparison.
Nevertheless, phonemicization is not a deterministic process and linguistic
practice is not uniform. This has certain consequences for the compilation and
use of cross-linguistic summary datasets such as this one. The following notes
are arranged by topic and highlight how these issues have been accommodated in
the compilation of the Australian dataset.

## LABELS

The labeling of phonemes acts to group phonemes, implicitly or explicitly, into
classes (e.g. ‘the stops’), yet phonemes may class together differently with
respect to allophony vs phonological alternations vs phonotactics and even vs
their history. Consequently, analysts are often forced to choose between
multiple conceivable classifications, each of which captures only some of the
facts. When they do so, one analyst’s criteria may differ from those of
another. As a result, some cross-doculectal variation in phoneme labels may
reflect more a variation among analysts’ necessary choices between multiple,
defensible options, than empirical differences among languages. Where this
appears to be the case, I have in some instances relabelled phonemes relative
to source documents, to promote cross-linguistic comparability. For example,
stops in languages with no contrast for voicing or fortition are labelled here
with plain, voiceless IPA symbols, following widespread phonological practice.
More cases are noted below.

## SEGMENTATION

The task of dividing words into segments can itself present challenges, since
in this task phonemic analysis becomes interdependent with the analysis of
phonotactics. The phonotactics of Australian languages are highly constrained
(Hamilton 1996), and this has enabled researchers to converge on broadly
similar criteria for segmentation, enhancing the comparability of
cross-linguistic data. However, in cases where languages contain unusual
phonotactic sequences at the phonetic level, the analytical response in terms
of segmentation and phonemicization has been more varied. By comparing across
sources, it is possible to discern certain recurrent patterns of analytic
practice. The next set of topics records attempts to accommodate these issues
in the Australian dataset. The aim has been to bring to the fore empirical
variation, over the variability among analytic choices.

## COMPLEX SEGMENTS

Some sources analyse a phonotactically unusual phonetic sequence [X+Y],
particularly ones which descend historically from *[X] or *[Y], as a complex
phonemic segment /XY/; others analyse it as a phonemic sequence /X/+/Y/.
However, no complex segment /XY/ posited for an Australian language is
contrastive with /X/+/Y/, at least under some, reasonable set of assumptions
about the rest of the inventory. Consequently, for the purposes of enhanced
cross-linguistic comparability, all such sequences are treated in the dataset
as two-segment phonemic sequences, /X/+/Y/. Notes on specific types: (1) A
minority of Australian languages permit phonetic stop+nasal or stop+lateral
sequences only at homorganic places of articulation. In original sources these
are analysed as sequences (e.g. Sommer 1969 on Kunjen) or single complex
segments (e.g. Breen 1981 on Arandic languages). (2) Almost all Australian
languages permit phonetic nasal+stop sequences at homorganic places of
articulation, but word-initially they are rare. In original sources they are
analysed as sequences (e.g. Dixon 1991 on Mbabaram) or single complex segments
(e.g. Crowley 1981 on Mpakwithi). (3) Few Australian languages permit phonetic
stop+liquid sequences. The sequence [tr] in particular is sometimes analysed in
original sources as a single complex segment (e.g. Hale 1997 on Linngithigh).
(4) In Australian languages [w] often colors neighboring vowels; furthermore,
most Australian languages permit [C+w] phonetic sequences, and these may be
temporally coarticulated (e.g. Round 2009:65 on Kayardild). In some languages,
these facts are analysed with phonemic /C/+/w/ sequences (e.g. Round 2009), but
occasionally with complex, labialized consonants (Dixon 1991 on Mbabaram). (5)
Harvey (2011) presents arguments for analysing ‘pre-palatalized’ complex
segments in Arandic languages as /j/+/C/.

## CORONAL PLACES OF ARTICULATION

All Australian languages contrast either one or two ‘apical’ places of
articulation, where the primary constriction is formed with the tongue tip; and
either one or two ‘laminal’ places of articulation, with constriction formed by
the tongue blade. In this dataset, laminals are labelled either as dental (with
the IPA bridge diacritic) or pre-palatal (with IPA curl). Apicals are plain
/t/, /n/ etc. for apical alveolars, IPA retroflex for retroflexes, and for
languages with no alveolar–retroflex contrast, the single apical place is
explicitly labelled as apical (with the IPA inverted bridge diacritic).

## STOP SERIES

Some Australian languages contrast two series of stops, typically matched at
all superlaryngeal places of articulation, and sources may phonemicize them in
a number of ways. For the purposes of enhanced cross-linguistic comparability,
this dataset follows Butcher (2004, 2012) and phonemicizes them as fortis and
lenis, with no voicing contrast.

## TAPS, FLAPS AND LENIS APICAL STOPS

Some Australian languages have an extra, voiced lenis, stop-like or
tap/flap-like phoneme solely at the apical place(s) of articulation. Sources
may phonemicize these as voiced/lenis stops or as taps/flaps (see Breen 1997
for discussion). For cross-linguistic comparability, they are phonemicized here
as tap/flaps.

**editor's note:** The retroflex lateral flap that was originally represented
as `ɺ̢` (`027A+0322`, the alveolar lateral flap + retroflex hook) in Dr.
Round's data has been changed by the PHOIBLE editors to be represented as `ɺ̠̺`
(`027A+0320+033A`, the alveolar lateral flap with retraction and apical
diacritics).

## VOWEL LENGTH

Most sources phonemicize long vowels as a single long vowel segment. Some
phonemicize them as two, adjacent short vowels (e.g. McDonald & Wurm 1979:5 for
Wangkumara [a:]). The phonemicization here is as the single, long segment.

## GLOTTAL STOP

Some sources analyse the unpredictable presence/absence of a glottal stop as a
prosodic feature (e.g. Waters 1980 for Djinang) and others as a phonemic
segment. The representation here is as a phonemic segment.
