# PHOIBLE features

In addition to phoneme inventories, PHOIBLE includes distinctive feature data for every phoneme in every language. The feature system used was created by the PHOIBLE developers to be descriptively adequate cross-linguistically. In other words, if two phonemes differ in their graphemic representation, then they necessarily differ in their featural representation as well (regardless of whether those two phonemes coexist in any known doculect). The feature system is loosely based on the feature system in Bruce Hayes 2009 with some additions drawn from Moisik, Scott R. and Esling, John H. 2011.

However, the final feature system goes beyond both of these sources, and is potentially subject to change as new languages are added to PHOIBLE.

For more information on the design, development, and challenges of PHOIBLE, see Moran 2012.

# Files:

- README.md
- segments.csv
- complex_segments.csv
- diacritics.csv
- phoible-segments-features.tsv

# See also:

https://github.com/bambooforest/features

# Notes:

These feature tables are mostly based on two sources:
Hayes, B. P. (2009). Introductory phonology. Oxford, UK: Wiley-Blackwell.
Moisik, S. R., & Esling, J. H. (2011). The “whole larynx” approach to laryngeal features. Proceedings of the ICPhS XVII 2011 (pp. 1406–1409). Presented at the The 17th International Congress of Phonetic Sciences (ICPhS XVII), Hong Kong, CN. Retrieved from http://www.icphs2011.hk/ICPHS_CongressProceedings.htm

NOTES
changed "0" values to "-" values for features "front" and "back" for segments k, g, kp, gb, x, ɣ, ʟ, ŋ, kx, and gɣ.  This is consistent with the textual description in Hayes's book (page 87), but corrects typographical errors in his charts (pages 96-97).  Front and back values for ɰ and ɧ are not explicitly described in Hayes nor included in his charts, but by inference ɧ has also been changed from 0front 0back to -front -back (by analogy with x), and ɰ changed to -front +back (by analogy with ɯ).--drm

voicing
M&E give two voicing features “glottal source” and “epilaryngeal source.” Glottal source is equivalent to the traditional “voicing” feature and so the feature “voi” is maintained.  According to M&E the only segments that get “epilaryngeal source” are [ ʜ ʢ ].  This feature is treated as a place feature, with abbreviation “epi,” and to which we add the epiglottal plosive [ʡ].

constricted glottis
Hayes's “constricted glottis” was used for glottal stop, and for ejective sounds.  M&E's “stiff vocal folds” is roughly equivalent, except they assert +stiff for the epiglottal plosive [ʡ].  Here again M&E's account is probably more phonetically accurate, and given that the place feature “epi” makes glottal and epiglottal stops distinct, from a feature system perspective it doesn't matter whether we assign +cg to [ʡ].  For now, we retain a Hayes-like system with only glottal stop and ejectives having +cg, but a more articulatorily accurate system might make a different choice.

spread glottis
from an articulatory perspective, all voiceless consonants have a glottis spread *wider* than it is for [h], yet only [h] and [ɦ] are listed as spread glottis in the basic feature set (spread glottis is also triggered by aspiration and breathy diacritics).  M&E argue that [ħ, ʜ] should also be spread glottis on articulatory grounds, and we agree with them, but will put off a fully phonetically-valid feature set until version 2.

clicks
clicks are represented with the coronal / labial features of the front closure, and the dorsal features of the back closure (to allow contrast between velar- and uvular-backed clicks).  Note that for the palatal clicks this is slightly different than previous treatments, in that palatal consonants normally have both coronal and dorsal features; in this case, the coronal features still suffice to distinguish the palatal click from other click types, so the dorsal features of a front palatal closure are are overridden by the dorsal features of the back closure.  All clicks are also [+click].

long and short
new features added not in either of the above sources: [short].  This was added to deal with the “half-long” and “short” diacritics.  Normal segments will be -shrt,-long; long segments will be -shrt,+long; “half-long” segments will be +shrt,+long; and “short” segments will be +shrt,-long.

ATR & RTR
The Hayes feature system included ATR as a feature that applied to vowels only (default for vowels was -atr, default for consonants was 0atr).  Some phonological evidence suggests that there are languages with both ATR harmony and RTR harmony.  Under an ATR-only system, ATR has its own feature and RTR/pharyngealization are represented as the set of dorsal feature values for pharyngeal consonants: +dors, -hi, +lo, -fr, +bk (these are the same features used to describe a low back vowel).  This causes problems for languages with RTR or pharyngealized vowels; the best an ATR-only system can do is to say that the plain vowels in such a language are +ATR, and the pharyngealized vowels are -ATR (which works fine within a single language, but is not ideal for cross-linguistic comparison purposes).  Worse is to use dorsal features to represent pharyngealization: overwriting the dorsal features would lead a pharyngealized [i] to be the same as an [ɑ], for instance.  To deal with this problem we have introduced an RTR feature to complement the ATR feature.  We acknowledge, following M&E, that in many cases pharyngealization does not actually involve tongue backing, and does not have expected coarticulatory effects that tongue backing would entail.  However, for now, we settle for a featurally adequate if phonetically imperfect solution.  Note that the feature values for left tack and right tack diacritics follow naturally (left tack = +atr, right tack = +rtr).  We acknowledge that this is at odds with how some people use the left and right tack diacritics, i.e., to indicate movement in F2xF1 space (slightly fronted/slightly backed).  However, we believe in most cases a slightly backed [i] is meant to be still a front vowel and still tense (i.e., neither [ɨ] nor [ɪ]) and as such there are no features that could accurately capture this usage of the left/right tacks to mean “slightly fronted/backed.”  Note also that the the right tack and the pharyngealized diacritic (ˤ) are featurally identical and therefore redundant; both are included for compatibility purposes since, by convention, the tacks are used for vowels and the pharyngeal modifier for consonants.

glottalization, creaky, fortition, ejectives & implosives
Previously the superscript glottal stop diacritic was not included, and any cases of its use were either resolved as creaky or as a glottalstop-consonant sequence.  Although the superscript glottal stop is not official IPA, we have decided to include it to allow transcription of, e.g., preglottalization, or non-ejective glottalized voiceless consonants (where creaky diacritic is inappropriate).  Under a Hayes-like system, superscript glottal stop would have the same features as the creaky-voicing diacritic AND the ejective diacritic (+cg, -sg).  This is obviously not ideal.  Raised glottal stop is now considered featurally synonymous with creaky voicing (+sg, -cg).  M&E's feature “raised larynx” was introduced to differentiate ejectives from creaky/glottalized sounds.  However, we decline to follow M&E in setting [ʡ ħ ʕ ʜ ʢ] as +rlx, because although that may be phonetically accurate, we are unaware of any languages in which pharyngeal and epiglottal sounds pattern phonologically with ejectives.  In a future version of this feature set which is more articulatorily accurate, we may change this (as well as revisiting the relationship between “fortis”, “stiff vocal folds”, “constricted glottis”, “raised larynx”, etc.  Given the addition of raised larynx, it also made sense to include “lowered larynx” to replace the “implosive” feature used by Hayes.

mid-centralization
the mid-centralized diacritic is defined as imparting the values of schwa on the features hi, lo, fr, bk, tns (rounding is unaffected).

advanced & retracted consonants
The system has been designed so that the advanced (plus) and retracted (minus) diacritics are used for consonants, and the four “tack” diacritics are reserved for vowels.

diacritics added that are not part of the official IPA:
a͈  [+fortis]
a͉  [-fortis]
aᴴ  [+epi]
aˀ  [+cg, -sg]  (synonymous with creaky)


symbols that are judged to be “phonetic detail” diacritics and thus are not handled (i.e., they are defined as transparent in the system, and don't change any feature values of their base glyphs)
a̹  (“more rounded”)
a̜  (“less rounded”)

outstanding issues:
uptack and downtack: either have to be conditional (if [+lo] then uptack = [-lo], else if [-hi] then uptack = [+hi], etc).  Or, we have to say that up/downtack signal “phonetic detail” that is non-contrastive, and we ignore those diacritics for featural purposes.  The second option makes more sense to me; we're already treating “more/less rounded” as transparent.
