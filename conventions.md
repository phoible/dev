PHOIBLE SEGMENT CONVENTIONS
===========================

## Diacritic ordering

Each segment type that is composed of more than one character is first normalized into a canonical decomposition form that adheres to the Unicode Normalization Form D (NFD). The diacritic ordering conventions we describe below deal with Unicode characters that are not in the 'Combining Diacritical Marks' block. The logical ordering of Combining Diacritical Marks is handled by normalization into NFD. Characters sequences that are not handled by NFD must be explicitly ordered, including characters from the 'Spacing Modifier Letters' block, which may appear as diacritics to the user. The ordering is influenced by the linguistic literature. To our knowledge, the IPA does not explicitly state in which order diacritics should appear in segments.

If a segment type contains more than one rightward diacritic, we use this order:

- unreleased/lateral release/nasal release → palatalized → labialized → velarized → pharyngealized → aspirated/ejective → long

For example, a labialized aspirated long alveolar plosive: <tʷʰː>. If a segment type contains more than one diacritic below the base segment:

- the place feature is applied first (dental, laminal, apical, fronted, backed, lowered, raised), then the laryngeal setting (voiced, voiceless, creaky voice, breathy voice), and finally the syllabic or non-syllabic marker (for vowels, ATR gets put on between place and laryngeal setting)

For example, a creaky voiced syllabic dental nasal: <n̪̰̩>


## Consonants

There are some common encoding errors that may occur when linguists use the (Latin-based) keyboard to input certain IPA symbols that Unicode has assigned to different code points. These include:

- the IPA symbol <ɡ> LATIN SMALL LETTER SCRIPT G (U+0261) is not the same code point as keyboarded <g> LATIN SMALL LETTER G (U+0067)

- the IPA symbol <ǃ> LATIN LETTER RETROFLEX CLICK (In the IPA, the <ǃ> is an alveolar or postalveolar click, not a retroflex click as stated in the Unicode Standard.) U+01C3 is not the same code point as keyboarded <!> EXCLAMATION POINT U+0021)

- the IPA symbol <ǀ> LATIN LETTER DENTAL CLICK (U$+$01C0) is not the same code point as keyboarded <|> VERTICAL LINE (U+007C)

- the IPA symbol <ʼ> MODIFIER LETTER APOSTROPHE (U+02BC) is not the same code point as keyboarded <'> APOSTROPHE (U+0027)

- the IPA symbol <ː> MODIFIER LETTER TRIANGULAR COLON (U+02D0) is not the same code point as keyboarded <:> COLON (U+003A)

In PHOIBLE we use the Unicode IPA symbols and not the keyboarded segments.

Other segment conventions relevant to consonants are given below by subsection.

### Aspiration

For aspiration, the conventions include: 

- Aspirated: pʰ
- Preaspirated: ʰt
- Breathy release: tɦ

@drammock : but this is now changed since my thesis, we have:

For aspiration we use:

- for voiceless aspirated base segments: <ʰ> MODIFIER LETTER SMALL H (U+02B0)
- for voiced aspirated base segments: <ʱ> MODIFIER LETTER SMALL H WITH HOOK (U+02B1)

When a segment is devoiced, i.e. marked with a devoiced diacritic <̥> COMBINING RING BELOW (U+0325), then we use the <ʰ> MODIFIER LETTER SMALL H (U+02B0).


### Double articulations

We do not currently use a 'tie bar', i.e. COMBINING DOUBLE INVERTED BREVE (U+0361) or COMBINING DOUBLE BREVE BELOW (U+035C), to signal double articulations (e.g. affricates, clicks and diphthongs). So for example, <k͡p> and <t͜s> appear as <kp> and <ts> in inventories in PHOIBLE.

Affricates are marked for homorganic place of articulation. For example, in SPA the 't/s-hacek-prenasalized' is indicated by the symbol <n̠t̠ʃ> and the 'voiceless retroflex sibilant affricate' in UPSID-451 is signaled by <ʈʂ>.


### Fricatives

SPA inlcudes cases like 'z-approximant' and ɻ-fricative; for these we followed the literature, which uses a downtack on a fricative to represent approximants and an uptack on approximants to make fricatives. 

- r-approximant-retroflex <ɻ>
- z-approximant-labialized-syllabic <z̞̩ʷ>
- r-flap-fricative <ɾ>

We use a lowered diacritic, the <o̞> COMBINING DOWN TACK BELOW (U+031E), with a fricative to make an approximant, e.g. SPA's 'beta-approximant' looks like <β̞>. The raised diacritic is also used with the pharyngeal fricative to indicate a voiced pharyngeal plosive <ʕ̝>.

The upwards and downwards tacks are typically used for vowels.

All 'affricated' trills and clicks are marked with the non-IPA diacritic <o͓> COMBINING X BELOW (U+0353), which we use to indicate 'frictionalized'. For example 'r-flap-fricative' in SPA and 'voiced alveolar fricative flap' in UPSID are both indicated as <ɾ͓>.

UPSID-451 forces the distinction between sibilant and non-sibilant fricatives, so another non-IPA diacritic was selected. To mark 'non-sibilant' fricatives, we use the <o͇> COMBINING EQUALS SIGN BELOW (U+0347), e.g. 'r-fricative' is <z͇>.


### Glottalization

Glottalization conventions include:

- Preglottalized: ˀd
- Glottalized / postglottalized: dˀ
- Creaky voiced / laryngealized: d̰


### Nasalization

For prenasalized consonants, i.e. homorganic nasals, we use <NC> where <N> is a nasal that agrees in place of articulation with the following consonant, e.g. <mb>, <nd>, <ŋɡ>, etc. The character <ⁿ> SUPERSCRIPT LATIN SMALL LETTER N (U+8319) is used to indicate nasal release, e.g. the 'd-nasal-release' in UPSID-451 is given as <dⁿ>.


### Clicks

Clicks are ordered with the voice setting first:

- <k> indicates voiceless
- <ɡ> indicates voiced
- <ŋ> indicates nasal

Following the voice setting, the place/manner of the click is indicated, e.g. a voiceless alveolar click is encoded as <kǃ>. Laryngeal modifiers are placed on the voice setting and diacritics for place are placed on the symbol for the click. For example, a 'voiceless nasal palatoalveolar click': <ŋ̥ǃ̠>.

All double exclamation marks should use <‼> DOUBLE EXCLAMATION MARK (U+203C).


### Labialized

Labialized segments are represented with the <ʷ> MODIFIER LETTER SMALL W (U+02B7), e.g. the 'labialized voiceless labio-velar plosive' in UPSID-451 is <kpʷ>. For velarized segments we use the <ˠ> MODIFIER LETTER SMALL GAMMA (U+02E0), e.g. SPA's 'd-velarized' is <dˠ>. Labiovelarized segments use the combination of both space modifying characters in this order: <ʷˠ>.

Labialized and labiovelarized are treated the same and represented with a superscript: ʷ


## Vowels

When a low back unrounded vowel appears in a phonological description, we use the character <ɑ> LATIN SMALL LETTER ALPHA (U+0251), even if the author used the keyboard <a> in his or her phoneme inventory chart (which seems to be the case more often than not).

For diphthongs we use <i> or <u> and not <j> or <w> to indicate the glide component of the diphthong. In cases in which this leads to a sequence of two identical vowels, we use the non-syllabic diacritic marker <o̯> COMBINING INVERTED BREVE BELOW (U+032F), e.g. SPA's 'i/yod' is marked with <ii̯>. Long vowels are marked with the length diacritic <ː>, e.g. SPA's 'iota-creaky voice-long' is <ɪ̰ː>.


## Marginal phonemes

Marginal phonemes are those that behave notably different phonologically than the majority of segments found in a particular language. Language contact factors contribute to marginal phonemes. For example, loanwords containing non-native sounds can introduce maringal phonemes into the borrowing language. There are varying degrees of 'marginal-ism'; see discussion in [1]. For PHOIBLE it would be ideal to create a ranking or vocabulary for varying degrees of marginal status  (perhaps along the line of 'anomalous' segments in UPSID [y]). To do so, we have collected any remarks about the marginality of segments as described in the resources from which we extracted inventories. However, since different authors use different descriptions of marginality, these have to be fit into some type of ranking. we propose adding this information in a future release of PHOIBLE. Currently we simply mark any type of phoneme described as marginal or loan by an author of a language description by enclosing those segments in less-than and greater-than symbols < >. 


## Allophones

Information about NA in the allophone column is now recorded on the wiki.


## Non-IPA symbols

### Epiglottal diacritic

We use the modifier letter

U+1D31 MODIFIER LETTER CAPITAL E <ᴱ>

to denote epiglottal (E for epiglottal is easy to remember). 


# References

[1] Zrinka Jelaska and Milvia Gulešć Machata. 2005. Prototypicality and the Concept Phoneme. Glossos 6.1-13.
