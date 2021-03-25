# COMPARATIVE NOTES ON THE ANALYSIS OF AUSTRALIAN PHONEME INVENTORIES

The `ER` source data comes from [Erich
Round](https://www.surrey.ac.uk/people/erich-round) and
should be cited as:

> Round, Erich R. 2021. Phonemic inventories of Australia, Version 3.0 (database of 404 languages). In Steven Moran & Daniel McCloy (Eds.), PHOIBLE 3.0. Jena: Max Planck Institute for the Science of Human History.

Notes on the inventories and their compilation from Erich are below. See also:

> Round, Erich. (2021, March 26). Australian phonemic inventories contributed to PHOIBLE 3.0: Essential explanatory notes (Version 2.0). Zenodo.

The data are available in phoible long format in
[ER_inventories.tsv](ER_inventories.tsv) and contain only phonemes, with no information on allophones. We have also collected for each citation a BibTeX reference, available in the [phoible-references.bib](../../data/phoible-references.bib) file. See the [InventoryID-Bibtex.csv](../../mappings/InventoryID-Bibtex.csv) mapping file for details. For up-to-date language codes for each inventory, we maintain a phoible index here:
[InventoryID-LanguageCodes.csv](../../mappings/InventoryID-LanguageCodes.csv).

Contents:
- Overview
- Phoneme labels
- Segmentation
- Complex segments
- Coronal places of articulation
- Stop series
- Obstruent series
- Taps, flaps and lenis apical stops
- Vowel length
- Glottal stop
- Tone
- Additions since version 1.0
- Comments on specific languages

## OVERVIEW

Phonemic analysis is a valuable prelude to cross-linguistic comparison.
Nevertheless, phonemicization is not a deterministic process and linguistic
practice is not uniform. This has certain consequences for the compilation and
use of cross-linguistic summary datasets such as this one. The following notes
are arranged by topic and highlight how these issues have been accommodated in
the compilation of the Australian dataset. Notes on individual languages follow 
at the end. For an extended discussion, see Round (2021a, 2021b).

## PHONEME LABELS

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
particularly ones which descend historically from \*[X] or \*[Y], as a complex
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
/t/, /n/ etc. for apical alveolars and IPA retroflex for retroflexes. In the
first edition of this dataset, in languages with no alveolar–retroflex 
contrast, the single apical place was explicitly labelled as apical (with 
the IPA inverted bridge diacritic). This seems to have invited confusion, so 
in the second edition I have removed it.

## STOP SERIES

Some Australian languages contrast two series of stops, typically matched at
all superlaryngeal places of articulation, and sources may phonemicize them in
a number of ways. For the purposes of enhanced cross-linguistic comparability,
this dataset follows Butcher (2004, 2012) and phonemicizes them as fortis and
lenis, with no voicing contrast.

## OBSTRUENT SERIES

Many Australian langauges are described as having stop phonemes with fricative
allophones. The most common practice is to phonemicize these as a series of 
stops, even when the stop allophone is highly infrequent (e.g. Hale
1997:209-210 on Linngithigh). For the purposes of enhanced cross-linguistic 
comparability, I carry this practice over to the rarer cases where analysts 
have split what could be one series of obstruents into a series of stops and
a series of fricatives at complementary places of articulation (e.g.
Mansfield 2019 on Murrin-patha).

## TAPS, FLAPS AND LENIS APICAL STOPS

Some Australian languages have an extra, voiced lenis, stop-like or
tap/flap-like phoneme solely at the apical place(s) of articulation. Sources
may phonemicize these as voiced/lenis stops or as taps/flaps (see Breen 1997
for discussion). For cross-linguistic comparability, they are phonemicized here
as tap/flaps.

## VOWEL LENGTH

Most sources phonemicize long vowels as a single long vowel segment. Some
phonemicize them as two, adjacent short vowels (e.g. McDonald & Wurm 1979:5 for
Wangkumara [a:]). The phonemicization here is as the single, long segment.

## GLOTTAL STOP

Some sources analyse the unpredictable presence/absence of a glottal stop as a
prosodic feature (e.g. Waters 1980 for Djinang) and others as a phonemic
segment. The representation here is as a phonemic segment.

## TONE

No Australian language has lexically distictive / phonemic tone.

### Works cited above
- Breen, Gavan. 1981. The Mayi Languages of the Queensland Gulf Country. A.I.A.S. New Series 29. Canberra: Australian Institute of Aboriginal Studies.
- Breen, Gavan. 1997. “Taps, Stops and Trills.” In Boundary Rider: Essays in Honour of Geoffrey O’Grady, edited by Darrell Tryon and Michael Walsh, 71–93. Canberra: Pacific Linguistics.
- Butcher, Andrew R. 2004. “‘Fortis/Lenis’ Revisited One More Time: The Aerodynamics of Some Oral Stop Contrasts in Three Continents.” Clinical Linguistics & Phonetics 18: 547–57.
- Butcher, Andrew R. 2012. “On the Phonetics of Long, Thin Phonologies.” In Quantitative Approaches to Problems in Linguistics, edited by Cathryn Donohue, Shunichi Ishihara, and William Steed, 133–154. Munich: Lincom Europa.
- Crowley, Terry. 1981. “The Mpakwithi Dialect of Anguthimri.” In Handbook of Australian Languages, edited by R. M. W. Dixon and Barry Blake, 2:147–194. Canberra: Australian National University Press.
- Dixon, R. M. W. 1991. “Mbabaram.” In Handbook of Australian Languages, edited by R. M. W. Dixon and Barry J. Blake, 4:348–402. Melbourne: Oxford University Press.
- Hale, Kenneth. 1997. “A Linngithigh Vocabulary.” In Boundary Rider: Essays in Honour of Geoffrey O’Grady, edited by Darrell Tyron and Michael Walsh, 209–46. Pacific Linguistics Series C 136. Canberra: Pacific Linguistics.
- Hamilton, Philip James. 1996. “Phonetic Constraints and Markedness in the Phonotactics of Australian Aboriginal Languages.” Ph.D. dissertation, University of Toronto.
- Harvey, Mark. 2011. “Prepalatals in Arandic.” Australian Journal of Linguistics 31 (1): 79–110.
- Mansfield, John. 2019. Murrinhpatha morphology and phonology. Boston: Walter de Gruyter.
- McDonald, Maryalyce, and Stephen A. Wurm. 1979. Basic Materials in Waŋkumara (Gaḷali): Grammar, Sentences and Vocabulary. Pacific Linguistics Series B 65. Canberra: Pacific Linguistics. 
- Round, Erich R. 2009. Kayardild Phonology, Morphology, and Morphosyntax. Ph.D. Dissertation, New Haven: Yale University.
- Round, Erich R. 2021a forthcoming. Segment inventories. In Oxford Guide to Australian languages, edited by Claire Bowern. Oxford: Oxford University Press.
- Round, Erich R. 2021b forthcoming. Phonotactics. In Oxford Guide to Australian languages, edited by Claire Bowern. Oxford: Oxford University Press.
- Sommer, Bruce A. 1969. Kunjen Phonology: Synchronic and Diachronic. Pacific Linguistics Series B 11. Canberra: Pacific Linguistics. 
- Waters, Bruce E. 1980. “Djinang Phonology.” In Papers in Australian Linguistics No. 14, edited by Bruce E. Waters and P. Busby, 1–72. Pacific Linguistics Series A 60. Canberra: Pacific Linguistics.

# ADDITIONS SINCE VERSION 1.0

The following varieties have been added: Agu Aloja, Barundji, Danggali, Flinders Island, Kaantju, Kokiny, Koko Dhawa, Kurnu, Marawara, Naualko, Ogh Alungul, Ogh Angkula, Wilyagali. The source has been changed for Mangala, from Sharp (2004) to Agnew (2020) and for Murrinh-patha, from Mansfied (2014) to Mansfield (2019).

# COMMENTS ON SPECIFIC LANGUAGES

- Adnyamathanha (adt, adny1235, Phoible inventory 2931) Prestopped sonorants in Schebeck 1974 interpreted here as segment sequences.
- Aghu Tharnggala (ggr, aghu1254, Phoible inventory 2880) Prenasalized stops in Jolly 1989 interpreted as nasal + lenis stop.
- Agu Aloja (aghu1254) Prenasalized stops in Rigsby 1976 interpreted here as segment sequences.
- Alngith (aid, alng1239, Phoible inventory 2908) Trill-released and prenasalized stops in Hale 1997 interpreted here as segment sequences.
- Alyawarr (aly, alya1239, Phoible inventory 2702) Prestopped nasals in Breen 2001 interpreted here as segment sequences.
- Anindilyakwa (aoi, anin1240, Phoible inventory 2651) Prenasalized stops and labiovelars in van Egmond 2012 interpreted here as segment sequences.
- Antekerrepenhe (adg, ande1247, Phoible inventory 2705) Prestopped nasals in Breen 2001 interpreted here as segment sequences.
- Aritinngithigh (dth, arit1239, Phoible inventory 2909) Trill-released stops in Hale 1976 interpreted here as segment sequences.
- Awu Alaya (typ, thay1248, Phoible inventory 2881) Prenasalized stops in Rigsby 1976 interpreted as nasal + lenis stop.
- Bakanh (pkn, paka1251, Phoible inventory 2901) Hamilton's online pdf inventory omits glottal stop.
- Balardung (nys, nyun1247, Phoible inventory 2864) This inventory is tentative.
- Barada (gnl, gang1268, Phoible inventory 2798) This inventory is tentative.
- Barna (bzr, biri1256, Phoible inventory 2799) This inventory is tentative.
- Barunggam (gbw, kabi1260, Phoible inventory 2942) This inventory is tentative. The data in Holmer 1983 likely conflates a distinct rhotic glide and trill.
- Bibbulman (xbp, bibb1234, Phoible inventory 2865) This inventory is tentative.
- Bidhawal (ihw, gana1268, Phoible inventory 2788) This inventory is tentative.
- Bigambal (xbe, biga1237, Phoible inventory 2982) This inventory is tentative.
- Binbinka (wmb, binb1241, Phoible inventory 2688) Two adjacent identical short vowels in Chadwick 1978 interpreted here as one long vowel.
- Biri (bzr, biri1256, Phoible inventory 2800) This inventory is tentative.
- Boonwurrung (wyi, boon1243, Phoible inventory 2775) This inventory is tentative.
- Buwandik (xbg, bung1264, Phoible inventory 2717) This inventory is tentative.
- Central Arrernte (aer, mpar1238, Phoible inventory 2706) Prestopped nasals and prepalatalized segments in Breen 2001 interpreted here as segment sequences.
- Darkinyung (xda, hawk1239, Phoible inventory 2984) This inventory is tentative.
- Daungwurrung (wyi, daun1243, Phoible inventory 2774) This inventory is tentative.
- Dharawal (tbh, thur1254, Phoible inventory 2993) This inventory is tentative.
- Dharawala (bym, bidy1243, Phoible inventory 2812) This inventory is tentative.
- Dharuk (xdk, sydn1236, Phoible inventory 2985) This inventory is tentative.
- Dharumba (tbh, thur1254, Phoible inventory 2994) This inventory is tentative.
- Dharumbal (xgm, dhar1248, Phoible inventory 2813) This inventory is tentative.
- Dhudhuroa (xyy, dhud1236, Phoible inventory 2727) This inventory is tentative. /a:/ is not mentioned in the phonology chapter of Blake&Reid 2002 but is in the lexicon.
- Dhurga (dhu, dhur1239, Phoible inventory 2995) This inventory is tentative.
- Diyari (dif, dier1241, Phoible inventory 2749) Trill-released stops in Austin 1981 interpreted here as segment sequences.
- Djabwurung (tjw, djab1234, Phoible inventory 2785) This inventory is tentative.
- Djirringany (xtv, sout2771, Phoible inventory 2996) This inventory is tentative.
- Eastern Anmatyerre (amx, east2380, Phoible inventory 2703) Prestopped nasals and prepalatalized segments in Green 2010 interpreted here as segment sequences.
- Eastern Arrernte (aer, east2379, Phoible inventory 2708) Prestopped nasals and prepalatalized segments in Breen 2001 interpreted here as segment sequences.
- Eora (xdk, sydn1236, Phoible inventory 2986) This inventory is tentative.
- Gabalbara (gnl, gang1268, Phoible inventory 2801) This inventory is tentative.
- Gabi-Gabi (gbw, kabi1260, Phoible inventory 2941) This inventory is tentative. The data in Holmer 1983 likely conflates a distinct rhotic glide and trill.
- Galaagu (kba, kala1379, Phoible inventory 2835) This inventory is tentative.
- Gangulu (gnl, gang1268, Phoible inventory 2802) This inventory is tentative.
- Ganulu (gnl, gang1268, Phoible inventory 2803) This inventory is tentative.
- Garingbal (bzr, biri1256, Phoible inventory 2804) This inventory is tentative.
- Garlali (gll, kala1380, Phoible inventory 2767) This inventory is tentative.
- Gidabal (gih, gida1240, Phoible inventory 2713) The place of articulation labeled 'dental' in Geytenbeek 1971 is 'apical' in standard Australianist terms.
- Goreng (xgg, gore1235, Phoible inventory 2866) This inventory is tentative.
- Gundungurra (xrd, gund1248, Phoible inventory 2997) This inventory is tentative.
- Gunggari (kgl, kung1258, Phoible inventory 2815) This inventory is tentative.
- Gureng-gureng (gnr, gure1255, Phoible inventory 2938) This inventory is tentative.
- Gurindji (gue, guri1247, Phoible inventory 2853) Long vowels are not mentioned in Meakins 2013 pp15-18, but see elsewhere in that source.
- Guwa (xgw, guwa1242, Phoible inventory 2737) This inventory is tentative.
- Guwamu (gwu, guwa1243, Phoible inventory 3017) This inventory is tentative.
- Guwar (bdy, guwa1244, Phoible inventory 2714) This inventory is tentative. Inventory inferred from information in Jefferies 2011.
- Guweng (gnr, gure1255, Phoible inventory 2939) This inventory is tentative.
- Jabirr-Jabirr (dyb, dyab1238, Phoible inventory 2699) Inventory inferred from information in McGregor 1996:11-12.
- Jandai (jan, jand1248, Phoible inventory 2728) This inventory is tentative. Inventory inferred from information in Jefferies 2011.
- Jaru (ddj, jaru1254, Phoible inventory 2854) /iji, uwu/ in Tsunoda 1981 phonemicized as /i:,u:/.
- Jawi (djw, djaw1238, Phoible inventory 2700) This inventory is tentative. Inventory inferred from information in McGregor 1996:11-12.
- Jukun (dyd, dyug1238, Phoible inventory 2695) This inventory is tentative. Inventory inferred from information in Hosokawa 1991.
- Kalaamaya (lkm, kala1401, Phoible inventory 2836) This inventory is tentative.
- Kalkatungu (ktg, kalk1246, Phoible inventory 2739) Two adjacent identical short vowels in Blake 1979 interpreted here as one long vowel.
- Kaniyang (nys, kani1276, Phoible inventory 2867) This inventory is tentative.
- Karangura (nmv, kara1508, Phoible inventory 2751) This inventory is tentative. Inventory inferred from information in Austin 1990, compared to Ngamini. Trill-released stops in Austin 1990 interpreted here as segment sequences.
- Kariyarra (vka, kari1304, Phoible inventory 2839) Retroflex nasal is missing from list in Wangka Maya 2001:11-12, but is in the lexicon.
- Kaurna (zku, kaur1267, Phoible inventory 2932) This inventory is tentative. Prestopped sonorants in Amery & Simpson 1974 interpreted here as segment sequences.
- Kaytetye (gbb, kayt1238, Phoible inventory 2711) Prestopped nasals and prepalatalized segments in Breen 2001 interpreted here as segment sequences.
- Keramin (nay, kera1256, Phoible inventory 2792) This inventory is tentative.
- Kokiny (kawa1290) This inventory is tentative. Inventory inferred from wordlist.
- Kungarakany (ggk, kung1259, Phoible inventory 2675) Phonemic inventory inferred from word data in Parish 1983.
- Kungardutji (xwk, wong1246, Phoible inventory 2759) Two adjacent identical short /a/ vowels in McDonald & Wurm 1979 interpreted here as one long vowel.
- Kungkari (lku, kuun1236, Phoible inventory 2765) This inventory is tentative.
- Kurrama (vku, kurr1243, Phoible inventory 2840) Retroflex lateral & dental glide are missing from list in Wangka Maya 2001:23, but are in the lexicon.
- Kurtjar (gdj, gurd1238, Phoible inventory 2905) Retroflex 'glide~tap' in Black 1996 phonemicised here as a glide.
- Ladji-Ladji (llj, ladj1234, Phoible inventory 2779) This inventory is tentative.
- Lamalama (lby, lamu1254, Phoible inventory 2894) Prenasalized stops in Verstraete 2018 interpreted as nasal + lenis stop.
- Linngithigh (lnj, leni1238, Phoible inventory 2912) Prenasalized stops in Hale 1997 interpreted as nasal + lenis stop; trill-released stops interpreted here as segment sequences.
- Lower Southern Aranda (axl, lowe1436, Phoible inventory 2709) Prestopped nasals and prepalatalized segments in Breen 2001 interpreted here as segment sequences.
- Malthanmungu (bpt, barr1247, Phoible inventory 2892) Inferred from lexical data in Sutton nd.
- Malyangapa (yga, maly1234, Phoible inventory 2968) Phonemic inventory inferred from lexical data in Hercus 1989.
- Marra (mec, mara1385, Phoible inventory 2679) [o], found only in an interjection, is not included in the inventory here.
- Marramaninyshi (zmm, mari1417, Phoible inventory 2637) This inventory is tentative.
- Mbabaram (vmb, mbab1239, Phoible inventory 2928) Word-final schwa in Dixon 1991 represented here as phoneme. Despite suggestion of p356, no long barred-i is attested.
- Minang (nys, nyun1247, Phoible inventory 2868) This inventory is tentative.
- Miyan (bzr, biri1256, Phoible inventory 2805) This inventory is tentative.
- Muk-Thang (unn, gana1278, Phoible inventory 2789) This inventory is tentative.
- Murrinh-patha (mwf, murr1259) Voiced fricatives in Mansfield 2019 interpreted here as lenis stops.
- Nari Nari (rnr, nari1241, Phoible inventory 2784) This inventory is tentative.
- Narrungga (nnr, naru1238, Phoible inventory 2936) Prestopped sonorants in Eira 2010 interpreted here as segment sequences.
- Ndra'ngith (dgt, tyan1235, Phoible inventory 2910) Trill-released stops in Hale 1976 interpreted here as segment sequences.
- Ngadjunmaya (nju, ngad1258, Phoible inventory 2838) This inventory is tentative.
- Ngamini (nmv, ngam1284, Phoible inventory 2752) Trill-released stops in Austin 1988, Breen 1997 interpreted here as segment sequences.
- Ngardi (rxd, ngar1288, Phoible inventory 2857) Prenasalized stops in Jagst 1975 interpreted here as segment sequences.
- Nimanburu (nmp, nima1245, Phoible inventory 2698) This inventory is tentative. Inventory inferred from information in McGregor 1996:11-12; Bowern 2010.
- Nulit (unn, gana1278, Phoible inventory 2791) This inventory is tentative.
- Ogh Alungul (aghu1254) This inventory is tentative. Inventory inferred from wordlist.
- Ogh Angkula (ikar1243) Prenasalized stops in Rigsby 1976 interpreted here as segment sequences.
- Parnkalla (bjb, bang1339, Phoible inventory 2935) This inventory is tentative. Prestopped sonorants in O'Grady 2001 interpreted here as segment sequences.
- Pinjarup (pnj, pinj1244, Phoible inventory 2869) This inventory is tentative.
- Pirlatapa (bxi, pirl1238, Phoible inventory 2753) This inventory is tentative. Trill-released stops in Breen 1997 interpreted here as segment sequences.
- Pirriya (xpa, pirr1240, Phoible inventory 2766) Lack of /u:/ may be an accidental gap due to small dataset.
- Punthamara (xpt, punt1240, Phoible inventory 2758) This inventory is tentative.
- Rimanggudinhma (zmv, mbar1253, Phoible inventory 2895) Prenasalized stops in Godman 1993 interpreted as nasal + lenis stop.
- Tableland Lamalama (lby, lamu1254, Phoible inventory 3020) This inventory is tentative. Prenasalized stops in Verstraete 2018 interpreted as nasal + lenis stop.
- Takalak (ikr, taga1279, Phoible inventory 2879) This inventory is tentative.
- Thangguai (unn, gana1278, Phoible inventory 2790) This inventory is tentative.
- Thawa (xtv, sout2771, Phoible inventory 3000) This inventory is tentative.
- Thirarri (dit, dira1238, Phoible inventory 2750) Trill-released stops in Austin 1981 interpreted here as segment sequences.
- Tiwi (tiw, tiwi1244, Phoible inventory 3005) /r/+alveolar in Osborne 1974 phonemicized here as retroflex.
- Wadikali (wdk, wadi1261, Phoible inventory 2969) This inventory is tentative. Phonemic inventory inferred from lexical data in appendix of Hercus&Austin 1994.
- Wadjabangay (bym, bidy1243, Phoible inventory 2821) This inventory is tentative.
- Wajuk (xwj, nyun1247, Phoible inventory 2871) This inventory is tentative.
- Waka Waka (wkw, waka1274, Phoible inventory 2944) This inventory is tentative. The data in Holmer 1983 likely conflates a distinct rhotic glide and trill.
- Wangan (bzr, biri1256, Phoible inventory 2822) This inventory is tentative.
- Wangkumara (xwk, wong1246, Phoible inventory 2760) Two adjacent identical short /a/ vowels in McDonald & Wurm 1979 interpreted here as one long vowel.
- Wardandi (wxw, ward1248, Phoible inventory 2870) This inventory is tentative.
- Warrnambool (xbg, warr1257, Phoible inventory 2718) This inventory is tentative. /a:/ is not mentioned in the phonology chapter of Blake 2003 but is in the lexicon.
- Wathawurrung (wth, wath1238, Phoible inventory 2776) This inventory is tentative.
- Wathi Wathi (tbh, wadi1260, Phoible inventory 2781) This inventory is tentative.
- Western Arrernte (are, west2441, Phoible inventory 2710) Prestopped nasals and prepalatalized segments in Breen 2001 interpreted here as segment sequences.
- Wiilman (nys, nyun1247, Phoible inventory 2872) This inventory is tentative.
- Wirangu (wiw, wira1265, Phoible inventory 2934) Two adjacent identical short /a/ vowels in Hercus 1992 interpreted here as one long vowel.
- Wiri (bzr, biri1256, Phoible inventory 2806) This inventory is tentative.
- Woiwurrung (wyi, woiw1238, Phoible inventory 2777) This inventory is tentative.
- Wubuy (nuy, nung1290, Phoible inventory 2665) The morphophonemic w1, w2 of Heath 1984 are represented here as their phonemic realizations /w,b,k/.
- Wudjari (nys, nyun1247, Phoible inventory 2873) This inventory is tentative.
- Wuli Wuli (wlu, wuli1242, Phoible inventory 2945) This inventory is tentative. The data in Holmer 1983 likely conflates a distinct rhotic glide and trill.
- Yabula-Yabula (xyy, yabu1234, Phoible inventory 2980) This inventory is tentative.
- Yagara (yxg, yaga1262, Phoible inventory 2731) This inventory is tentative. Inventory inferred from information in Jefferies 2011.
- Yambina (bzr, biri1256, Phoible inventory 2807) This inventory is tentative.
- Yanda (yda, yand1251, Phoible inventory 2738) This inventory is tentative.
- Yandruwandha (ynd, yand1253, Phoible inventory 2754) Trill-released stops and prestopped laterals in Breen 2004 interpreted here as segment sequences.
- Yangga (bzr, biri1256, Phoible inventory 2808) This inventory is tentative.
- Yanyuwa (jao, yany1243, Phoible inventory 2950) Two adjacent identical short /a/ vowels in Kirton & Charlie 1987 interpreted here as one long vowel; prenasalized stops interpreted as segment sequences.
- Yardliyawarra (yxl, yarl1236, Phoible inventory 2970) This inventory is tentative. Phonemic inventory inferred from lexical data in appendix of Hercus&Austin 1994.
- Yawarrawarrka (yww, yawa1258, Phoible inventory 2756) Trill-released stops and prestopped laterals in Breen 2004 interpreted here as segment sequences.
- Yetimarala (gnl, gang1268, Phoible inventory 2809) This inventory is tentative.
- Yilba (bzr, biri1256, Phoible inventory 2810) This inventory is tentative.
- Yindjibarndi (yij, yind1247, Phoible inventory 2849) Note that [o:] of Wordick 1982:37 is variant of /uwa/.
- Yiningay (bym, bidy1243, Phoible inventory 2824) This inventory is tentative.
- Yitha Yitha (xth, lowe1403, Phoible inventory 2796) This inventory is tentative.
- Yorta Yorta (xyy, yort1237, Phoible inventory 2981) This inventory is tentative.
- Yugambal (yub, yuga1244, Phoible inventory 2983) This inventory is tentative.
- Yuwat (nys, nyun1247, Phoible inventory 2874) This inventory is tentative.
- Yuwi (bzr, biri1256, Phoible inventory 2811) This inventory is tentative.