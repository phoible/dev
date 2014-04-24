Data from:

- Maddieson, Ian and Kristin Precoda. 1990. Updating UPSID. In UCLA Working Papers in Phonetics, 74, 104-111.
- Maddieson, Ian. 1984. Pattern of Sounds. Cambridge University Press.





NOTES: Below are the old instructions for loading UPSID csv files into SQL tables and for generating an output format that could be loaded into a Python script that would convert the data into an XML format that could be read into the PHOIBLE MySQL database.

The SQL query below may be relevant for the new aggregate.R script.

NOTES: UPSID_IPA_correspondences.csv is an expanded UPSID_CharCodes.csv (also in repo, fyi).

=============================================================

% UPSID TRANSFORMATION - LATEST

1. UPLOAD UPSID DATA to DB
	-UPSID tables (if they aren't already uploaded)
	-UPSID language name to language code (spreadsheet xls)

	SORT THEM!

2. RUN MYSQL QUERY UPSID_langname_segments

then the shit's not sorted...?

SELECT UPSID_LanguageCodes.LanguageCode, 
	UPSID_Languages.LangName, 
	UPSID_CharCodes.IPA
FROM UPSID_Languages INNER JOIN UPSID_Segments ON UPSID_Languages.LangNum = UPSID_Segments.LangNum
	 INNER JOIN UPSID_CharCodes ON UPSID_Segments.CCID = UPSID_CharCodes.CCID
	 INNER JOIN UPSID_LanguageCodes ON UPSID_Languages.LangName = UPSID_LanguageCodes.LangName
ORDER BY LanguageCode	
/* WHERE UPSID_LanguageCodes.LanguageCode = 'acv' */

which produces table:

LanguageCode	LangName	IPA
acv	ACHUMAWI	p
acv	ACHUMAWI	t

### apparently you have to get rid of the header!

Save to disk (sequel pro names it "query_result.tsv" - make sure to save it to the right folder!)


3. TreeNode.optimized.py OUTPUT_UPSID.csv > output.xml




** old stuff below

gets table:

LanguageCode	LangName	IPA


this table is output as OUTPUT_UPSID.csv



TreeNode.optimized.py

input: UPSID_langname_segments_IPA.txt
output: XML for PHOIBLE database


# tab output from the database
UPSID_langname_segments_IPA.txt

what the fuuuck do we do about /R/? (for now i just put the character into the IPA list)

509	R		rr	voiced alveolar r-sound

what do we these symbols?

517	ɺ̢		l.[	voiced retroflex lateral flap	
516	ɺ̢̰		l.[*	laryngealized voiced retroflex lateral flap	























% UPSID TRANSFORMATION

1. UPLOAD UPSID DATA to DB
	-UPSID tables
	-UPSID language name to language code 

1. RUN MYSQL QUERY UPSID_langname_segments

SELECT UPSID_Languages.LangName, 
	UPSID_Segments.CCID
FROM UPSID_Languages INNER JOIN UPSID_Segments ON UPSID_Languages.LangNum = UPSID_Segments.LangNum

gets table:

LanguageCode	LangName	LangNum	IPA


this table is output as OUTPUT_UPSID.xls


input for UPSID2IPA.py



LangName	CCID	
GREEK		26
GREEK		30


output OUTPUT_UPSID.xls

run that file in NeoOffice

save as csv 

this part does the unicode conversion?

UPSID2IPA.py
-

UPSID_IPA_correspondences.csv:
CCID	IPA	Notes	CharCode	Description




TreeNode.optimized.py

input: UPSID_langname_segments_IPA.txt
output: XML for PHOIBLE database


# tab output from the database
UPSID_langname_segments_IPA.txt

what the fuuuck do we do about /R/? (for now i just put the character into the IPA list)

509	R		rr	voiced alveolar r-sound

what do we these symbols?

517	ɺ̢		l.[	voiced retroflex lateral flap	
516	ɺ̢̰		l.[*	laryngealized voiced retroflex lateral flap	
