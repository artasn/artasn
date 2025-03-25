Example3-ELM {joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) elm-module3(8)}
LINK-DEFINITIONS ::=
BEGIN

IMPORTS Example3Encodings-1, Example3Encodings-2,
	#Sequence1, #Sequence2, #Octet3, #Sequence3, #SequenceOfIntegers
	FROM Example3-EDM { joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) 
	edm-module3(10) };

ENCODE 	#Sequence1
	WITH Example3Encodings-1
	COMPLETED BY PER-BASIC-UNALIGNED

ENCODE  #Sequence2, #Octet3, #SequenceOfIntegers, #Sequence3
	WITH Example3Encodings-2
	COMPLETED BY PER-BASIC-UNALIGNED

END

