Example4-ELM {joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) elm-module4(14)}
LINK-DEFINITIONS ::=
BEGIN

IMPORTS Example4Encodings
	FROM Example4-EDM { joint-iso-itu-t(2) asn1(1) ecn(4) examples(5)
	edm-module4(16)}
	#ProfileIndication, #ProfileIndication2
	FROM Example4-EDM { joint-iso-itu-t(2) asn1(1) ecn(4) examples(5)
	asn1-module4(15)};

ENCODE #ProfileIndication, #ProfileIndication2
	WITH Example4Encodings
	COMPLETED BY PER-BASIC-UNALIGNED

END

