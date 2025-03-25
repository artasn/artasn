Example6-ELM {joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) elm-module6(17)}
LINK-DEFINITIONS ::=
BEGIN
IMPORTS #My-Special-1, #My-Special-2, #My-Special-3
	FROM Example6-EDM
	{joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) asn1-module6(18)}
	Example6Encodings
	FROM Example6Encodings
	{joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) edm-module6(19)};

	ENCODE #My-Special-1, #My-Special-2, #My-Special-3
	WITH Example6Encodings
	COMPLETED BY PER-BASIC-UNALIGNED

END

