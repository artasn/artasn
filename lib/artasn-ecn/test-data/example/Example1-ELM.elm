Example1-ELM {joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) elm-module1(1)} 
LINK-DEFINITIONS ::=
BEGIN

IMPORTS
        Example1Encodings FROM Example-EDM
        {joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) edm-module1(3)}
        #MyPDU,#Sequence2 FROM Example1-ASN1-Module
        {joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) asn1-module1(2)};

ENCODE #MyPDU WITH Example1Encodings
        COMPLETED BY PER-BASIC-UNALIGNED

END
