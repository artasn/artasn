Example2-ELM {joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) elm-module2(4)}
LINK-DEFINITIONS ::=
BEGIN
IMPORTS
Example2Encodings FROM Example2-EDM
        {joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) edm-module2(6)}
        #ExampleMessages FROM Example2-ASN1-Module
        {joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) asn1-module2(5)};

        ENCODE #ExampleMessages WITH Example2Encodings
        COMPLETED BY PER-BASIC-UNALIGNED
END

