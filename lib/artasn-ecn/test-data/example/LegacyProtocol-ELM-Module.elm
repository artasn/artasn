LegacyProtocol-ELM-Module 
	{ joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) elm-module4(12) }
LINK-DEFINITIONS ::=
BEGIN
IMPORTS
LegacyProtocolEncodings FROM LegacyProtocol-EDM-Module
        { joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) edm-module4(13) }
#LegacyProtocolMessages FROM LegacyProtocol-ASN1-Module
        { joint-iso-itu-t(2) asn1(1) ecn(4) examples(5) asn1-module4(11) };

ENCODE #LegacyProtocolMessages WITH LegacyProtocolEncodings
COMPLETED BY PER-BASIC-UNALIGNED

END

