use crate::values::Oid;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BasicEncodingKind {
    Basic,
    Canonical,
    Distinguished,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PackedEncodingKind {
    BasicAligned,
    BasicUnaligned,
    CanonicalAligned,
    CanonicalUnaligned,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum XmlEncodingKind {
    Basic,
    Canonical,
    Extended,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OctetEncodingKind {
    Basic,
    Canonical,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TransferSyntax {
    Basic(BasicEncodingKind),
    Packed(PackedEncodingKind),
    Xml(XmlEncodingKind),
    Octet(OctetEncodingKind),
}

lazy_static::lazy_static! {
    static ref TRANFER_SYNTAX_OIDS: Vec<(TransferSyntax, Oid)> = vec![
        (
            TransferSyntax::Basic(BasicEncodingKind::Basic),
            Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                1, // basic-encoding
            ]),
        ),
        (
            TransferSyntax::Basic(BasicEncodingKind::Canonical),
            Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                2, // ber-derived
                0, // canonical-encoding
            ]),
        ),
        (
            TransferSyntax::Basic(BasicEncodingKind::Distinguished),
            Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                2, // ber-derived
                1, // distinguished-encoding
            ]),
        ),
        (
            TransferSyntax::Packed(PackedEncodingKind::BasicAligned),
            Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                3, // packed-encoding
                0, // basic
                0, // aligned
            ]),
        ),
        (
            TransferSyntax::Packed(PackedEncodingKind::BasicUnaligned),
            Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                3, // packed-encoding
                0, // basic
                1, // unaligned
            ]),
        ),
        (
            TransferSyntax::Packed(PackedEncodingKind::CanonicalAligned),
            Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                3, // packed-encoding
                1, // canonical
                0, // aligned
            ]),
        ),
        (
            TransferSyntax::Packed(PackedEncodingKind::CanonicalUnaligned),
            Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                3, // packed-encoding
                1, // canonical
                1, // unaligned
            ]),
        ),
        (
            TransferSyntax::Xml(XmlEncodingKind::Basic),
            Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                5, // xer-encoding
                0, // basic
            ]),
        ),
        (
            TransferSyntax::Xml(XmlEncodingKind::Canonical),
            Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                5, // xer-encoding
                1, // canonical
            ]),
        ),
        (
            TransferSyntax::Xml(XmlEncodingKind::Extended),
            Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                5, // xer-encoding
                2, // extended
            ]),
        ),
        (
            TransferSyntax::Octet(OctetEncodingKind::Basic),
            Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                6, // oer-encoding
                0, // basic
            ]),
        ),
        (
            TransferSyntax::Octet(OctetEncodingKind::Canonical),
            Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                6, // oer-encoding
                1, // canonical
            ]),
        ),
    ];
}

impl TransferSyntax {
    pub fn get_by_oid(oid: &Oid) -> Option<Self> {
        for (ts, ts_oid) in TRANFER_SYNTAX_OIDS.iter() {
            if oid == ts_oid {
                return Some(ts.clone());
            }
        }

        None
    }

    pub fn get_oid(&self) -> Oid {
        for (ts, ts_oid) in TRANFER_SYNTAX_OIDS.iter() {
            if self == ts {
                return ts_oid.clone();
            }
        }

        unreachable!();
    }

    pub fn name(&self) -> &'static str {
        match self {
            Self::Basic(kind) => match kind {
                BasicEncodingKind::Basic => "BER",
                BasicEncodingKind::Canonical => "CER",
                BasicEncodingKind::Distinguished => "DER",
            },
            Self::Packed(kind) => match kind {
                PackedEncodingKind::BasicAligned => "PER",
                PackedEncodingKind::BasicUnaligned => "UPER",
                PackedEncodingKind::CanonicalAligned => "CPER",
                PackedEncodingKind::CanonicalUnaligned => "CUPER",
            },
            Self::Xml(kind) => match kind {
                XmlEncodingKind::Basic => "XER",
                XmlEncodingKind::Canonical => "CXER",
                XmlEncodingKind::Extended => "E-XER",
            },
            Self::Octet(kind) => match kind {
                OctetEncodingKind::Basic => "OER",
                OctetEncodingKind::Canonical => "COER",
            },
        }
    }

    /// True if asn1chef currently supports this transfer syntax, otherwise false.
    pub fn is_implemented(&self) -> bool {
        match self {
            Self::Basic(kind) => match kind {
                BasicEncodingKind::Distinguished => true,
                _ => false,
            },
            _ => false,
        }
    }
}
