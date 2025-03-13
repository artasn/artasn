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

pub struct TransferSyntaxSupport {
    /// True if encoding values in the transfer syntax is supported by asn1chef.
    pub encode: bool,
    /// True if decoding values in the transfer syntax is supported by asn1chef.
    pub decode: bool,
}

impl TransferSyntaxSupport {
    pub fn new(encode: bool, decode: bool) -> TransferSyntaxSupport {
        TransferSyntaxSupport { encode, decode }
    }
}

struct TransferSyntaxData {
    pub syntax: TransferSyntax,
    pub oid: Oid,
    pub name: &'static str,
}

lazy_static::lazy_static! {
    static ref TRANFER_SYNTAXES: Vec<TransferSyntaxData> = vec![
        TransferSyntaxData {
            syntax: TransferSyntax::Basic(BasicEncodingKind::Basic),
            oid: Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                1, // basic-encoding
            ]),
            name: "BER",
        },
        TransferSyntaxData {
            syntax: TransferSyntax::Basic(BasicEncodingKind::Canonical),
            oid: Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                2, // ber-derived
                0, // canonical-encoding
            ]),
            name: "CER",
        },
        TransferSyntaxData {
            syntax: TransferSyntax::Basic(BasicEncodingKind::Distinguished),
            oid: Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                2, // ber-derived
                1, // distinguished-encoding
            ]),
            name: "DER",
        },
        TransferSyntaxData {
            syntax: TransferSyntax::Packed(PackedEncodingKind::BasicAligned),
            oid: Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                3, // packed-encoding
                0, // basic
                0, // aligned
            ]),
            name: "PER",
        },
        TransferSyntaxData {
            syntax: TransferSyntax::Packed(PackedEncodingKind::BasicUnaligned),
            oid: Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                3, // packed-encoding
                0, // basic
                1, // unaligned
            ]),
            name: "UPER",
        },
        TransferSyntaxData {
            syntax: TransferSyntax::Packed(PackedEncodingKind::CanonicalAligned),
            oid: Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                3, // packed-encoding
                1, // canonical
                0, // aligned
            ]),
            name: "CPER",
        },
        TransferSyntaxData {
            syntax: TransferSyntax::Packed(PackedEncodingKind::CanonicalUnaligned),
            oid: Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                3, // packed-encoding
                1, // canonical
                1, // unaligned
            ]),
            name: "CUPER",
        },
        TransferSyntaxData {
            syntax: TransferSyntax::Xml(XmlEncodingKind::Basic),
            oid: Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                5, // xer-encoding
                0, // basic
            ]),
            name: "XER",
        },
        TransferSyntaxData {
            syntax: TransferSyntax::Xml(XmlEncodingKind::Canonical),
            oid: Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                5, // xer-encoding
                1, // canonical
            ]),
            name: "CXER",
        },
        TransferSyntaxData {
            syntax: TransferSyntax::Xml(XmlEncodingKind::Extended),
            oid: Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                5, // xer-encoding
                2, // extended
            ]),
            name: "E-XER",
        },
        TransferSyntaxData {
            syntax: TransferSyntax::Octet(OctetEncodingKind::Basic),
            oid: Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                6, // oer-encoding
                0, // basic
            ]),
            name: "OER",
        },
        TransferSyntaxData {
            syntax: TransferSyntax::Octet(OctetEncodingKind::Canonical),
            oid: Oid(vec![
                2, // joint-iso-itu-t
                1, // asn1
                6, // oer-encoding
                1, // canonical
            ]),
            name: "COER",
        },
    ];
}

impl TransferSyntax {
    pub fn syntaxes<'a>() -> Vec<&'a TransferSyntax> {
        let mut syntaxes = Vec::with_capacity(TRANFER_SYNTAXES.len());
        for data in TRANFER_SYNTAXES.iter() {
            syntaxes.push(&data.syntax);
        }
        syntaxes
    }

    pub fn get_by_oid<'a>(oid: &Oid) -> Option<&'a TransferSyntax> {
        for data in TRANFER_SYNTAXES.iter() {
            if oid == &data.oid {
                return Some(&data.syntax);
            }
        }

        None
    }

    pub fn get_oid<'a>(&self) -> &'a Oid {
        for data in TRANFER_SYNTAXES.iter() {
            if self == &data.syntax {
                return &data.oid;
            }
        }

        unreachable!()
    }

    pub fn name(&self) -> &'static str {
        for data in TRANFER_SYNTAXES.iter() {
            if self == &data.syntax {
                return data.name;
            }
        }

        unreachable!()
    }

    pub fn get_support(&self) -> TransferSyntaxSupport {
        match self {
            Self::Basic(kind) => match kind {
                BasicEncodingKind::Distinguished => TransferSyntaxSupport::new(true, true),
                _ => TransferSyntaxSupport::new(true, false),
            },
            _ => TransferSyntaxSupport::new(false, false),
        }
    }
}
