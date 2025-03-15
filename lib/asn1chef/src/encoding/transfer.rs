use super::*;
use crate::{
    compiler::{parser::Result, Context},
    types::ResolvedType,
    values::{BuiltinValue, Oid},
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BasicEncodingKind {
    Basic,
    Canonical,
    Distinguished,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PackedEncodingKind {
    BasicAligned,
    BasicUnaligned,
    CanonicalAligned,
    CanonicalUnaligned,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum XmlEncodingKind {
    Basic,
    Canonical,
    Extended,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

type EncodeFunc = fn(
    syntax: &TransferSyntax,
    buf: &mut Vec<u8>,
    context: &Context,
    value: &BuiltinValue,
    value_type: &ResolvedType,
) -> Result<()>;
type DecodeFunc = fn(
    syntax: &TransferSyntax,
    buf: &[u8],
    context: &Context,
    mode: &DecodeMode,
) -> DecodeResult<Vec<DecodedValue>>;

pub struct TransferSyntaxCodec {
    pub encoder: Option<EncodeFunc>,
    pub decoder: Option<DecodeFunc>,
}

impl TransferSyntaxCodec {
    pub fn new(encoder: EncodeFunc, decoder: DecodeFunc) -> TransferSyntaxCodec {
        TransferSyntaxCodec {
            encoder: Some(encoder),
            decoder: Some(decoder),
        }
    }

    pub fn unsupported() -> TransferSyntaxCodec {
        TransferSyntaxCodec {
            encoder: None,
            decoder: None,
        }
    }
}

struct TransferSyntaxData {
    pub syntax: TransferSyntax,
    pub oid: Oid,
    pub name: &'static str,
    pub codec: TransferSyntaxCodec,
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
            codec: TransferSyntaxCodec {
                encoder: Some(ber_encode_value),
                decoder: None,
            },
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
            codec: TransferSyntaxCodec {
                encoder: Some(ber_encode_value),
                decoder: None,
            },
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
            codec: TransferSyntaxCodec::new(ber_encode_value, ber_decode_value),
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
            codec: TransferSyntaxCodec::unsupported(),
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
            codec: TransferSyntaxCodec::unsupported(),
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
            codec: TransferSyntaxCodec::unsupported(),
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
            codec: TransferSyntaxCodec::unsupported(),
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
            codec: TransferSyntaxCodec::unsupported(),
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
            codec: TransferSyntaxCodec::unsupported(),
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
            codec: TransferSyntaxCodec::unsupported(),
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
            codec: TransferSyntaxCodec::unsupported(),
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
            codec: TransferSyntaxCodec::unsupported(),
        },
    ];
}

macro_rules! find_data_field {
    ( $self:expr, $field:ident ) => {
        for data in TRANFER_SYNTAXES.iter() {
            if $self == &data.syntax {
                return &data.$field;
            }
        }

        unreachable!()
    };
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

    pub fn get_by_name<'a>(name: &str) -> Option<&'a TransferSyntax> {
        for data in TRANFER_SYNTAXES.iter() {
            if name == data.name {
                return Some(&data.syntax);
            }
        }

        None
    }

    pub fn get_oid<'a>(&self) -> &'a Oid {
        find_data_field!(self, oid);
    }

    pub fn get_name(&self) -> &'static str {
        find_data_field!(self, name);
    }

    pub fn get_codec<'a>(&self) -> &'a TransferSyntaxCodec {
        find_data_field!(self, codec);
    }
}

fn ber_encode_value(
    syntax: &TransferSyntax,
    buf: &mut Vec<u8>,
    context: &Context,
    value: &BuiltinValue,
    value_type: &ResolvedType,
) -> Result<()> {
    match syntax {
        TransferSyntax::Basic(_) => (),
        other => panic!("illegal TransferSyntax (expecting Basic): {:?}", other),
    };
    ber::ber_encode_value(buf, context, value, value_type)
}

fn ber_decode_value(
    syntax: &TransferSyntax,
    buf: &[u8],
    context: &Context,
    mode: &DecodeMode,
) -> DecodeResult<Vec<DecodedValue>> {
    let kind = match syntax {
        TransferSyntax::Basic(kind) => *kind,
        other => panic!("illegal TransferSyntax (expecting Basic): {:?}", other),
    };
    ber::ber_decode_value(kind, buf, context, mode)
}
