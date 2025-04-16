use std::io;

use num::BigInt;

use super::*;
use crate::{compiler::parser, module::QualifiedIdentifier, types::*, values::*};

#[derive(Debug)]
pub enum DecodedValueForm {
    Primitive(DecodedValueKind),
    Constructed(Vec<DecodedValue>),
}

#[derive(Debug)]
pub struct DecodedValueMetadata {
    pub type_ident: UntaggedType,
    pub component_name: Option<String>,
}

#[derive(Debug)]
pub struct DecodedValue {
    pub tag: TlvElement<TlvTag>,
    pub len: TlvElement<u32>,
    pub value_pos: TlvPos,
    pub form: DecodedValueForm,
    pub metadata: Option<DecodedValueMetadata>,
}

#[derive(Debug)]
pub enum DecodeMode {
    Contextless,
    SpecificType {
        source_ident: Option<QualifiedIdentifier>,
        component_name: Option<String>,
        resolved: ResolvedType,
    },
}

#[derive(Debug)]
pub enum DecodeError {
    Io(io::Error),
    Parser(parser::Error),
    Decoder { message: String, pos: TlvPos },
}

impl Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(err) => f.write_fmt(format_args!("io error: {}", err)),
            Self::Parser(err) => f.write_fmt(format_args!("parser error: {}", err.kind.message())),
            Self::Decoder { message, pos } => f.write_fmt(format_args!(
                "decoder error: {} at bytes {}+{}",
                message,
                pos.start,
                pos.end - pos.start
            )),
        }
    }
}

pub type DecodeResult<T> = Result<T, DecodeError>;

#[derive(Debug)]
pub enum DecodedValueKind {
    Raw(Vec<u8>),
    Boolean(bool),
    Integer(BigInt),
    BitString(BitStringValue),
    OctetString(Vec<u8>),
    Null,
    ObjectIdentifier(Oid),
    Real(f64),
    Enumerated(DecodedEnumerationItem),
    Time(Time),
    CharacterString(TagType, String),
    UTCTime(UTCTime),
    Date(Date),
    TimeOfDay(TimeOfDay),
    DateTime(DateTime),
    Duration(Duration),
}

#[derive(Debug)]
pub struct DecodedEnumerationItem {
    pub item: Option<String>,
    pub value: i64,
}
