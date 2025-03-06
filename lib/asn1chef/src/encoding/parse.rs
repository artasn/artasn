use std::io;

use num::{bigint::Sign, BigInt, BigUint};
use widestring::{Utf16String, Utf32String};

use super::*;
use crate::{compiler::parser, module::QualifiedIdentifier, types::*, values::Oid};

#[derive(Debug)]
pub enum DecodedValueForm {
    Primitive(DecodedValueKind),
    Constructed(Vec<DecodedValue>),
}

#[derive(Debug)]
pub struct DecodedValueMetadata {
    pub type_ident: Option<QualifiedIdentifier>,
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

fn der_decode_universal(tlv: &Tlv<'_>, tag_type: TagType) -> io::Result<DecodedValueKind> {
    let value = tlv.value.element;
    Ok(match tag_type {
        TagType::Any => DecodedValueKind::Raw(value.to_vec()),
        TagType::Boolean => {
            if value.len() == 0 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "BOOLEAN must have a value",
                ));
            } else if value.len() > 1 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "BOOLEAN is not in canonical format",
                ));
            }

            DecodedValueKind::Boolean(value[0] != 0)
        }
        TagType::Integer => {
            const SIGN_MASK: u8 = 0b1000_0000;

            if value.len() == 0 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "INTEGER must have a value",
                ));
            }

            let sign = if value[0] & SIGN_MASK == SIGN_MASK {
                Sign::Minus
            } else {
                Sign::Plus
            };

            DecodedValueKind::Integer(BigInt::from_bytes_be(sign, value))
        }
        TagType::BitString => {
            if value.len() < 2 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "BIT STRING must have a value",
                ));
            }

            let unused_bits = value[0];
            if unused_bits != 0 {
                todo!("unimplemented: BIT STRING unused bits");
            }

            let be_bits = &value[1..];

            DecodedValueKind::BitString(BigUint::from_bytes_be(be_bits))
        }
        TagType::OctetString => DecodedValueKind::OctetString(value.to_vec()),
        TagType::Null => {
            if value.len() != 0 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "NULL cannot have a value",
                ));
            }

            DecodedValueKind::Null
        }
        TagType::ObjectIdentifier => {
            let mut nodes = Vec::new();
            let mut offset = 0;
            let (first_node, len) = read_vlq(&value)?;
            offset += len;
            if first_node >= 80 {
                nodes.push(2);
                nodes.push(first_node - 80);
            } else {
                nodes.push(first_node / 40);
                nodes.push(first_node % 40);
            }
            while offset < value.len() {
                let (node, len) = read_vlq(&value[offset..])?;
                offset += len;
                nodes.push(node);
            }
            DecodedValueKind::ObjectIdentifier(Oid(nodes))
        }
        TagType::NumericString
        | TagType::PrintableString
        | TagType::IA5String
        | TagType::VisibleString
        | TagType::TeletexString
        | TagType::VideotexString
        | TagType::GeneralString
        | TagType::GraphicString
        | TagType::UTF8String
        | TagType::BMPString
        | TagType::CharacterString
        | TagType::UniversalString => {
            let str = if value.len() == 0 {
                String::new()
            } else {
                let err = io::Error::new(
                    io::ErrorKind::InvalidData,
                    "string contains bytes not in character set",
                );
                match tag_type {
                    TagType::NumericString
                    | TagType::PrintableString
                    | TagType::IA5String
                    | TagType::VisibleString => strings::T50_MAP.decode_bytes(value).ok_or(err)?,
                    TagType::TeletexString => strings::T61_MAP.decode_bytes(value).ok_or(err)?,
                    TagType::VideotexString => strings::T100_MAP.decode_bytes(value).ok_or(err)?,
                    TagType::GeneralString
                    | TagType::GraphicString
                    | TagType::UTF8String
                    | TagType::CharacterString => String::from_utf8(value.to_vec())
                        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?,
                    TagType::BMPString => {
                        if value.len() % 2 != 0 {
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                "UniversalString is not properly UTF-16 encoded",
                            ));
                        }
                        let mut buf = Vec::with_capacity(value.len() / 2);
                        for i in (0..value.len()).step_by(2) {
                            let mut be_bytes = [0u8; 2];
                            be_bytes.copy_from_slice(&value[i..i + 2]);
                            buf.push(u16::from_be_bytes(be_bytes));
                        }
                        Utf16String::from_vec(buf)
                            .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?
                            .to_string()
                    }
                    TagType::UniversalString => {
                        if value.len() % 4 != 0 {
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                "UniversalString is not properly UTF-32 encoded",
                            ));
                        }
                        let mut buf = Vec::with_capacity(value.len() / 4);
                        for i in (0..value.len()).step_by(4) {
                            let mut be_bytes = [0u8; 4];
                            be_bytes.copy_from_slice(&value[i..i + 4]);
                            buf.push(u32::from_be_bytes(be_bytes));
                        }
                        Utf32String::from_vec(buf)
                            .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?
                            .to_string()
                    }
                    _ => unreachable!(),
                }
            };
            DecodedValueKind::CharacterString(tag_type, str)
        }
        TagType::Sequence | TagType::Set => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("{} must have the Constructed bit set", tag_type),
            ));
        }
        TagType::UTCTime => {
            if value.len() == 0 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "UTCTime must have a value",
                ));
            }

            // shorted possible value is YYMMDDhhmmZ
            if value.len() < 11 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "UTCTime value is too short",
                ));
            }

            let (time, tz) = if value[value.len() - 1] == b'Z' {
                (&value[..value.len() - 1], UTCTimeZone::Z)
            } else {
                let tz_sign = value[value.len() - 5];
                if tz_sign == b'+' || tz_sign == b'-' {
                    (
                        &value[..value.len() - 5],
                        UTCTimeZone::Offset {
                            sign: match tz_sign {
                                b'+' => UTCTimeZoneSign::Plus,
                                b'-' => UTCTimeZoneSign::Minus,
                                _ => unreachable!(),
                            },
                            hour: TwoDigitInteger::from_chars(
                                &value[value.len() - 4..value.len() - 2],
                            )?,
                            minute: TwoDigitInteger::from_chars(&value[value.len() - 2..])?,
                        },
                    )
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "UTCTime time zone is malformed",
                    ));
                }
            };

            if time.len() != 10 && time.len() != 12 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "UTCTime is malformed (invalid length)",
                ));
            }
            let year = TwoDigitInteger::from_chars(&time[..2])?;
            let month = TwoDigitInteger::from_chars(&time[2..4])?;
            let day = TwoDigitInteger::from_chars(&time[4..6])?;
            let hour = TwoDigitInteger::from_chars(&time[6..8])?;
            let minute = TwoDigitInteger::from_chars(&time[8..10])?;
            let second = match time.len() {
                10 => None,
                12 => Some(TwoDigitInteger::from_chars(&time[10..])?),
                _ => unreachable!(),
            };

            DecodedValueKind::UTCTime {
                year,
                month,
                day,
                hour,
                minute,
                second,
                tz,
            }
        }
        other => todo!("decode {:?}", other),
    })
}

fn tag_eq_tlv(tag: &Tag, tlv_tag: &TlvTag) -> bool {
    tag.class == tlv_tag.class && tag.num == tlv_tag.num
}

struct ComponentData<'a> {
    pub name: Option<String>,
    pub tagged_type: &'a TaggedType,
    pub index: usize,
}

fn get_component_by_tag<'a>(
    mode: &'a DecodeMode,
    tlv_tag: &TlvElement<TlvTag>,
    index: usize,
) -> DecodeResult<Option<ComponentData<'a>>> {
    // TODO: implement SET
    Ok(match mode {
        DecodeMode::Contextless => None,
        DecodeMode::SpecificType { resolved, .. } => match &resolved.ty {
            BuiltinType::Structure(structure) => {
                for (component_index, component) in
                    structure.components.iter().enumerate().skip(index)
                {
                    let component_type = component
                        .component_type
                        .resolve()
                        .map_err(|err| DecodeError::Parser(err))?;

                    let tag_matches = tag_eq_tlv(&component_type.tag, &tlv_tag.element);
                    if !tag_matches {
                        if component.optional || component.default_value.is_some() {
                            continue;
                        } else {
                            return Err(DecodeError::Decoder {
                            message: format!("SEQUENCE component at index {} is defined with tag {} but the encoded value has tag {}", component_index, component_type.tag, tlv_tag.element),
                            pos: tlv_tag.pos,
                        });
                        }
                    }

                    let data = ComponentData {
                        name: Some(component.name.element.clone()),
                        tagged_type: &component.component_type,
                        index: component_index,
                    };
                    if tag_matches {
                        return Ok(Some(data));
                    }
                }
                None
            }
            BuiltinType::StructureOf(of) => Some(ComponentData {
                name: None,
                tagged_type: &of.component_type,
                index,
            }),
            _ => None,
        },
    })
}

impl DecodedValue {
    pub fn der_decode(tlv: Tlv<'_>, mode: &DecodeMode) -> DecodeResult<DecodedValue> {
        let form = match tlv.tag.element.form {
            TypeForm::Primitive => {
                let kind = match tlv.tag.element.class {
                    Class::Universal => match TagType::try_from(tlv.tag.element.num) {
                        Ok(tag_type) => der_decode_universal(&tlv, tag_type)
                            .map_err(|err| DecodeError::Io(err))?,
                        Err(_) => DecodedValueKind::Raw(tlv.value.element.to_vec()),
                    },
                    _ => match &mode {
                        DecodeMode::Contextless => {
                            DecodedValueKind::Raw(tlv.value.element.to_vec())
                        }
                        DecodeMode::SpecificType { resolved, .. } => {
                            if tag_eq_tlv(&resolved.tag, &tlv.tag.element) {
                                // we use resolved.ty.tag_type() here to get the UNIVERSAL tag type for the underlying builtin type, not the user-defined tag
                                der_decode_universal(
                                    &tlv,
                                    resolved.ty.tag_type().expect("tag_type"),
                                )
                                .map_err(|err| DecodeError::Io(err))?
                            } else {
                                return Err(DecodeError::Decoder {
                                    message: format!("DecodeMode is SpecificType but encoded tag {} does not match provided tag {}", tlv.tag.element, resolved.tag),
                                    pos: tlv.tag.pos,
                                });
                            }
                        }
                    },
                };
                DecodedValueForm::Primitive(kind)
            }
            TypeForm::Constructed => {
                let mut index = 0;
                let mut elements = Vec::new();
                for tlv in DerReader::new(tlv.value.element, tlv.value.pos.start) {
                    let tlv = tlv.map_err(|err| DecodeError::Io(err))?;
                    let component = get_component_by_tag(&mode, &tlv.tag, index)?;
                    let mode = match component {
                        Some(data) => {
                            index = data.index + 1;
                            DecodeMode::SpecificType {
                                source_ident: match &data.tagged_type.ty {
                                    UntaggedType::Reference(typeref) => {
                                        Some(typeref.element.clone())
                                    }
                                    UntaggedType::BuiltinType(_) => None,
                                },
                                component_name: data.name,
                                resolved: data
                                    .tagged_type
                                    .resolve()
                                    .map_err(|err| DecodeError::Parser(err))?,
                            }
                        }
                        None => {
                            index += 1;
                            DecodeMode::Contextless
                        }
                    };
                    elements.push(Self::der_decode(tlv, &mode)?);
                }
                DecodedValueForm::Constructed(elements)
            }
        };
        let metadata = match mode {
            DecodeMode::Contextless => None,
            DecodeMode::SpecificType {
                source_ident,
                component_name,
                ..
            } => Some(DecodedValueMetadata {
                type_ident: source_ident.clone(),
                component_name: component_name.clone(),
            }),
        };

        Ok(DecodedValue {
            tag: tlv.tag,
            len: TlvElement::new(tlv.value.element.len() as u32, tlv.len_pos),
            value_pos: tlv.value.pos,
            form,
            metadata,
        })
    }
}

#[derive(Debug)]
pub struct TwoDigitInteger(pub u8);

impl TwoDigitInteger {
    pub fn from_chars(chars: &[u8]) -> io::Result<TwoDigitInteger> {
        if chars.len() != 2 {
            panic!("TwoDigitInteger::from_chars: len == {}", chars.len());
        }
        let msd = chars[0];
        let lsd = chars[1];
        if !(msd as char).is_ascii_digit() || !(lsd as char).is_ascii_digit() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("invalid two-digit integer: {}{}", msd as char, lsd as char),
            ));
        }
        Ok(TwoDigitInteger((msd - b'0') * 10 + (lsd - b'0')))
    }
}

#[derive(Debug)]
pub enum UTCTimeZoneSign {
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum UTCTimeZone {
    Z,
    Offset {
        sign: UTCTimeZoneSign,
        hour: TwoDigitInteger,
        minute: TwoDigitInteger,
    },
}

#[derive(Debug)]
pub enum DecodedValueKind {
    Raw(Vec<u8>),
    Boolean(bool),
    Integer(BigInt),
    BitString(BigUint),
    OctetString(Vec<u8>),
    Null,
    ObjectIdentifier(Oid),
    Real(f64),
    CharacterString(TagType, String),
    UTCTime {
        year: TwoDigitInteger,
        month: TwoDigitInteger,
        day: TwoDigitInteger,
        hour: TwoDigitInteger,
        minute: TwoDigitInteger,
        second: Option<TwoDigitInteger>,
        tz: UTCTimeZone,
    },
}
