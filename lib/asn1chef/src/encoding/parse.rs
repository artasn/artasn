use std::io;

use num::{bigint::Sign, BigInt, BigUint};
use widestring::Utf32String;

use super::{read_vlq, strings, DerReader, Tlv, TlvElement, TlvPos, TlvTag};
use crate::{
    types::{Class, TagType, TypeForm},
    values::Oid,
};

#[derive(Debug)]
pub enum DecodedValueForm {
    Primitive(DecodedValueKind),
    Constructed(Vec<DecodedValue>),
}

#[derive(Debug)]
pub struct DecodedValue {
    pub tag: TlvElement<TlvTag>,
    pub len: TlvElement<u32>,
    pub value_pos: TlvPos,
    pub form: DecodedValueForm,
}

impl DecodedValue {
    pub fn der_decode(tlv: Tlv<'_>) -> io::Result<DecodedValue> {
        let value = tlv.value.element;
        let form = match tlv.tag.element.form {
            TypeForm::Primitive => {
                let kind = match tlv.tag.element.class {
                    Class::Universal => match TagType::try_from(tlv.tag.element.num) {
                        Ok(tag_type) => match tag_type {
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
                                        | TagType::VisibleString => {
                                            strings::T50_MAP.decode_bytes(value).ok_or(err)?
                                        }
                                        TagType::TeletexString => {
                                            strings::T61_MAP.decode_bytes(value).ok_or(err)?
                                        }
                                        TagType::VideotexString => {
                                            strings::T100_MAP.decode_bytes(value).ok_or(err)?
                                        }
                                        TagType::GeneralString
                                        | TagType::GraphicString
                                        | TagType::UTF8String
                                        | TagType::BMPString
                                        | TagType::CharacterString => {
                                            String::from_utf8(value.to_vec()).map_err(|err| {
                                                io::Error::new(io::ErrorKind::InvalidData, err)
                                            })?
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
                                                .map_err(|err| {
                                                    io::Error::new(io::ErrorKind::InvalidData, err)
                                                })?
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
                                                minute: TwoDigitInteger::from_chars(
                                                    &value[value.len() - 2..],
                                                )?,
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
                        },
                        Err(_) => DecodedValueKind::Raw(value.to_vec()),
                    },
                    _ => DecodedValueKind::Raw(value.to_vec()),
                };
                DecodedValueForm::Primitive(kind)
            }
            TypeForm::Constructed => {
                let elements = DerReader::new(value, tlv.value.pos.start)
                    .into_iter()
                    .map(|tlv| Self::der_decode(tlv?))
                    .collect::<io::Result<Vec<DecodedValue>>>()?;
                DecodedValueForm::Constructed(elements)
            }
        };

        Ok(DecodedValue {
            tag: tlv.tag,
            len: TlvElement::new(value.len() as u32, tlv.len_pos),
            value_pos: tlv.value.pos,
            form,
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
