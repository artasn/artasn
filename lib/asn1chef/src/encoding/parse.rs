use std::io;

use num::{bigint::Sign, BigInt, BigUint};

use super::{read_vlq, DerReader, Tlv, TlvElement, TlvPos, TlvTag};
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
                            TagType::NumericString | TagType::PrintableString => {
                                let str = String::from_utf8(value.to_vec()).map_err(|err| {
                                    io::Error::new(io::ErrorKind::InvalidData, err)
                                })?;
                                DecodedValueKind::PrintableString(str)
                            }
                            TagType::Sequence | TagType::Set => {
                                return Err(io::Error::new(
                                    io::ErrorKind::InvalidData,
                                    format!("{} must have the Constructed bit set", tag_type),
                                ));
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
pub enum DecodedValueKind {
    Raw(Vec<u8>),
    Boolean(bool),
    Integer(BigInt),
    BitString(BigUint),
    OctetString(Vec<u8>),
    Null,
    ObjectIdentifier(Oid),
    Real(f64),
    PrintableString(String),
}
