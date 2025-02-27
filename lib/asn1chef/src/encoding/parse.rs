use std::io;

use num::BigUint;

use super::{read_vlq, DerReader, Tlv, TlvElement, TlvPos, TlvTag};
use crate::{
    types::{Class, TagType},
    values::Oid,
};

pub struct DecodedValue {
    pub tag: TlvElement<TlvTag>,
    pub len: TlvElement<u32>,
    pub value_pos: TlvPos,
    pub kind: DecodedValueKind,
}

impl DecodedValue {
    pub fn der_decode(tlv: Tlv<'_>) -> io::Result<DecodedValue> {
        let value = tlv.value.element;
        let kind = match tlv.tag.element.class {
            Class::Universal => match TagType::try_from(tlv.tag.element.num) {
                Ok(tag_type) => {
                    match tag_type {
                        TagType::Boolean => {
                            if value.len() == 0 {
                                return Err(io::Error::new(
                                    io::ErrorKind::InvalidData,
                                    "BOOLEAN must have a value",
                                ));
                            } else if value.len() > 1 || (value[0] != 0 && value[0] != 1) {
                                return Err(io::Error::new(
                                    io::ErrorKind::InvalidData,
                                    "BOOLEAN is not in canonical format",
                                ));
                            }

                            DecodedValueKind::Boolean(value[0] != 0)
                        }
                        TagType::Integer => {
                            const SIGN_MASK: u8 = 0b1000_0000;
                            const BUF_SIZE: usize = std::mem::size_of::<i64>();

                            if value.len() == 0 {
                                return Err(io::Error::new(
                                    io::ErrorKind::InvalidData,
                                    "INTEGER must have a value",
                                ));
                            } else if value.len() > 8 {
                                return Err(io::Error::new(
                                    io::ErrorKind::InvalidData,
                                    "INTEGER cannot be larger than 8 bytes",
                                ));
                            }

                            let mut be_buf;
                            if value[0] & SIGN_MASK == SIGN_MASK {
                                // the number is negative
                                be_buf = [0xff; BUF_SIZE];
                            } else {
                                // the number is positive
                                be_buf = [0x00; BUF_SIZE];
                            }
                            be_buf[BUF_SIZE - value.len()..].copy_from_slice(value);

                            DecodedValueKind::Integer(i64::from_be_bytes(be_buf))
                        }
                        TagType::OctetString => DecodedValueKind::OctetString(value.to_vec()),
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
                        TagType::Sequence => {
                            let elements = DerReader::new(value, tlv.value.pos.start)
                                .read_all()?
                                .into_iter()
                                .map(|tlv| Self::der_decode(tlv))
                                .collect::<io::Result<Vec<DecodedValue>>>()?;
                            DecodedValueKind::Sequence(elements)
                        }
                        other => todo!("decode {:?}", other),
                    }
                }
                Err(_) => DecodedValueKind::Raw(value.to_vec()),
            },
            _ => DecodedValueKind::Raw(value.to_vec()),
        };

        Ok(DecodedValue {
            tag: tlv.tag,
            len: TlvElement::new(value.len() as u32, tlv.len_pos),
            value_pos: tlv.value.pos,
            kind,
        })
    }
}

pub enum DecodedValueKind {
    Raw(Vec<u8>),
    Boolean(bool),
    Integer(i64),
    BitString(BigUint),
    OctetString(Vec<u8>),
    Null,
    ObjectIdentifier(Oid),
    Real(f64),
    Sequence(Vec<DecodedValue>),
}
