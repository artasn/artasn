use std::io;

use num::bigint::Sign;
use num::BigInt;
use widestring::{Utf16String, Utf32String};

use super::reader::{read_vlq, DerReader};
use crate::compiler::parser::Result;
use crate::encoding::*;
use crate::{
    compiler::{
        parser::{AstElement, Loc},
        Context,
    },
    types::*,
    values::*,
};

struct ComponentData {
    pub name: Option<String>,
    pub tagged_type: TaggedType,
    pub index: usize,
}

fn get_component_by_tag(
    context: &Context,
    mode: &DecodeMode,
    tlv_tag: &TlvElement<TlvTag>,
    index: usize,
) -> DecodeResult<Option<ComponentData>> {
    // TODO: implement SET
    Ok(match mode {
        DecodeMode::Contextless => None,
        DecodeMode::SpecificType { resolved, .. } => match &resolved.ty {
            BuiltinType::Structure(structure) => {
                for (component_index, component) in structure
                    .resolve_components(context)
                    .map_err(DecodeError::Parser)?
                    .into_iter()
                    .enumerate()
                    .skip(index)
                {
                    let component_type = component
                        .component_type
                        .resolve(context)
                        .map_err(DecodeError::Parser)?;
                    let matching_tag = type_eq_tlv(context, &component_type, &tlv_tag.element)
                        .map_err(DecodeError::Parser)?;
                    match matching_tag {
                        Some(matching_type) => {
                            return Ok(Some(ComponentData {
                                name: Some(component.name.element.clone()),
                                tagged_type: matching_type.into(),
                                index: component_index,
                            }));
                        }
                        None => {
                            if component.optional || component.default_value.is_some() {
                                continue;
                            } else {
                                let tag_str = component_type
                                    .get_possible_tags(context)
                                    .map_err(DecodeError::Parser)?
                                    .into_iter()
                                    .map(|(tag, _)| tag.to_string())
                                    .collect::<Vec<String>>()
                                    .join(" | ");
                                return Err(DecodeError::Decoder {
                                    message: format!("SEQUENCE component at index {} is defined with tag '{}' but the encoded value has tag '{}'", component_index, tag_str, tlv_tag.element),
                                    pos: tlv_tag.pos,
                                });
                            }
                        }
                    }
                }
                None
            }
            BuiltinType::StructureOf(of) => Some(ComponentData {
                name: None,
                tagged_type: *of.component_type.clone(),
                index,
            }),
            BuiltinType::Choice(choice) => {
                if index != 0 {
                    return Err(DecodeError::Decoder {
                        message: "CHOICE encoding cannot contain multiple values".to_string(),
                        pos: tlv_tag.pos,
                    });
                }

                for alternative in choice
                    .resolve_alternatives(context)
                    .map_err(DecodeError::Parser)?
                {
                    let alternative_type = alternative
                        .alternative_type
                        .resolve(context)
                        .map_err(DecodeError::Parser)?;
                    if let Some(matching_type) =
                        type_eq_tlv(context, &alternative_type, &tlv_tag.element)
                            .map_err(DecodeError::Parser)?
                    {
                        return Ok(Some(ComponentData {
                            name: Some(alternative.name.element.clone()),
                            tagged_type: matching_type.into(),
                            index: 0,
                        }));
                    }
                }

                return Err(DecodeError::Decoder {
                    message: format!(
                        "CHOICE does not define an alternative with tag '{}'",
                        tlv_tag.element
                    ),
                    pos: tlv_tag.pos,
                });
            }
            _ => None,
        },
    })
}

fn type_eq_tlv(
    context: &Context,
    resolved_type: &ResolvedType,
    tlv_tag: &TlvTag,
) -> Result<Option<ResolvedType>> {
    let tags = resolved_type.get_possible_tags(context)?;
    for (tag, ty) in tags {
        if tag.class == tlv_tag.class && tag.num == tlv_tag.num {
            return Ok(Some(ResolvedType {
                tag: Some(tag),
                ty,
                // TODO: include the constraints here,
                // so that they can be used for verifying decoded values
                constraints: None,
            }));
        }
    }
    Ok(None)
}

fn ber_decode_integer(value: &[u8]) -> io::Result<BigInt> {
    const SIGN_MASK: u8 = 0b1000_0000;

    if value.is_empty() {
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

    Ok(BigInt::from_bytes_be(sign, value))
}

fn ber_decode_universal(
    syntax: BasicEncodingKind,
    tlv: &Tlv<'_>,
    tag_type: TagType,
) -> io::Result<DecodedValueKind> {
    let value = tlv.value.element;
    Ok(match tag_type {
        TagType::Any => DecodedValueKind::Raw(value.to_vec()),
        TagType::Boolean => {
            if value.is_empty() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "BOOLEAN must have a value",
                ));
            } else if value.len() > 1 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "BOOLEAN is too large",
                ));
            }

            DecodedValueKind::Boolean(match syntax {
                BasicEncodingKind::Basic => value[0] != 0x00,
                _ => match value[0] {
                    0x00 => false,
                    0xff => true,
                    _ => {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            "BOOLEAN is not in canonical format",
                        ))
                    }
                },
            })
        }
        TagType::Integer => DecodedValueKind::Integer(ber_decode_integer(value)?),
        TagType::BitString => {
            if value.len() < 2 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "BIT STRING must have a value",
                ));
            }

            let unused_bits = value[0];
            let data = value[1..].to_vec();

            DecodedValueKind::BitString(BitStringValue { data, unused_bits })
        }
        TagType::OctetString => DecodedValueKind::OctetString(value.to_vec()),
        TagType::Null => {
            if !value.is_empty() {
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
            let (first_node, len) = read_vlq(value)?;
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
        TagType::Enumerated => {
            let value = ber_decode_integer(value)?;
            let value: i64 = value.try_into().map_err(|_| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "ENUMERATED value out of bounds for signed 64-bit integer",
                )
            })?;
            DecodedValueKind::Enumerated(DecodedEnumerationItem { item: None, value })
        }
        TagType::ObjectDescriptor
        | TagType::NumericString
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
            let str = if value.is_empty() {
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
                    | TagType::ObjectDescriptor
                    | TagType::UTF8String
                    | TagType::CharacterString => String::from_utf8(value.to_vec())
                        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?,
                    TagType::BMPString => {
                        if value.len() % 2 != 0 {
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                "BMPString is not properly UTF-16 encoded",
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
        TagType::Time => match String::from_utf8(value.to_vec()) {
            Ok(value) => {
                DecodedValueKind::Time(Time::parse(&AstElement::new(&value, Loc::at(0))).map_err(
                    |err| io::Error::new(io::ErrorKind::InvalidData, err.kind.message()),
                )?)
            }
            Err(err) => return Err(io::Error::new(io::ErrorKind::InvalidData, err)),
        },
        TagType::Sequence | TagType::Set => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("{} must have the Constructed bit set", tag_type),
            ));
        }
        TagType::UTCTime => DecodedValueKind::UTCTime(
            UTCTime::parse(&AstElement::new(value, Loc::at(0)))
                .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.kind.message()))?,
        ),
        TagType::Date => DecodedValueKind::Date(
            Date::parse(&AstElement::new(value, Loc::at(0)))
                .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.kind.message()))?,
        ),
        TagType::TimeOfDay => DecodedValueKind::TimeOfDay(
            TimeOfDay::parse(&AstElement::new(value, Loc::at(0)))
                .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.kind.message()))?,
        ),
        TagType::DateTime => DecodedValueKind::DateTime(
            DateTime::parse(&AstElement::new(value, Loc::at(0)))
                .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.kind.message()))?,
        ),
        TagType::Duration => match String::from_utf8(value.to_vec()) {
            Ok(value) => DecodedValueKind::Duration(
                Duration::parse(&AstElement::new(&value, Loc::at(0))).map_err(|err| {
                    io::Error::new(io::ErrorKind::InvalidData, err.kind.message())
                })?,
            ),
            Err(err) => return Err(io::Error::new(io::ErrorKind::InvalidData, err)),
        },
        other => todo!("decode {:?}", other),
    })
}

fn ber_decode_tlv(
    syntax: BasicEncodingKind,
    context: &Context,
    tlv: Tlv<'_>,
    mode: &DecodeMode,
) -> DecodeResult<DecodedValue> {
    let form = match tlv.tag.element.form {
        TypeForm::Primitive => {
            let mut kind = match tlv.tag.element.class {
                Class::Universal => match TagType::try_from(tlv.tag.element.num) {
                    Ok(tag_type) => {
                        ber_decode_universal(syntax, &tlv, tag_type).map_err(DecodeError::Io)?
                    }
                    Err(_) => DecodedValueKind::Raw(tlv.value.element.to_vec()),
                },
                _ => match &mode {
                    DecodeMode::Contextless => DecodedValueKind::Raw(tlv.value.element.to_vec()),
                    DecodeMode::SpecificType { resolved, .. } => {
                        if let Some(matching_type) =
                            type_eq_tlv(context, resolved, &tlv.tag.element)
                                .map_err(DecodeError::Parser)?
                        {
                            // we use ty.tag_type() here to get the UNIVERSAL tag type for the underlying builtin type, not the user-defined tag
                            ber_decode_universal(
                                syntax,
                                &tlv,
                                matching_type.ty.tag_type().expect("tag_type"),
                            )
                            .map_err(DecodeError::Io)?
                        } else {
                            let tag_str = resolved
                                .get_possible_tags(context)
                                .map_err(DecodeError::Parser)?
                                .into_iter()
                                .map(|(tag, _)| tag.to_string())
                                .collect::<Vec<String>>()
                                .join(" | ");
                            return Err(DecodeError::Decoder {
                                message: format!("DecodeMode is SpecificType but encoded tag {} does not match provided tag {}", tlv.tag.element, tag_str),
                                pos: tlv.tag.pos,
                            });
                        }
                    }
                },
            };
            if let (
                DecodedValueKind::Enumerated(enumeration),
                DecodeMode::SpecificType { resolved, .. },
            ) = (&mut kind, mode)
            {
                match &resolved.ty {
                    BuiltinType::Enumerated(enumerated) => {
                        for item in enumerated {
                            let item_value = match &item.value {
                                EnumerationItemValue::Implied(implied) => *implied,
                                EnumerationItemValue::Specified(spec) => {
                                    match spec.resolve(context).map_err(DecodeError::Parser)?.value
                                    {
                                        BuiltinValue::Integer(int) => {
                                            int.try_into().expect("out of bounds ENUMERATED item")
                                        }
                                        other => panic!("expecting INTEGER but found {}", other),
                                    }
                                }
                            };
                            if item_value == enumeration.value {
                                enumeration.item = Some(item.name.element.clone());
                                break;
                            }
                        }
                    }
                    other => panic!(
                        "decoded an ENUMERATED item but the SpecificType is {}",
                        other
                    ),
                }
            }
            DecodedValueForm::Primitive(kind)
        }
        TypeForm::Constructed => {
            let mut index = 0;
            let mut elements = Vec::new();
            for tlv in DerReader::new(tlv.value.element, tlv.value.pos.start) {
                let tlv = tlv.map_err(DecodeError::Io)?;
                let component = get_component_by_tag(context, mode, &tlv.tag, index)?;
                let mode = match component {
                    Some(data) => {
                        index = data.index + 1;
                        DecodeMode::SpecificType {
                            source_ident: match &data.tagged_type.ty {
                                UntaggedType::BuiltinType(_) => None,
                                UntaggedType::Reference(typeref) => Some(typeref.element.clone()),
                                UntaggedType::ObjectClassField(_) => {
                                    todo!("handle ObjectClassField")
                                }
                            },
                            component_name: data.name,
                            resolved: data
                                .tagged_type
                                .resolve(context)
                                .map_err(DecodeError::Parser)?,
                        }
                    }
                    None => {
                        index += 1;
                        DecodeMode::Contextless
                    }
                };
                elements.push(ber_decode_tlv(syntax, context, tlv, &mode)?);
            }
            DecodedValueForm::Constructed(elements)
        }
    };
    let metadata = match mode {
        DecodeMode::Contextless => None,
        DecodeMode::SpecificType {
            source_ident,
            component_name,
            resolved,
        } => Some(DecodedValueMetadata {
            type_ident: match source_ident {
                Some(source_ident) => {
                    UntaggedType::Reference(AstElement::new(source_ident.clone(), Loc::default()))
                }
                None => UntaggedType::BuiltinType(resolved.ty.clone()),
            },
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

pub fn ber_decode_value(
    syntax: BasicEncodingKind,
    buf: &[u8],
    context: &Context,
    mode: &DecodeMode,
) -> DecodeResult<Vec<DecodedValue>> {
    let reader = DerReader::new(buf, 0);
    reader
        .into_iter()
        .map(|tlv| {
            Ok(ber_decode_tlv(
                syntax,
                context,
                tlv.map_err(DecodeError::Io)?,
                mode,
            ))?
        })
        .collect::<DecodeResult<Vec<DecodedValue>>>()
}

#[cfg(test)]
mod test {
    use crate::{
        compiler::{test::json_test, Context},
        encoding::{BasicEncodingKind, DecodeMode},
    };

    use super::ber_decode_value;

    #[test]
    fn test_ber_decode_contextless() {
        // TODO: once the compiler has the functionality to compile the X.509 modules,
        // change DecodeMode from Contextless to SpecificType with the Certificate type
        let der = include_bytes!("../../../test-data/decode/LetsEncryptX3.der");
        let context = Context::new();
        ber_decode_value(
            BasicEncodingKind::Distinguished,
            der,
            &context,
            &DecodeMode::Contextless,
        )
        .unwrap();
    }

    json_test!(
        test_ber_decode_specific_type,
        "../../../test-data/decode/DecodeTest"
    );
}
