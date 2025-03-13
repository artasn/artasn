use std::io;

use num::{bigint::Sign, BigInt, BigUint, Signed};
use widestring::{Utf16String, Utf32String};

use crate::{
    compiler::{parser::Result, Context},
    types::{BuiltinType, Class, ResolvedType, Tag, TagKind, TagSource, TagType},
    values::{BuiltinValue, StructureValueComponent, ValueResolve},
};

use super::strings;

pub fn der_encode_integer(buf: &mut Vec<u8>, num: &BigInt) {
    const SIGN_MASK: u8 = 0b1000_0000;

    if num == &BigInt::ZERO {
        // fast encode for 0
        buf.push(0x00);
    } else {
        let sign = num.sign();
        let bytes = num.to_signed_bytes_le();

        // write the bytes in little-endian order,
        // such that when the DER is reversed after encoding,
        // the bytes are in big-endian order
        buf.extend_from_slice(&bytes);

        let msb = bytes[bytes.len() - 1];
        if sign != Sign::Minus && msb & SIGN_MASK == SIGN_MASK {
            // when the sign bit is set in the msb, but the number is positive, add a padding byte without the sign bit
            buf.push(0x00);
        } else if sign == Sign::Minus && msb & SIGN_MASK != SIGN_MASK {
            // when the sign bit is not set in the msb, but the number is negative, add a padding byte containing the sign bit
            buf.push(0xff);
        }
    }
}

pub fn der_decode_integer(value: &[u8]) -> io::Result<BigInt> {
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

pub fn der_encode_character_string(buf: &mut Vec<u8>, tag_type: TagType, str: &str) {
    match tag_type {
        TagType::NumericString
        | TagType::PrintableString
        | TagType::IA5String
        | TagType::VisibleString => {
            // NumericString, PrintableString, VisibleString, and IA5String all contain only 7-bit ASCII (T.50) characters
            let bytes = strings::T50_MAP
                .encode_str(str)
                .expect("string cannot be encoded to T.50");
            buf.extend(bytes.into_iter().rev());
        }
        TagType::TeletexString => {
            let bytes = strings::T61_MAP
                .encode_str(str)
                .expect("TeletexString cannot be encoded to T.61");
            buf.extend(bytes.into_iter().rev());
        }
        TagType::VideotexString => {
            let bytes = strings::T100_MAP
                .encode_str(str)
                .expect("VideotexString cannot be encoded to T.100");
            buf.extend(bytes.into_iter().rev());
        }
        TagType::UTF8String
        | TagType::GeneralString
        | TagType::GraphicString
        | TagType::ObjectDescriptor => {
            // Rust strings are already UTF-8, so we just write the string's internal bytes for UTF8String
            // GeneralString, GraphicString, and ObjectDescriptor are implemented as UTF-8 as well
            buf.extend(str.bytes().rev());
        }
        TagType::BMPString => {
            // BMPString is encoded as UTF-16
            let utf16str = Utf16String::from_str(str);
            for b in utf16str.into_vec().into_iter().rev() {
                buf.extend_from_slice(&b.to_le_bytes());
            }
        }
        TagType::UniversalString => {
            // UniversalString is encoded as UTF-32
            let utf32str = Utf32String::from_str(str);
            for b in utf32str.into_vec().into_iter().rev() {
                buf.extend_from_slice(&b.to_le_bytes());
            }
        }
        other => todo!("{:?}", other),
    }
}

pub fn der_encode_real(buf: &mut Vec<u8>, mut mantissa: BigInt, base: i64, mut exponent: BigInt) {
    if mantissa == BigInt::ZERO {
        return;
    }

    if base == 2 {
        // while mantissa is divisible by 2 (even)
        while !mantissa.bit(0) {
            mantissa >>= 1;
            exponent += 1;
        }

        der_encode_integer(buf, &mantissa.abs());

        let exp_len = buf.len();
        der_encode_integer(buf, &exponent);
        let exp_len = buf.len() - exp_len;

        let mut bitflags = 0x00;
        bitflags |= 0b1000_0000; // binary encoding
        if mantissa.sign() == Sign::Minus {
            bitflags |= 0b0100_0000; // sign bit
        }

        // See X.690 clause 8.5.7.4 for what is being encoded here
        bitflags |= match exp_len {
            1 => 0b00,
            2 => 0b01,
            3 => 0b10,
            _ => {
                // X.690 clause 8.5.7.4(d) states that "the first nine bits of the transmitted exponent shall not be all zeros or all ones"
                // figure out why this is and how to handle it
                buf.push(
                    exp_len
                        .try_into()
                        .expect("exp_len is larger than 255 bytes"),
                );
                0b11
            }
        };

        buf.push(bitflags);
    } else if base == 10 {
        // while mantissa is divisble by 10
        while &mantissa % 10 == BigInt::ZERO {
            mantissa /= 10;
            exponent += 1;
        }

        let sign = exponent.sign();
        let uexp = exponent.magnitude();
        buf.extend(uexp.to_str_radix(10).bytes().rev());
        if uexp == &BigUint::ZERO {
            buf.push(b'+');
        } else if sign == Sign::Minus {
            buf.push(b'-');
        }
        buf.push(b'E');
        buf.push(b'.');
        buf.extend(mantissa.to_str_radix(10).bytes().rev());
        buf.push(0b0000_0011); // bitflags: base 10, ISO 6093 NR3 form
    } else {
        panic!("base = {} but must be either 2 or 10", base);
    }
}

pub fn der_encode_structure(
    buf: &mut Vec<u8>,
    context: &Context,
    components: &[StructureValueComponent],
    resolved_type: &ResolvedType,
) -> Result<()> {
    let ty_components = match &resolved_type.ty {
        BuiltinType::Structure(structure) => &structure.components,
        _ => unreachable!(),
    };

    // ast.rs guarantees all components in SEQUENCE/SET type are provided in value,
    // and that the value provides only components in the SEQUENCE/SET type,
    // and that the component values are in the same order as in the type definition
    for component in components.iter().rev() {
        // default values aren't encoded
        if component.is_default {
            continue;
        }
        let value = component.value.resolve(context)?;
        let value_ty = ty_components
            .iter()
            .find(|ty_component| ty_component.name.element == component.name.element)
            .expect("find type component matching value component for SEQUENCE/SET")
            .component_type
            .resolve(context)?;
        value.der_encode(buf, context, &value_ty)?;
    }

    Ok(())
}

/// See X.690 clause 8.18 and its subclauses to see what is being encoded here.
pub fn der_encode_external(
    buf: &mut Vec<u8>,
    context: &Context,
    components: &[StructureValueComponent],
    resolved_type: &ResolvedType,
) -> Result<()> {
    if components
        .iter()
        .find(|component| component.name.element.as_str() == "encoding")
        .is_some()
    {
        // if the "encoding" component is present, then EXTERNAL is defined as the X.208 version;
        // the X.690 encoding maps one-to-one with X.208 EXTERNAL, and can be encoded as a normal structure
        der_encode_structure(buf, context, components, resolved_type)?;
    } else {
        let data_value = components
            .iter()
            .find(|component| component.name.element.as_str() == "data-value")
            .expect("missing data-value");
        let data_value = data_value.value.resolve(context)?;
        data_value.der_encode(
            buf,
            context,
            &ResolvedType {
                tag: Some(Tag::new(
                    Class::ContextSpecific,
                    1,
                    TagKind::Implicit,
                    TagSource::KindSpecified,
                )), // this is the tag for the 'octet-aligned' variant of 'encoding CHOICE { ... }'
                ty: BuiltinType::OctetString,
                constraint: None,
            },
        )?;

        if let Some(data_value_descriptor) = components
            .iter()
            .find(|component| component.name.element.as_str() == "data-value-descriptor")
        {
            let data_value_descriptor = data_value_descriptor.value.resolve(context)?;
            data_value_descriptor.der_encode(
                buf,
                context,
                &ResolvedType::universal(TagType::ObjectDescriptor),
            )?;
        }

        let (direct_reference, indirect_reference) = match &components[0].value.resolve(context)? {
            BuiltinValue::Choice(choice) => {
                let alternative_value = choice.value.resolve(context)?;
                match choice.alternative.element.as_str() {
                    "syntax" => (Some(alternative_value), None),
                    "presentation-context-id" => (None, Some(alternative_value)),
                    "context-negotiation" => match alternative_value {
                        BuiltinValue::Sequence(seq) => {
                            let presentation_context_id =
                                seq.components[0].value.resolve(context)?;
                            let transfer_syntax = seq.components[1].value.resolve(context)?;
                            (Some(transfer_syntax), Some(presentation_context_id))
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        };
        if let Some(indirect_reference) = indirect_reference {
            indirect_reference.der_encode(
                buf,
                context,
                &ResolvedType::universal(TagType::Integer),
            )?;
        }
        if let Some(direct_reference) = direct_reference {
            direct_reference.der_encode(
                buf,
                context,
                &ResolvedType::universal(TagType::ObjectIdentifier),
            )?;
        }
    }
    Ok(())
}
