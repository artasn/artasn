use crate::{
    compiler::parser::{Error, ErrorKind, Loc},
    encoding::{ber::MAX_VLQ_LEN, *},
};

use num::{bigint::Sign, BigInt, BigUint, Signed};
use widestring::{Utf16String, Utf32String};

use crate::{
    compiler::{parser::Result, Context},
    types::*,
    values::*,
};

pub(crate) fn write_vlq(mut n: u64, buf: &mut Vec<u8>) -> usize {
    const CARRY_BIT: u8 = 0b1000_0000;
    const MASK: u8 = 0b0111_1111;

    if n < 0x80 {
        buf.push(n as u8);
        return 1;
    }

    let mut vlq_buf = [0u8; MAX_VLQ_LEN];
    let mut index = 0;

    while n >= 0x80 {
        vlq_buf[index] = (n & MASK as u64) as u8 | CARRY_BIT;
        index += 1;
        n >>= 7;
    }

    vlq_buf[index] = n as u8 | CARRY_BIT;
    vlq_buf[0] &= MASK;
    index += 1;
    buf.extend_from_slice(&vlq_buf[..index]);
    index
}

pub(crate) fn write_tlv_len(len: u64, buf: &mut Vec<u8>) {
    if len < 0x80 {
        buf.push(len as u8);
    } else {
        let (le_bytes, le_bytes_len) = u64_to_le_bytes(len);
        buf.extend_from_slice(&le_bytes[..le_bytes_len]);
        buf.push(0x80 | le_bytes_len as u8);
    }
}

fn ber_encode_integer(buf: &mut Vec<u8>, num: &BigInt) {
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

fn ber_encode_character_string(buf: &mut Vec<u8>, tag_type: TagType, str: &str) {
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

fn ber_encode_real(buf: &mut Vec<u8>, mut mantissa: BigInt, base: i64, mut exponent: BigInt) {
    if mantissa == BigInt::ZERO {
        return;
    }

    if base == 2 {
        // while mantissa is divisible by 2 (even)
        while !mantissa.bit(0) {
            mantissa >>= 1;
            exponent += 1;
        }

        ber_encode_integer(buf, &mantissa.abs());

        let exp_len = buf.len();
        ber_encode_integer(buf, &exponent);
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

fn ber_encode_structure(
    buf: &mut Vec<u8>,
    context: &Context,
    components: &[StructureValueComponent],
) -> Result<()> {
    // ast.rs guarantees all components in SEQUENCE/SET type are provided in value,
    // and that the value provides only components in the SEQUENCE/SET type,
    // and that the component values are in the same order as in the type definition
    for component in components.iter().rev() {
        // default values aren't encoded
        if component.is_default {
            continue;
        }
        let typed_value = component.value.resolve(context)?;
        ber_encode_value(buf, context, &typed_value)?;
    }

    Ok(())
}

/// See X.690 clause 8.18 and its subclauses to see what is being encoded here.
fn ber_encode_external(
    buf: &mut Vec<u8>,
    context: &Context,
    components: &[StructureValueComponent],
) -> Result<()> {
    if components
        .iter()
        .any(|component| component.name.element.as_str() == "encoding")
    {
        // if the "encoding" component is present, then EXTERNAL is defined as the X.208 version;
        // the X.690 encoding maps one-to-one with X.208 EXTERNAL, and can be encoded as a normal structure
        ber_encode_structure(buf, context, components)?;
    } else {
        let data_value = components
            .iter()
            .find(|component| component.name.element.as_str() == "data-value")
            .expect("missing data-value");
        let data_value = data_value.value.resolve(context)?;
        ber_encode_value(buf, context, &data_value)?;

        if let Some(data_value_descriptor) = components
            .iter()
            .find(|component| component.name.element.as_str() == "data-value-descriptor")
        {
            let data_value_descriptor = data_value_descriptor.value.resolve(context)?;
            ber_encode_value(buf, context, &data_value_descriptor)?;
        }

        let (direct_reference, indirect_reference) =
            match &components[0].value.resolve(context)?.value {
                BuiltinValue::Choice(choice) => {
                    let alternative_value = choice.value.resolve(context)?;
                    match choice.alternative.element.as_str() {
                        "syntax" => (Some(alternative_value), None),
                        "presentation-context-id" => (None, Some(alternative_value)),
                        "context-negotiation" => match &alternative_value.value {
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
            ber_encode_value(buf, context, &indirect_reference)?;
        }
        if let Some(direct_reference) = direct_reference {
            ber_encode_value(buf, context, &direct_reference)?;
        }
    }
    Ok(())
}

fn ber_encode_tag(
    buf: &mut Vec<u8>,
    tag: &Tag,
    ctx: TagContext<'_>,
    form_override: Option<TypeForm>,
) {
    let class = match tag.class {
        Class::Universal => 0b00,
        Class::Application => 0b01,
        Class::ContextSpecific => 0b10,
        Class::Private => 0b11,
    };
    let form = form_override
        .map(|form| match form {
            TypeForm::Primitive => 0b0,
            TypeForm::Constructed => 0b1,
        })
        .unwrap_or_else(|| match (ctx.is_outer_explicit, ctx.ty.form()) {
            // if not the outer tag of an EXPLICIT definition and is primitive, form = 0
            (false, TypeForm::Primitive) => 0b0,
            // if either the outer tag of an EXPLICIT definition or is constructed, form = 1
            _ => 0b1,
        });
    if tag.num >= 31 {
        write_vlq(tag.num as u64, buf);
    }
    let msb_tag = if tag.num <= 30 {
        tag.num as u8
    } else {
        0b11111
    };
    buf.push(class << 6 | form << 5 | msb_tag);
}

fn is_real_type(ty: &BuiltinType) -> bool {
    match ty {
        BuiltinType::Structure(structure) => structure
            .components
            .iter()
            .find(|component| component.name.element.as_str() == "asn1chef-special")
            .is_some(),
        _ => false,
    }
}

fn is_external_type(ty: &BuiltinType) -> bool {
    match ty {
        BuiltinType::Structure(structure) => structure
            .components
            .iter()
            .find(|component| component.name.element.as_str() == "asn1chef-external")
            .is_some(),
        _ => false,
    }
}

/// Reverse-encodes the value, including its tag.
/// The resulting bytes are in reverse order.
/// The bytes of the final output must be reversed to be valid DER.
/// Regardless of the TransferSyntax provided, the output will always be valid DER.
/// Since DER is always valid BER and valid CER, this is always acceptable.
pub fn ber_encode_value(
    buf: &mut Vec<u8>,
    context: &Context,
    typed_value: &ResolvedValue,
) -> Result<()> {
    let resolved_type = &typed_value.ty;

    let start_len = buf.len();
    match &typed_value.value {
        BuiltinValue::Boolean(b) => {
            if *b {
                buf.push(0xff);
            } else {
                buf.push(0x00);
            }
        }
        BuiltinValue::Integer(num) => {
            if let Some(tag) = &resolved_type.tag {
                match (tag.class, TagType::try_from(tag.num)) {
                    (Class::Universal, Ok(TagType::Real)) => {
                        ber_encode_real(buf, num.clone(), 10, BigInt::ZERO)
                    }
                    _ => ber_encode_integer(buf, num),
                }
            } else {
                ber_encode_integer(buf, num);
            }
        }
        BuiltinValue::BitString(bit_string) => {
            buf.extend(bit_string.data.iter().rev());
            buf.push(bit_string.unused_bits);
        }
        BuiltinValue::OctetString(octet_string) => buf.extend(octet_string.iter().rev()),
        BuiltinValue::Null => (),
        BuiltinValue::ObjectIdentifier(oid) => {
            let oid = oid.resolve_oid(context)?.0;

            match oid.len() {
                2.. => {
                    for node in oid.iter().skip(2).rev() {
                        write_vlq(*node, buf);
                    }
                }
                _ => {
                    return Err(Error {
                        kind: ErrorKind::Ast(
                            "illegal OBJECT IDENTIFIER with less than two nodes".to_string(),
                        ),
                        loc: Loc::default(), // TODO: add loc info to the entire oid, not just its elements
                    });
                }
            }

            let prefix = oid[0] * 40 + oid[1];
            write_vlq(prefix, buf);
        }
        BuiltinValue::RelativeOid(oid) => {
            let oid = oid.resolve_oid(context)?.0;
            for node in oid.iter().rev() {
                write_vlq(*node, buf);
            }
        }
        BuiltinValue::RealLiteral(lit) => {
            ber_encode_real(buf, lit.mantissa.clone(), 10, lit.exponent.clone());
        }
        BuiltinValue::Enumerated(enumerated) => {
            let resolved = enumerated.resolve(context)?;
            let num = match &resolved.value {
                BuiltinValue::Integer(num) => num.clone(),
                other => {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "ENUMERATED value must be of type INTEGER, but found {}",
                            other.tag_type(context)?
                        )),
                        loc: enumerated.loc,
                    })
                }
            };
            ber_encode_integer(buf, &num);
        }
        BuiltinValue::Time(time) => {
            buf.extend(time.to_ber_string().into_bytes().into_iter().rev());
        }
        BuiltinValue::Sequence(structure) | BuiltinValue::Set(structure) => {
            if is_real_type(&typed_value.ty.ty) {
                let special = structure
                    .components
                    .iter()
                    .find(|component| component.name.element.as_str() == "asn1chef-special");
                if let Some(special) = special {
                    match &special.value.resolve(context)?.value {
                        BuiltinValue::Integer(int) => {
                            let int: u32 = int.try_into().expect("SpecialReal is out of bounds");
                            if int == 0 {
                                // PLUS-INFINITY
                                buf.push(0x40);
                            } else if int == 1 {
                                // MINUS-INFINITY
                                buf.push(0x41);
                            } else if int == 2 {
                                // NOT-A-NUMBER
                                buf.push(0x42);
                            } else {
                                unreachable!();
                            }
                        }
                        _ => unreachable!(),
                    }
                } else {
                    macro_rules! to_int {
                        ( $component:expr) => {{
                            let component = &$component;
                            match &component.value.resolve(context)?.value {
                                BuiltinValue::Integer(int) => int.clone(),
                                _ => unreachable!(),
                            }
                        }};
                    }

                    let mantissa = to_int!(structure.components[0]);
                    let base = to_int!(structure.components[1]);
                    let exponent = to_int!(structure.components[2]);

                    ber_encode_real(
                        buf,
                        mantissa,
                        base.try_into().expect("base is out of bounds"),
                        exponent,
                    );
                }
            } else if is_external_type(&typed_value.ty.ty) {
                ber_encode_external(buf, context, &structure.components)?
            } else {
                ber_encode_structure(buf, context, &structure.components)?;
            }
        }
        BuiltinValue::SequenceOf(structure) | BuiltinValue::SetOf(structure) => {
            for element in structure.iter().rev() {
                let resolved = element.resolve(context)?;
                ber_encode_value(buf, context, &resolved)?;
            }
        }
        BuiltinValue::Choice(choice) => {
            let value = choice.value.resolve(context)?;
            ber_encode_value(buf, context, &value)?;
        }
        BuiltinValue::CharacterString(tag_type, str) => {
            ber_encode_character_string(buf, *tag_type, str);
        }
        BuiltinValue::UTCTime(utc) => {
            buf.extend(utc.to_ber_string().into_bytes().into_iter().rev());
        }
        BuiltinValue::GeneralizedTime(gt) => {
            buf.extend(gt.to_ber_string().into_bytes().into_iter().rev());
        }
        BuiltinValue::Date(date) => {
            buf.extend(date.to_ber_string().into_bytes().into_iter().rev());
        }
        BuiltinValue::TimeOfDay(time_of_day) => {
            buf.extend(time_of_day.to_ber_string().into_bytes().into_iter().rev());
        }
        BuiltinValue::DateTime(date_time) => {
            buf.extend(date_time.to_ber_string().into_bytes().into_iter().rev());
        }
        BuiltinValue::Duration(duration) => {
            buf.extend(duration.to_ber_string().into_bytes().into_iter().rev());
        }
        BuiltinValue::Containing(containing) => {
            let constraint = resolved_type
                .constraint
                .as_ref()
                .map(|constraint| {
                    constraint
                        .flatten()
                        .iter()
                        .find_map(|subtype| match &subtype.element {
                            SubtypeElement::Contents(contents) => Some(contents),
                            _ => None,
                        })
                })
                .expect("CONTAINING value for type without constraint")
                .expect("CONTAINING value for type without contents constraint");

            let ts = match &constraint.encoded_by {
                Some(encoded_by) => {
                    let resolved = encoded_by.resolve(context)?;
                    match &resolved.value {
                        BuiltinValue::ObjectIdentifier(oid) => {
                            let oid = oid.resolve_oid(context)?;
                            match TransferSyntax::get_by_oid(&oid) {
                                    Some(ts) => {
                                        if ts.get_codec().encoder.is_some() {
                                            ts
                                        } else {
                                            return Err(Error {
                                                kind: ErrorKind::Ast(format!("encoding with the {} transfer syntax is not yet implemented", ts.get_name())),
                                                loc: encoded_by.loc,
                                            })
                                        }
                                    },
                                    None => {
                                        return Err(Error {
                                            kind: ErrorKind::Ast(format!("the provided OBJECT IDENTIFIER ({}) does not represent a registered transfer syntax", oid)),
                                            loc: encoded_by.loc,
                                        })
                                    }
                                }
                        }
                        other => {
                            return Err(Error {
                                kind: ErrorKind::Ast(format!(
                                    "expecting OBJECT IDENTIFIER for the transfer  found {}",
                                    other.tag_type(context)?
                                )),
                                loc: encoded_by.loc,
                            })
                        }
                    }
                }
                None => &TransferSyntax::Basic(BasicEncodingKind::Distinguished),
            };

            let contained_value = containing.value.resolve(context)?;
            let encoder = ts.get_codec().encoder.unwrap();
            encoder(ts, buf, context, &contained_value)?;

            match &resolved_type.ty {
                // write the bit string unused bits count
                BuiltinType::BitString(_) => buf.push(0x00),
                BuiltinType::OctetString => (),
                _ => unreachable!(),
            }
        }
    }

    let tag = resolved_type.tag.as_ref();
    if let Some(tag) = tag {
        let end_len = buf.len();
        if let TagKind::Explicit(inner_tag) = &tag.kind {
            if let Some((class, num)) = inner_tag.or(resolved_type
                .ty
                .tag_type()
                .map(|tag_type| (Class::Universal, tag_type as u16)))
            {
                write_tlv_len((end_len - start_len) as u64, buf);
                ber_encode_tag(
                    buf,
                    &Tag {
                        class,
                        num,
                        kind: TagKind::Implicit,
                        source: TagSource::TagImplied,
                    },
                    TagContext {
                        is_outer_explicit: false,
                        ty: &resolved_type.ty,
                    },
                    None,
                );
            }
        }

        let end_len = buf.len();
        write_tlv_len((end_len - start_len) as u64, buf);

        let form_override = if is_real_type(&typed_value.ty.ty) {
            Some(TypeForm::Primitive)
        } else {
            None
        };
        ber_encode_tag(
            buf,
            tag,
            TagContext {
                is_outer_explicit: matches!(tag.kind, TagKind::Explicit(_)),
                ty: &resolved_type.ty,
            },
            form_override,
        );
    } else {
        assert!(
            matches!(resolved_type.ty, BuiltinType::Choice(_)),
            "ber_encode: resolved_type tag is None but type is not CHOICE"
        );
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::write_vlq;

    fn val_to_vlq(val: u64) -> Vec<u8> {
        let mut buf = Vec::with_capacity(10);
        write_vlq(val, &mut buf);
        buf.into_iter().rev().collect()
    }

    #[test]
    fn test_write_vlq() {
        assert_eq!(val_to_vlq(0), vec![0x00]);
        assert_eq!(val_to_vlq(1), vec![0x01]);
        assert_eq!(val_to_vlq(0x7f), vec![0x7f]);
        assert_eq!(val_to_vlq(0x80), vec![0x81, 0x00]);
        assert_eq!(val_to_vlq(0x81), vec![0x81, 0x01]);
        assert_eq!(val_to_vlq(0x3fff), vec![0xff, 0x7f]);
        assert_eq!(val_to_vlq(0x4000), vec![0x81, 0x80, 0x00]);
        assert_eq!(
            val_to_vlq(0xffff_ffff_ffff_ffff),
            vec![0x81, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f]
        );
    }
}
