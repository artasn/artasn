mod simple;

use num::BigUint;
pub use simple::*;

use crate::{
    compiler::{
        context, encode,
        parser::{AstElement, Error, ErrorKind, Result},
    },
    module::QualifiedIdentifier,
    types::*,
};

#[derive(Debug, Clone)]
pub enum Value {
    Boolean(bool),
    Integer(i64),
    BitString(BigUint),
    OctetString(Vec<u8>),
    Null,
    ObjectIdentifier(ObjectIdentifier),
    Real(f64),
    Enumerated(EnumeratedValue),
    Sequence(SequenceValue),
    Choice(ChoiceValue),
}

impl Value {
    pub fn tag_type(&self) -> TagType {
        match self {
            Self::Boolean(_) => TagType::Boolean,
            Self::Integer(_) => TagType::Integer,
            Self::BitString(_) => TagType::BitString,
            Self::OctetString(_) => TagType::OctetString,
            Self::Null => TagType::Null,
            Self::ObjectIdentifier(_) => TagType::ObjectIdentifier,
            Self::Real(_) => TagType::Real,
            Self::Enumerated(_) => TagType::Enumerated,
            Self::Sequence(_) => TagType::Sequence,
            Self::Choice(choice) => choice
                .value
                .resolve()
                .expect("CHOICE value failed to resolve")
                .tag_type(),
        }
    }

    /// Reverse-encodes the value, including its tag.
    /// The resulting bytes are in reverse order.
    /// The bytes of the final output must be reversed to be valid DER.
    pub fn der_encode(
        &self,
        buf: &mut Vec<u8>,
        tagged_type: &ResolvedType,
        structure_component_index: Option<u8>,
    ) -> Result<()> {
        let start_len = buf.len();
        match self {
            Self::ObjectIdentifier(oid) => {
                let oid = oid.resolve_oid()?.0;

                if oid.len() > 2 {
                    for node in oid.iter().skip(2).rev() {
                        encode::write_vlq(*node, buf);
                    }
                }

                let prefix = oid[0] * 40 + oid[1];
                encode::write_vlq(prefix, buf);
            }
            Self::Integer(num) => {
                const SIGN_MASK: u8 = 0b1000_0000;
                let num = *num;
                if num == 0 {
                    buf.extend_from_slice(&[0x00]);
                } else if num == -1 {
                    buf.extend_from_slice(&[0xff]);
                } else {
                    let prefix = if num.is_negative() { 0xff } else { 0x00 };
                    let le_bytes = num.to_le_bytes();
                    let mut msb_index = le_bytes.len() - 1;
                    while le_bytes[msb_index] == prefix {
                        msb_index -= 1;
                    }
                    buf.extend_from_slice(&le_bytes[..msb_index + 1]);
                    if num > 0 && le_bytes[msb_index] & SIGN_MASK == SIGN_MASK {
                        // if the number is positive and the leftmost bit of the msb is 1, add a padding byte of zeroes
                        // this ensures the sign bit is a zero
                        buf.extend_from_slice(&[0x00]);
                    } else if num < 0 && le_bytes[msb_index] & SIGN_MASK != SIGN_MASK {
                        // if the number is negative and the leftmost bit of the msb is 0, add a padding byte of ones
                        // this ensures the sign bit is a one
                        buf.extend_from_slice(&[0xff]);
                    }
                }
            }
            Self::OctetString(octet_string) => buf.extend(octet_string.iter().rev()),
            Self::Sequence(sequence) => {
                // ast.rs guarantees all components in SEQUENCE type are provided in value,
                // and that the value provides only components in the SEQUENCE type,
                // and that the component values are in the same order as in the type definition
                let ty_components = match &tagged_type.ty {
                    BuiltinType::Sequence(seq) => &seq.components,
                    _ => unreachable!(),
                };
                for (index, component) in sequence.components.iter().enumerate().rev() {
                    let value = component.value.resolve()?;
                    let value_ty = ty_components
                        .iter()
                        .find(|ty_component| ty_component.name.element == component.name.element)
                        .unwrap()
                        .component_type
                        .resolve()?;
                    value.der_encode(buf, &value_ty, Some(index as u8))?;
                }
            }
            other => todo!("{:#02X?}", other),
        }

        let end_len = buf.len();
        if tagged_type.tag.kind == TagKind::Explicit {
            encode::write_vlq((end_len - start_len) as u64, buf);
            Tag::default().ber_encode(
                buf,
                TagContext {
                    is_outer_explicit: false,
                    structure_component_index,
                    ty: &tagged_type.ty,
                },
            );
        }

        let end_len = buf.len();
        encode::write_vlq((end_len - start_len) as u64, buf);
        tagged_type.tag.ber_encode(
            buf,
            TagContext {
                is_outer_explicit: tagged_type.tag.kind == TagKind::Explicit,
                structure_component_index,
                ty: &tagged_type.ty,
            },
        );

        Ok(())
    }

    // pub fn der_encode(&self, writer: io::BufWriter<Vec<u8>>) {
    //     match self {
    //         Self::BitString(uint_data) => {

    //         }
    //         Self::OctetString(binary) => {
    //             let (bit_modulus, pad_size) = match self. {
    //                 StringKind::BString => (8, 1),
    //                 StringKind::HString => (2, 4),
    //                 _ => unreachable!(),
    //             };
    //             let pad_len = match sbits.len() {
    //                 0 => match from_literal {
    //                     StringKind::BString => 8,
    //                     StringKind::HString => {
    //                         sbits = "";
    //                         0
    //                     }
    //                     _ => unreachable!(),
    //                 },
    //                 len => {
    //                     let padding = bit_modulus - (len % bit_modulus);
    //                     if padding == bit_modulus {
    //                         0
    //                     } else {
    //                         padding
    //                     }
    //                 }
    //             };
    //             let mut dbits = String::with_capacity(sbits.len() + pad_len);
    //             dbits.write_str(&sbits);
    //             for _ in 0..pad_len {
    //                 dbits.write_char('0');
    //             }
    //             println!(
    //                 "sbits: {:X?}, dbits: {:X?}, pad_size={pad_size:} pad_len={pad_len:}",
    //                 sbits, dbits
    //             );

    //             let big = match BigUint::from_str_radix(
    //                 &dbits,
    //                 match from_literal {
    //                     StringKind::BString => 2,
    //                     StringKind::HString => 16,
    //                     _ => unreachable!(),
    //                 },
    //             ) {
    //                 Ok(parsed) => parsed,
    //                 Err(err) => {
    //                     if format!("{}", err) == "cannot parse integer from empty string" {
    //                         BigUint::ZERO
    //                     } else {
    //                         panic!("{:?}", err);
    //                     }
    //                 }
    //             };

    //             let mut buf: Vec<u8> = Vec::new();
    //             buf.push(to_tag as u8);
    //             let be_bytes = big.to_bytes_be();
    //             let len = be_bytes.len();
    //             if to_tag == TagType::BitString {
    //                 if pad_len == 8 {
    //                     buf.push(1);
    //                 } else {
    //                     buf.push(1 + len as u8); // L (pad-len + big-endian bytes)
    //                     buf.push((pad_len * pad_size) as u8);
    //                 }
    //             } else if to_tag == TagType::OctetString {
    //                 if len > 0 {
    //                     buf.push(len as u8);
    //                 }
    //             }
    //             buf.extend(be_bytes);

    //             writer.write_all(&buf).unwrap();
    //         }
    //     }
    // }
}

macro_rules! valref {
    () => {
        crate::values::ValueReference<{crate::types::TagType::Any as u8}>
    };
    ($tag:ident) => {
        crate::values::ValueReference<{crate::types::TagType::$tag as u8}>
    }
}
pub(crate) use valref;

#[derive(Debug, Clone)]
pub enum ValueReference<const TYPE_TAG: u8> {
    Value(Value),
    Reference(QualifiedIdentifier),
}

impl<const TYPE_TAG: u8> ValueReference<TYPE_TAG> {
    /// Casts `&'a self` to `&'a ValueReference<{ TagType::Any as u8 }>`.
    ///
    /// See [`ValueReference::as_type`] for implementation details.
    pub fn as_any(&self) -> &ValueReference<{ TagType::Any as u8 }> {
        self.as_type()
    }

    /// Casts `&'a self` to `&'a ValueReference<NEW_TAG>`.
    ///
    /// The returned reference is identical to `&self` (including pointing to the same memory),
    /// except for the modified compile-time `<const TAG_TYPE: u8>` generic parameter.
    pub fn as_type<const NEW_TAG: u8>(&self) -> &ValueReference<NEW_TAG> {
        // since the `<TYPE_TAG>` generic parameter type is only present at compile-time,
        // we can safely transmute a &ValueReference<X> -> &ValueReference<Y>,
        // because the runtime memory representations of both these types are identical
        unsafe { std::mem::transmute(self) }
    }
}

pub trait ValueResolve {
    fn resolve(&self) -> Result<&Value>;
}

impl<const TYPE_TAG: u8> ValueResolve for AstElement<ValueReference<TYPE_TAG>> {
    fn resolve(&self) -> Result<&Value> {
        let mut valref = self.as_ref().map(|valref| valref.as_any());
        let value = loop {
            match &valref.element {
                ValueReference::<{ TagType::Any as u8 }>::Value(value) => break value,
                ValueReference::<{ TagType::Any as u8 }>::Reference(ident) => {
                    valref = context()
                        .lookup_value(ident)
                        .ok_or_else(|| Error {
                            kind: ErrorKind::Ast(format!("undefined reference to type '{ident}")),
                            loc: self.loc,
                        })?
                        .value.as_ref();
                }
            }
        };
        let tag_type = value.tag_type();
        let ref_type = TagType::try_from(TYPE_TAG).unwrap();
        if !TagType::compare(tag_type, ref_type) {
            return Err(Error {
                kind: ErrorKind::Ast(format!("expecting {}, got {}", ref_type, tag_type)),
                loc: self.loc,
            });
        }
        Ok(value)
    }
}
