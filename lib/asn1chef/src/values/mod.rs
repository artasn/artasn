mod simple;

use num::{bigint::Sign, BigInt, BigUint};
pub use simple::*;

use crate::{
    compiler::{
        context,
        parser::{AstElement, Error, ErrorKind, Loc, Result},
    },
    encoding,
    module::QualifiedIdentifier,
    types::*,
};

#[derive(Debug, Clone)]
pub enum Value {
    Boolean(bool),
    Integer(BigInt),
    BitString(BigUint),
    OctetString(Vec<u8>),
    Null,
    ObjectIdentifier(ObjectIdentifier),
    Real(f64),
    Enumerated(EnumeratedValue),
    Sequence(SequenceValue),
    SequenceOf(Vec<AstElement<valref!()>>),
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
            Self::Sequence(_) | Self::SequenceOf(_) => TagType::Sequence,
            Self::Choice(choice) => choice
                .value
                .resolve()
                .expect("CHOICE value failed to resolve")
                .tag_type(),
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Boolean(_) => "BOOLEAN",
            Self::Integer(_) => "INTEGER",
            Self::BitString(_) => "BIT STRING",
            Self::OctetString(_) => "OCTET STRING",
            Self::Null => "NULL",
            Self::ObjectIdentifier(_) => "OBJECT IDENTIFIER",
            Self::Real(_) => "REAL",
            Self::Enumerated(_) => "ENUMERATED",
            Self::Sequence(_) => "SEQUENCE",
            Self::SequenceOf(_) => "SEQUENCE OF",
            Self::Choice(_) => "CHOICE",
        }
    }

    /// Reverse-encodes the value, including its tag.
    /// The resulting bytes are in reverse order.
    /// The bytes of the final output must be reversed to be valid DER.
    pub fn der_encode(&self, buf: &mut Vec<u8>, tagged_type: &ResolvedType) -> Result<()> {
        let start_len = buf.len();
        match self {
            Self::ObjectIdentifier(oid) => {
                let oid = oid.resolve_oid()?.0;

                if oid.len() > 2 {
                    for node in oid.iter().skip(2).rev() {
                        encoding::write_vlq(*node, buf);
                    }
                } else if oid.len() < 2 {
                    return Err(Error {
                        kind: ErrorKind::Ast(
                            "illegal OBJECT IDENTIFIER with less than two nodes".to_string(),
                        ),
                        loc: Loc::default(),
                    });
                }

                let prefix = oid[0] * 40 + oid[1];
                encoding::write_vlq(prefix, buf);
            }
            Self::Integer(num) => {
                const SIGN_MASK: u8 = 0b1000_0000;

                let mut num = num.clone();
                let sign = num.sign();
                if sign == Sign::Minus {
                    // add one to the value for two's complement negative representation
                    num += 1;
                }

                if num == BigInt::ZERO {
                    if sign == Sign::Minus {
                        // fast encode for -1
                        buf.extend_from_slice(&[0xff]);
                    } else {
                        // fast encode for 0
                        buf.extend_from_slice(&[0x00]);
                    }
                } else {
                    let (_, mut bytes) = num.to_bytes_le();

                    // invert all bits when the number is negative
                    if sign == Sign::Minus {
                        for i in 0..bytes.len() {
                            bytes[i] = !bytes[i];
                        }
                    }

                    // write the bytes in little-endian order,
                    // such that when the DER is reversed after encoding,
                    // the bytes are in big-endian order
                    buf.extend_from_slice(&bytes);

                    let msb = bytes[bytes.len() - 1];
                    if sign != Sign::Minus && msb & SIGN_MASK == SIGN_MASK {
                        // when the sign bit is set in the msb, but the number is positive, add a padding byte without the sign bit
                        buf.extend_from_slice(&[0x00]);
                    } else if sign == Sign::Minus && msb & SIGN_MASK != SIGN_MASK {
                        // when the sign bit is not set in the msb, but the number is negative, add a padding byte containing the sign bit
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
                for component in sequence.components.iter().rev() {
                    // default values aren't encoded
                    if component.is_default {
                        continue;
                    }
                    let value = component.value.resolve()?;
                    let value_ty = ty_components
                        .iter()
                        .find(|ty_component| ty_component.name.element == component.name.element)
                        .expect("find type component matching value component for SEQUENCE")
                        .component_type
                        .resolve()?;
                    value.der_encode(buf, &value_ty)?;
                }
            }
            Self::SequenceOf(sequence) => {
                let component_type = match &tagged_type.ty {
                    BuiltinType::SequenceOf(seq_ty) => &seq_ty.component_type,
                    other => unreachable!("value is SequenceOf but type is {:?}", other),
                };
                let component_type = component_type.resolve()?;
                for element in sequence.iter().rev() {
                    let resolved = element.resolve()?;
                    resolved.der_encode(buf, &component_type)?;
                }
            }
            other => todo!("{:#02X?}", other),
        }

        let end_len = buf.len();
        if tagged_type.tag.kind == TagKind::Explicit {
            encoding::write_tlv_len((end_len - start_len) as u64, buf);
            Tag::default().der_encode(
                buf,
                TagContext {
                    is_outer_explicit: false,
                    ty: &tagged_type.ty,
                },
            );
        }

        let end_len = buf.len();
        encoding::write_tlv_len((end_len - start_len) as u64, buf);
        tagged_type.tag.der_encode(
            buf,
            TagContext {
                is_outer_explicit: tagged_type.tag.kind == TagKind::Explicit,
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
                        .value
                        .as_ref();
                }
            }
        };
        let tag_type = value.tag_type();
        let ref_type = TagType::try_from(TYPE_TAG as u16).expect("<const TYPE_TAG: u8> -> TypeTag");
        if !TagType::compare(tag_type, ref_type) {
            return Err(Error {
                kind: ErrorKind::Ast(format!("expecting {}, got {}", ref_type, tag_type)),
                loc: self.loc,
            });
        }
        Ok(value)
    }
}
