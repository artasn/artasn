use num::{BigInt, BigUint};

mod simple;
pub use simple::*;

mod time;
pub use time::*;

use crate::{
    compiler::{
        parser::{AstElement, Error, ErrorKind, Loc, Result},
        Context,
    },
    encoding,
    module::QualifiedIdentifier,
    types::*,
};

#[derive(Debug, Clone)]
pub enum BuiltinValue {
    Boolean(bool),
    Integer(BigInt),
    BitString(BigUint),
    OctetString(Vec<u8>),
    Null,
    ObjectIdentifier(ObjectIdentifier),
    Real(f64),
    Enumerated(Box<AstElement<Value>>),
    Time(Time),
    Sequence(StructureValue),
    SequenceOf(Vec<AstElement<Value>>),
    Set(StructureValue),
    SetOf(Vec<AstElement<Value>>),
    Choice(ChoiceValue),
    CharacterString(TagType, String),
    UTCTime(UTCTime),
    // GeneralizedTime(GeneralizedTime),
    Date(Date),
    TimeOfDay(TimeOfDay),
    DateTime(DateTime),
    Duration(Duration),
}

impl BuiltinValue {
    pub fn tag_type(&self, context: &Context) -> Result<TagType> {
        Ok(match self {
            Self::Boolean(_) => TagType::Boolean,
            Self::Integer(_) => TagType::Integer,
            Self::BitString(_) => TagType::BitString,
            Self::OctetString(_) => TagType::OctetString,
            Self::Null => TagType::Null,
            Self::ObjectIdentifier(_) => TagType::ObjectIdentifier,
            Self::Real(_) => TagType::Real,
            Self::Enumerated(_) => TagType::Enumerated,
            Self::Time(_) => TagType::Time,
            Self::Sequence(_) | Self::SequenceOf(_) => TagType::Sequence,
            Self::Set(_) | Self::SetOf(_) => TagType::Set,
            Self::Choice(choice) => choice.value.resolve(context)?.tag_type(context)?,
            Self::CharacterString(tag_type, _) => *tag_type,
            Self::UTCTime(_) => TagType::UTCTime,
            Self::Date(_) => TagType::Date,
            Self::TimeOfDay(_) => TagType::TimeOfDay,
            Self::DateTime(_) => TagType::DateTime,
            Self::Duration(_) => TagType::Duration,
        })
    }

    /// Reverse-encodes the value, including its tag.
    /// The resulting bytes are in reverse order.
    /// The bytes of the final output must be reversed to be valid DER.
    pub fn der_encode(
        &self,
        buf: &mut Vec<u8>,
        context: &Context,
        resolved_type: &ResolvedType,
    ) -> Result<()> {
        let start_len = buf.len();
        match self {
            Self::Boolean(b) => {
                if *b {
                    buf.push(0xff);
                } else {
                    buf.push(0x00);
                }
            }
            Self::Integer(num) => encoding::der_encode_integer(buf, num.clone()),
            Self::BitString(bit_string) => {
                let le_bytes = bit_string.to_bytes_le();
                buf.extend(&le_bytes);
                buf.push(0x00); // TODO: implement support for bit strings with bit length not a multiple of 8
            }
            Self::OctetString(octet_string) => buf.extend(octet_string.iter().rev()),
            Self::Null => (),
            Self::ObjectIdentifier(oid) => {
                let oid = oid.resolve_oid(context)?.0;

                match oid.len() {
                    2.. => {
                        for node in oid.iter().skip(2).rev() {
                            encoding::write_vlq(*node, buf);
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
                encoding::write_vlq(prefix, buf);
            }
            Self::Enumerated(enumerated) => {
                let resolved = enumerated.resolve(context)?;
                let num = match resolved {
                    Self::Integer(num) => num.clone(),
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
                encoding::der_encode_integer(buf, num);
            }
            Self::Time(time) => {
                buf.extend(time.to_ber_string().into_bytes().into_iter().rev());
            }
            Self::Sequence(structure) | Self::Set(structure) => {
                // ast.rs guarantees all components in SEQUENCE/SET type are provided in value,
                // and that the value provides only components in the SEQUENCE/SET type,
                // and that the component values are in the same order as in the type definition
                let ty_components = match &resolved_type.ty {
                    BuiltinType::Structure(structure) => &structure.components,
                    _ => unreachable!(),
                };
                for component in structure.components.iter().rev() {
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
            }
            Self::SequenceOf(structure) | Self::SetOf(structure) => {
                let component_type = match &resolved_type.ty {
                    BuiltinType::StructureOf(structure_ty) => &structure_ty.component_type,
                    other => unreachable!("value is SequenceOf but type is {:?}", other),
                };
                let component_type = component_type.resolve(context)?;
                for element in structure.iter().rev() {
                    let resolved = element.resolve(context)?;
                    resolved.der_encode(buf, context, &component_type)?;
                }
            }
            Self::Choice(choice) => {
                let value = choice.value.resolve(context)?;
                value.der_encode(buf, context, &choice.alternative_type)?;
            }
            Self::CharacterString(tag_type, str) => {
                encoding::der_encode_character_string(buf, *tag_type, str);
            }
            Self::UTCTime(utc) => {
                buf.extend(utc.to_ber_string().into_bytes().into_iter().rev());
            }
            Self::Date(date) => {
                buf.extend(date.to_ber_string().into_bytes().into_iter().rev());
            }
            Self::TimeOfDay(time_of_day) => {
                buf.extend(time_of_day.to_ber_string().into_bytes().into_iter().rev());
            }
            Self::DateTime(date_time) => {
                buf.extend(date_time.to_ber_string().into_bytes().into_iter().rev());
            }
            Self::Duration(duration) => {
                buf.extend(duration.to_ber_string().into_bytes().into_iter().rev());
            }
            other => todo!("{:#02X?}", other),
        }

        let tag = resolved_type.tag.as_ref();
        if let Some(tag) = tag {
            let end_len = buf.len();
            if tag.kind == TagKind::Explicit {
                if let Some(tag_type) = resolved_type.ty.tag_type() {
                    encoding::write_tlv_len((end_len - start_len) as u64, buf);
                    Tag::universal(tag_type).der_encode(
                        buf,
                        TagContext {
                            is_outer_explicit: false,
                            ty: &resolved_type.ty,
                        },
                    );
                }
            }

            let end_len = buf.len();
            encoding::write_tlv_len((end_len - start_len) as u64, buf);
            tag.der_encode(
                buf,
                TagContext {
                    is_outer_explicit: tag.kind == TagKind::Explicit,
                    ty: &resolved_type.ty,
                },
            );
        } else {
            assert!(
                matches!(resolved_type.ty, BuiltinType::Choice(_)),
                "der_encode: resolved_type tag is None but type is not CHOICE"
            );
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    BuiltinValue(BuiltinValue),
    Reference(QualifiedIdentifier),
}

pub trait ValueResolve {
    fn resolve<'a>(&'a self, context: &'a Context) -> Result<&'a BuiltinValue>;
}

impl ValueResolve for AstElement<Value> {
    fn resolve<'a>(&'a self, context: &'a Context) -> Result<&'a BuiltinValue> {
        let mut valref = self;
        loop {
            match &valref.element {
                Value::BuiltinValue(value) => return Ok(value),
                Value::Reference(ident) => {
                    valref = &context
                        .lookup_value(ident)
                        .ok_or_else(|| Error {
                            kind: ErrorKind::Ast(format!(
                                "undefined reference to value '{}'",
                                ident
                            )),
                            loc: valref.loc,
                        })?
                        .value;
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test;

    #[test]
    pub fn test_encode_universal_types() {
        let module_file = include_str!("../../test-data/encode/UniversalTypeTest.asn");
        let test_file = include_str!("../../test-data/encode/UniversalTypeTest.test.json");

        test::execute_json_test(module_file, test_file);
    }

    #[test]
    pub fn test_encode_enumerated() {
        let module_file = include_str!("../../test-data/encode/EnumeratedTest.asn");
        let test_file = include_str!("../../test-data/encode/EnumeratedTest.test.json");

        test::execute_json_test(module_file, test_file);
    }

    #[test]
    pub fn test_encode_choice_implicit_tagging() {
        let module_file = include_str!("../../test-data/encode/ChoiceTestImplicitTagging.asn");
        let test_file = include_str!("../../test-data/encode/ChoiceTestImplicitTagging.test.json");

        test::execute_json_test(module_file, test_file);
    }

    #[test]
    pub fn test_encode_choice_automatic_tagging() {
        let module_file = include_str!("../../test-data/encode/ChoiceTestAutomaticTagging.asn");
        let test_file = include_str!("../../test-data/encode/ChoiceTestAutomaticTagging.test.json");

        test::execute_json_test(module_file, test_file);
    }
}
