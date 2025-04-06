use std::fmt::Display;

use num::BigInt;

mod simple;
pub use simple::*;

mod time;
pub use time::*;

mod special;
pub use special::*;

mod class;
pub use class::*;

use crate::{
    compiler::{
        parser::{AstElement, Error, ErrorKind, Result},
        Context,
    },
    module::QualifiedIdentifier,
    types::*,
};

#[derive(Debug, Clone)]
pub enum BuiltinValue {
    // simple built-in values
    Boolean(bool),
    Integer(BigInt),
    BitString(BitStringValue),
    OctetString(Vec<u8>),
    Null,
    ObjectIdentifier(ObjectIdentifier),
    RelativeOid(ObjectIdentifier),
    RealLiteral(RealLiteral),
    Enumerated(Box<AstElement<TypedValue>>),
    Time(Time),
    Structure(TagType, StructureValue),
    StructureOf(TagType, Vec<AstElement<TypedValue>>),
    Choice(ChoiceValue),
    CharacterString(TagType, String),
    UTCTime(UTCTime),
    GeneralizedTime(GeneralizedTime),
    Date(Date),
    TimeOfDay(TimeOfDay),
    DateTime(DateTime),
    Duration(Duration),

    // special values
    Containing(ContainingValue),
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
            Self::RelativeOid(_) => TagType::RelativeOid,
            Self::RealLiteral(_) => TagType::Real,
            Self::Enumerated(_) => TagType::Enumerated,
            Self::Time(_) => TagType::Time,
            Self::Structure(tag_type, _) | Self::StructureOf(tag_type, _) => *tag_type,
            Self::Choice(choice) => choice.value.resolve(context)?.value.tag_type(context)?,
            Self::CharacterString(tag_type, _) => *tag_type,
            Self::UTCTime(_) => TagType::UTCTime,
            Self::GeneralizedTime(_) => TagType::GeneralizedTime,
            Self::Date(_) => TagType::Date,
            Self::TimeOfDay(_) => TagType::TimeOfDay,
            Self::DateTime(_) => TagType::DateTime,
            Self::Duration(_) => TagType::Duration,
            Self::Containing(containing) => containing.container_type,
        })
    }
}

impl Display for BuiltinValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean(b) => f.write_str(match b {
                true => "TRUE",
                false => "FALSE",
            })?,
            Self::Integer(i) => f.write_str(&i.to_string())?,
            Self::BitString(bs) => f.write_fmt(format_args!("'{}'B", bs))?,
            Self::OctetString(bytes) => {
                f.write_fmt(format_args!("'{}'H", hex::encode_upper(bytes)))?
            }
            Self::Null => f.write_str("NULL")?,
            Self::ObjectIdentifier(oid) | Self::RelativeOid(oid) => oid.fmt(f)?,
            Self::RealLiteral(lit) => lit.fmt(f)?,
            Self::Enumerated(_) => todo!("ENUMERATED to string"),
            Self::Time(time) => f.write_fmt(format_args!("\"{}\"", time.source))?,
            Self::Structure(_, _) => f.write_str("{ ... }")?,
            Self::StructureOf(_, _) => f.write_str("{ ..., }")?,
            Self::Choice(choice) => f.write_fmt(format_args!(
                "{} : {}",
                choice.alternative.element, choice.value.element.value
            ))?,
            Self::CharacterString(_, str) => f.write_fmt(format_args!("\"{}\"", str))?,
            Self::UTCTime(time) => f.write_fmt(format_args!("\"{}\"", time.to_ber_string()))?,
            Self::GeneralizedTime(time) => {
                f.write_fmt(format_args!("\"{}\"", time.to_ber_string()))?
            }
            Self::Date(date) => f.write_fmt(format_args!("\"{}\"", date.to_ber_string()))?,
            Self::TimeOfDay(time_of_day) => {
                f.write_fmt(format_args!("\"{}\"", time_of_day.to_ber_string()))?
            }
            Self::DateTime(date_time) => {
                f.write_fmt(format_args!("\"{}\"", date_time.to_ber_string()))?
            }
            Self::Duration(duration) => f.write_fmt(format_args!("\"{}\"", duration.source))?,
            Self::Containing(containing) => f.write_fmt(format_args!(
                "CONTAINING {}",
                containing.value.element.value
            ))?,
        }

        Ok(())
    }
}

pub trait TryEq {
    fn try_eq(&self, context: &Context, rhs: &Self) -> Result<bool>;
}

#[derive(Debug, Clone)]
pub struct ResolvedValue {
    pub ty: ResolvedType,
    pub value: BuiltinValue,
}

impl TryEq for AstElement<ResolvedValue> {
    fn try_eq(&self, context: &Context, rhs: &Self) -> Result<bool> {
        let lhs = &self.element;
        let rhs = &rhs.element;
        Ok(match (&lhs.value, &rhs.value) {
            (BuiltinValue::Boolean(lhs), BuiltinValue::Boolean(rhs)) => lhs == rhs,
            (BuiltinValue::Integer(lhs), BuiltinValue::Integer(rhs)) => lhs == rhs,
            (BuiltinValue::BitString(lhs), BuiltinValue::BitString(rhs)) => lhs == rhs,
            (BuiltinValue::OctetString(lhs), BuiltinValue::OctetString(rhs)) => lhs == rhs,
            (BuiltinValue::Null, BuiltinValue::Null) => true,
            (BuiltinValue::ObjectIdentifier(lhs), BuiltinValue::ObjectIdentifier(rhs)) => {
                lhs.resolve_oid(context)? == rhs.resolve_oid(context)?
            }
            (BuiltinValue::Enumerated(lhs), BuiltinValue::Enumerated(rhs)) => {
                lhs.try_eq(context, rhs)?
            }
            (BuiltinValue::Choice(lhs), BuiltinValue::Choice(rhs)) => {
                lhs.value.try_eq(context, &rhs.value)?
            }
            (
                BuiltinValue::CharacterString(lhs_tag, lhs),
                BuiltinValue::CharacterString(rhs_tag, rhs),
            ) => lhs_tag == rhs_tag && lhs == rhs,
            _ => {
                return Err(Error {
                    kind: ErrorKind::Ast(format!(
                        "cannot compare values of types {} and {}",
                        lhs.value.tag_type(context)?,
                        rhs.value.tag_type(context)?
                    )),
                    loc: self.loc,
                })
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum ValueReference {
    BuiltinValue(BuiltinValue),
    Reference(QualifiedIdentifier),
}

impl Display for ValueReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BuiltinValue(builtin) => builtin.fmt(f),
            Self::Reference(reference) => reference.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedValue {
    pub resolved_type: ResolvedType,
    pub value: ValueReference,
}

pub trait ValueResolve {
    fn resolve<'a>(&'a self, context: &'a Context) -> Result<ResolvedValue>;
}

impl ValueResolve for AstElement<TypedValue> {
    fn resolve<'a>(&'a self, context: &'a Context) -> Result<ResolvedValue> {
        let mut valref = self;
        let mut tag = self.element.resolved_type.tag.as_ref();
        loop {
            match &valref.element.value {
                ValueReference::BuiltinValue(value) => {
                    let resolved_type = valref.element.resolved_type.clone();
                    return Ok(ResolvedValue {
                        ty: ResolvedType {
                            tag: tag.cloned(),
                            ty: resolved_type.ty,
                            constraints: resolved_type.constraints,
                        },
                        value: value.clone(),
                    });
                }
                ValueReference::Reference(ident) => {
                    let deref = &context
                        .lookup_value(ident)
                        .ok_or_else(|| Error {
                            kind: ErrorKind::Ast(format!(
                                "undefined reference to value '{}'",
                                ident
                            )),
                            loc: valref.loc,
                        })?
                        .value;
                    if !deref
                        .element
                        .resolved_type
                        .ty
                        .is_assignable_to(&valref.element.resolved_type.ty)
                        && deref
                            .element
                            .resolved_type
                            .tag
                            .as_ref()
                            .map(|tag| (&tag.class, &tag.num))
                            != valref
                                .element
                                .resolved_type
                                .tag
                                .as_ref()
                                .map(|tag| (&tag.class, &tag.num))
                    {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "valuereference '{}' of type {} cannot reference a value of type {}",
                                ident,
                                valref.element.resolved_type.ty,
                                deref.element.resolved_type.ty,
                            )),
                            loc: valref.loc,
                        });
                    }
                    valref = deref;
                    tag = tag.or(valref.element.resolved_type.tag.as_ref());
                }
            }
        }
    }
}

impl TryEq for AstElement<TypedValue> {
    fn try_eq(&self, context: &Context, rhs: &Self) -> Result<bool> {
        // fast path: if comparing two identical valuereferences, we know the underlying values are the same
        if let (ValueReference::Reference(ref1), ValueReference::Reference(ref2)) =
            (&self.element.value, &rhs.element.value)
        {
            if ref1 == ref2 {
                return Ok(true);
            }
        }

        let resolved_lhs = self.resolve(context)?;
        let resolved_rhs = rhs.resolve(context)?;
        AstElement::new(resolved_lhs, self.loc)
            .try_eq(context, &AstElement::new(resolved_rhs, rhs.loc))
    }
}

pub fn resolve_untyped(
    context: &Context,
    valref: AstElement<&QualifiedIdentifier>,
) -> Result<BuiltinValue> {
    let mut ident = valref;
    loop {
        let deref = &context
            .lookup_value(ident.element)
            .ok_or_else(|| Error {
                kind: ErrorKind::Ast(format!("undefined reference to value '{}'", ident.element)),
                loc: ident.loc,
            })?
            .value;
        match &deref.element.value {
            ValueReference::BuiltinValue(value) => {
                return Ok(value.clone());
            }
            ValueReference::Reference(valref) => {
                ident = AstElement::new(valref, deref.loc);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test::json_test;

    json_test!(
        test_encode_universal_types,
        "../../test-data/encode/UniversalTypeTest"
    );
    json_test!(
        test_encode_enumerated,
        "../../test-data/encode/EnumeratedTest"
    );
    json_test!(
        test_encode_choice_implicit_tagging,
        "../../test-data/encode/ChoiceTestImplicitTagging"
    );
    json_test!(
        test_encode_choice_automatic_tagging,
        "../../test-data/encode/ChoiceTestAutomaticTagging"
    );
    json_test!(
        test_encode_external_x680,
        "../../test-data/encode/External-X680Test"
    );
    json_test!(
        test_encode_embedded_pdv,
        "../../test-data/encode/EmbeddedPDVTest"
    );
    json_test!(
        test_encode_character_string,
        "../../test-data/encode/CharacterStringTest"
    );
    json_test!(test_encode_real, "../../test-data/encode/RealTest");
    json_test!(
        test_encode_contents_constraint,
        "../../test-data/encode/ContentsConstraintTest"
    );
    json_test!(
        test_encode_automatic_tagging,
        "../../test-data/encode/tagging/AutomaticTaggingTest"
    );
    json_test!(
        test_encode_explicit_tagging,
        "../../test-data/encode/tagging/ExplicitTaggingTest"
    );
    json_test!(
        test_encode_implicit_tagging,
        "../../test-data/encode/tagging/ImplicitTaggingTest"
    );
    json_test!(
        test_encode_type_parameter,
        "../../test-data/encode/TypeParameterTest"
    );
    json_test!(
        test_encode_value_parameter,
        "../../test-data/encode/ValueParameterTest"
    );
    json_test!(
        test_encode_bit_string,
        "../../test-data/encode/BitStringTest"
    );
    json_test!(
        test_encode_octet_string,
        "../../test-data/encode/OctetStringTest"
    );
    json_test!(
        test_encode_named_bit_string,
        "../../test-data/encode/NamedBitTest"
    );
    json_test!(
        test_encode_named_number_integer,
        "../../test-data/encode/NamedNumberTest"
    );
    json_test!(
        test_type_identifier,
        "../../test-data/encode/classes/TypeIdentifierTest"
    );
    json_test!(
        test_object_set_parameter,
        "../../test-data/encode/classes/ObjectSetParameterTest"
    );
    json_test!(
        test_object_class_with_object_field,
        "../../test-data/encode/classes/ObjectFieldTest"
    );
    json_test!(
        test_object_class_with_object_set_field,
        "../../test-data/encode/classes/ObjectSetFieldTest"
    );
    json_test!(
        test_instance_of,
        "../../test-data/encode/classes/InstanceOfTest"
    );
}
