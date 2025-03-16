use num::{BigInt, BigUint};

mod simple;
pub use simple::*;

mod time;
pub use time::*;

mod special;
pub use special::*;

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
    BitString(BigUint),
    OctetString(Vec<u8>),
    Null,
    ObjectIdentifier(ObjectIdentifier),
    RelativeOid(ObjectIdentifier),
    RealLiteral(RealLiteral),
    Enumerated(Box<AstElement<Value>>),
    Time(Time),
    Sequence(StructureValue),
    SequenceOf(Vec<AstElement<Value>>),
    Set(StructureValue),
    SetOf(Vec<AstElement<Value>>),
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
            Self::Sequence(_) | Self::SequenceOf(_) => TagType::Sequence,
            Self::Set(_) | Self::SetOf(_) => TagType::Set,
            Self::Choice(choice) => choice.value.resolve(context)?.tag_type(context)?,
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

#[derive(Debug, Clone)]
pub enum Value {
    BuiltinValue(BuiltinValue),
    Reference(QualifiedIdentifier),
}

pub trait ValueResolve {
    fn resolve<'a>(&'a self, context: &'a Context) -> Result<&'a BuiltinValue>;
    fn try_eq(&self, context: &Context, rhs: &Self) -> Result<bool>;
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

    fn try_eq(&self, context: &Context, rhs: &Self) -> Result<bool> {
        let lhs = self.resolve(context)?;
        let rhs = rhs.resolve(context)?;
        Ok(match (lhs, rhs) {
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
                        lhs.tag_type(context)?,
                        rhs.tag_type(context)?
                    )),
                    loc: self.loc,
                })
            }
        })
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test::json_test;

    json_test!(test_encode_universal_types, "../../test-data/encode/UniversalTypeTest");
    json_test!(test_encode_enumerated, "../../test-data/encode/EnumeratedTest");
    json_test!(
        test_encode_choice_implicit_tagging,
        "../../test-data/encode/ChoiceTestImplicitTagging"
    );
    json_test!(
        test_encode_choice_automatic_tagging,
        "../../test-data/encode/ChoiceTestAutomaticTagging"
    );
    json_test!(test_encode_external_x680, "../../test-data/encode/External-X680Test");
    json_test!(test_encode_embedded_pdv, "../../test-data/encode/EmbeddedPDVTest");
    json_test!(test_encode_character_string, "../../test-data/encode/CharacterStringTest");
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
    // json_test!(
    //     test_encode_value_parameter,
    //     "../../test-data/encode/ValueParameterTest"
    // );
}
