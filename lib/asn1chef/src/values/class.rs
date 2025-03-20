use crate::{
    compiler::{
        parser::{AstElement, Error, ErrorKind, Result},
        Context,
    },
    module::QualifiedIdentifier,
    types::TaggedType,
};

use super::TypedValue;

#[derive(Debug, Clone)]
pub struct InformationObjectClassValue {
    pub fields: Vec<(String, ObjectClassValueField)>,
}

#[derive(Debug, Clone)]
pub enum ObjectClassValueField {
    Type(TaggedType),
    Value(AstElement<TypedValue>),
}

#[derive(Debug, Clone)]
pub enum ObjectClassReference {
    Value(InformationObjectClassValue),
    Reference(AstElement<QualifiedIdentifier>),
}

impl ObjectClassReference {
    pub fn resolve<'a>(&'a self, context: &'a Context) -> Result<&'a InformationObjectClassValue> {
        let mut classref = self;
        loop {
            match classref {
                Self::Value(value) => return Ok(value),
                Self::Reference(ident) => {
                    classref = context
                        .lookup_information_object_class_value(&ident.element)
                        .ok_or_else(|| Error {
                            kind: ErrorKind::Ast(format!(
                                "undefined reference to information object class value '{}'",
                                ident.element
                            )),
                            loc: ident.loc,
                        })?;
                }
            }
        }
    }
}
