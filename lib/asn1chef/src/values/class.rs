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
pub struct InformationObject {
    pub fields: Vec<(String, ObjectField)>,
}

#[derive(Debug, Clone)]
pub enum ObjectField {
    Type(TaggedType),
    Value(AstElement<TypedValue>),
    Object(InformationObjectReference),
    ObjectSet(Vec<InformationObjectReference>),
}

#[derive(Debug, Clone)]
pub enum InformationObjectReference {
    Value(InformationObject),
    Reference(AstElement<QualifiedIdentifier>),
}

impl InformationObjectReference {
    pub fn resolve<'a>(&'a self, context: &'a Context) -> Result<&'a InformationObject> {
        let mut objectref = self;
        loop {
            match objectref {
                Self::Value(value) => return Ok(value),
                Self::Reference(ident) => {
                    objectref = context
                        .lookup_information_object(&ident.element)
                        .ok_or_else(|| Error {
                            kind: ErrorKind::Ast(format!(
                                "undefined reference to information object '{}'",
                                ident.element
                            )),
                            loc: ident.loc,
                        })?;
                }
            }
        }
    }
}
