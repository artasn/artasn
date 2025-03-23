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

impl InformationObject {
    pub fn find_field<'a>(&'a self, field_ref: &AstElement<String>) -> Result<&'a ObjectField> {
        self.fields
            .iter()
            .find_map(|(name, field)| {
                if name == &field_ref.element {
                    Some(field)
                } else {
                    None
                }
            })
            .ok_or_else(|| Error {
                kind: ErrorKind::Ast(format!(
                    "no such field '{}' in information object",
                    field_ref.element
                )),
                loc: field_ref.loc,
            })
    }
}

pub type InformationObjectSet = Vec<ObjectSetElement>;

#[derive(Debug, Clone)]
pub enum ObjectSetElement {
    Object(InformationObjectReference),
    ObjectSet(AstElement<QualifiedIdentifier>),
}

#[derive(Debug, Clone)]
pub enum ObjectField {
    Type(TaggedType),
    Value(AstElement<TypedValue>),
    ObjectFieldReference(ObjectFieldReference),
    Object(InformationObjectReference),
    ObjectSet(InformationObjectSet),
}

#[derive(Debug, Clone)]
pub struct ObjectFieldReference {
    pub object_ref: AstElement<QualifiedIdentifier>,
    pub field: AstElement<String>,
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
