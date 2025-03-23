use crate::{
    compiler::{parser::*, Context},
    module::QualifiedIdentifier,
    values::TypedValue,
};

use super::{ObjectClassFieldReference, TaggedType};

#[derive(Debug, Clone)]
pub struct InformationObjectClass {
    pub name: AstElement<String>,
    pub fields: Vec<(AstElement<String>, ObjectClassField)>,
    pub syntax: Vec<ObjectClassSyntaxNodeGroup>,
    /// True if ast::register_all_information_object_classes has finished.
    /// Otherwise false (i.e. ast::register_all_information_object_class_names has finished, but ast::register_all_information_object_classes has not yet been called.)
    pub parsed: bool,
}

impl InformationObjectClass {
    pub fn find_field<'a>(
        &'a self,
        field_ref: &AstElement<String>,
    ) -> Result<&'a ObjectClassField> {
        self.fields
            .iter()
            .find_map(|(name, field)| {
                if name.element == field_ref.element {
                    Some(field)
                } else {
                    None
                }
            })
            .ok_or_else(|| Error {
                kind: ErrorKind::Ast(format!(
                    "no such field '{}' in information object class type '{}'",
                    field_ref.element, self.name.element,
                )),
                loc: field_ref.loc,
            })
    }
}

#[derive(Debug, Clone)]
pub enum ObjectClassField {
    Value(ObjectClassFieldValue),
    OpenType(ObjectClassFieldType),
    Object(ObjectClassFieldObject),
    ObjectSet(ObjectClassFieldObject),
}

#[derive(Debug, Clone)]
pub struct ObjectClassFieldValue {
    pub field_type: ObjectClassFieldValueType,
    pub option: Option<ObjectClassFieldValueOption>,
}

#[derive(Debug, Clone)]
pub enum ObjectClassFieldValueType {
    TaggedType(TaggedType),
    ObjectClassFieldReference(ObjectClassFieldReference),
    OpenTypeReference(AstElement<String>),
}

#[derive(Debug, Clone)]
pub enum ObjectClassFieldValueOption {
    Unique,
    Optional,
    Default(AstElement<TypedValue>),
}

#[derive(Debug, Clone)]
pub struct ObjectClassFieldType {
    pub optional: bool,
}

#[derive(Debug, Clone)]
pub struct ObjectClassFieldObject {
    pub class: AstElement<QualifiedIdentifier>,
    pub optional: bool,
}

#[derive(Debug, Clone)]
pub enum ObjectClassSyntaxNodeGroup {
    Required(ObjectClassSyntaxNode),
    Optional(Vec<ObjectClassSyntaxNodeGroup>),
}

#[derive(Debug, Clone)]
pub struct ObjectClassSyntaxNode {
    pub kind: ObjectClassSyntaxNodeKind,
    /// When kind is TypeField, associated_data is the name of the open type field.
    /// When kind is ValueField, associated_data is the name of the value field.
    /// When kind is TokenLiteral, associated_data is the text of the literal.
    ///
    /// For example:
    ///
    /// ```asn1
    /// WITH SYNTAX {
    ///   &Type [, WITH PROPERTY &prop]
    /// }
    /// ```
    /// When parsed, this returns the following psuedo-syntax:
    ///
    /// ```text
    /// vec![
    ///   ObjectClassSyntaxNodeGroup::Required(
    ///     ObjectClassSyntaxNode(kind = TypeField,    associated_data = "Type"),
    ///   ),
    ///   ObjectClassSyntaxNodeGroup::Optional(vec![
    ///     ObjectClassSyntaxNode(kind = TokenLiteral, associated_data = ","),
    ///     ObjectClassSyntaxNode(kind = TokenLiteral, associated_data = "WITH"),
    ///     ObjectClassSyntaxNode(kind = TokenLiteral, associated_data = "PROPERTY"),
    ///     ObjectClassSyntaxNode(kind = ValueField,   associated_data = "prop"),
    ///   ]),
    /// ]
    /// ```
    pub associated_data: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectClassSyntaxNodeKind {
    TypeField,
    ValueField,
    TokenLiteral,
}

#[derive(Debug, Clone)]
pub enum InformationObjectClassReference {
    Class(InformationObjectClass),
    Reference(AstElement<QualifiedIdentifier>),
}

impl InformationObjectClassReference {
    pub fn resolve_reference<'a>(
        &self,
        context: &'a Context,
    ) -> Result<&'a InformationObjectClass> {
        let mut classref = match self {
            Self::Class(_) => panic!("resolve_reference on Class"),
            Self::Reference(ident) => ident,
        };
        loop {
            let resolved = context
                .lookup_information_object_class(&classref.element)
                .ok_or_else(|| Error {
                    kind: ErrorKind::Ast(format!(
                        "undefined reference to information object class '{}'",
                        classref.element
                    )),
                    loc: classref.loc,
                })?;
            match resolved {
                Self::Class(class) => return Ok(class),
                Self::Reference(ident) => classref = ident,
            }
        }
    }

    pub fn resolve<'a>(&'a self, context: &'a Context) -> Result<&'a InformationObjectClass> {
        match self {
            Self::Class(class) => Ok(class),
            Self::Reference(_) => self.resolve_reference(context),
        }
    }
}
