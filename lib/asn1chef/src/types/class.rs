use crate::{compiler::parser::AstElement, values::TypedValue};

use super::TaggedType;

#[derive(Debug, Clone)]
pub struct InformationObjectClass {
    pub fields: Vec<(AstElement<String>, ObjectClassField)>,
    pub syntax: Vec<ObjectClassSyntaxNodeGroup>,
}

#[derive(Debug, Clone)]
pub enum ObjectClassField {
    Value(ObjectClassFieldValue),
    OpenType(ObjectClassFieldType),
}

#[derive(Debug, Clone)]
pub struct ObjectClassFieldValue {
    pub field_type: TaggedType,
    pub option: Option<ObjectClassFieldValueOption>,
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
pub enum ObjectClassSyntaxNodeGroup {
    Required(ObjectClassSyntaxNode),
    Optional(Vec<ObjectClassSyntaxNode>),
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
