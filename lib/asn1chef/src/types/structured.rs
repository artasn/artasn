use std::fmt::Display;

use crate::{
    compiler::{ast::types::LazyParsedDefaultValue, parser::AstElement},
    module::QualifiedIdentifier,
};

use super::{TagType, TaggedType};

pub trait ComponentLike: Clone {
    fn name(&self) -> &AstElement<String>;
    fn component_type(&self) -> &Box<TaggedType>;
}

#[derive(Debug, Clone)]
pub struct StructureComponent {
    pub name: AstElement<String>,
    pub component_type: Box<TaggedType>,
    pub optional: bool,
    pub default_value: Option<LazyParsedDefaultValue>,
}

impl ComponentLike for StructureComponent {
    fn name(&self) -> &AstElement<String> {
        &self.name
    }

    fn component_type(&self) -> &Box<TaggedType> {
        &self.component_type
    }
}

#[derive(Debug, Clone)]
pub struct ObjectClassFieldReference {
    pub class_type: AstElement<QualifiedIdentifier>,
    pub kind: ObjectClassFieldReferenceKind,
    pub field: AstElement<String>,
}

impl Display for ObjectClassFieldReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.class_type.element.name)?;
        f.write_str(".&")?;
        f.write_str(&self.field.element)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectClassFieldReferenceKind {
    TypeLike,
    ValueLike,
}

#[derive(Debug, Clone)]
pub struct Structure {
    // Always either Sequence or Set
    pub ty: TagType,
    pub components: Vec<StructureComponent>,
}

#[derive(Debug, Clone)]
pub struct StructureOf {
    // Always either Sequence or Set
    pub ty: TagType,
    pub component_type: Box<TaggedType>,
}

#[derive(Debug, Clone)]
pub struct ChoiceAlternative {
    pub name: AstElement<String>,
    pub alternative_type: Box<TaggedType>,
}

impl ComponentLike for ChoiceAlternative {
    fn name(&self) -> &AstElement<String> {
        &self.name
    }

    fn component_type(&self) -> &Box<TaggedType> {
        &self.alternative_type
    }
}

#[derive(Debug, Clone)]
pub struct Choice {
    pub alternatives: Vec<ChoiceAlternative>,
}
