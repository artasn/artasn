use crate::{compiler::parser::AstElement, values::valref};

use super::{Constraints, TagType, TaggedType};

#[derive(Debug, Clone)]
pub struct StructureComponent {
    // Left blank for SequenceOf/SetOf
    pub name: AstElement<String>,
    pub component_type: Box<TaggedType>,
    pub optional: bool,
    pub default_value: Option<Box<AstElement<valref!()>>>,
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
    pub size_constraints: Option<Constraints>,
    pub component_type: Box<TaggedType>,
}

#[derive(Debug, Clone)]
pub struct ChoiceAlternative {
    pub name: AstElement<String>,
    pub ty: Box<TaggedType>,
}

#[derive(Debug, Clone)]
pub struct Choice {
    pub alternatives: Vec<ChoiceAlternative>,
}
