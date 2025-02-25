use crate::{compiler::parser::AstElement, values::valref};

use super::{TagType, TaggedType};

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
    // Always either Sequence, SequenceOf, Set, or SetOf
    pub ty: TagType,
    pub components: Vec<StructureComponent>,
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
