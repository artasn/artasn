use crate::{compiler::parser::AstElement, values::valref};

use super::{typeref, TagType};

#[derive(Debug, Clone)]
pub struct StructureComponent {
    // Left blank for SequenceOf/SetOf
    pub name: AstElement<String>,
    pub component_type: Box<AstElement<typeref!()>>,
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
    pub ty: Box<AstElement<typeref!()>>,
}

#[derive(Debug, Clone)]
pub struct Choice {
    pub alternatives: Vec<ChoiceAlternative>,
}
