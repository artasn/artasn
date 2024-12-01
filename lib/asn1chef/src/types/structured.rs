use crate::values::valref;

use super::{typeref, TagType};

#[derive(Debug, Clone)]
pub struct StructureComponent {
    // Left blank for SequenceOf/SetOf
    pub name: String,
    pub component_type: Box<typeref!()>,
    pub optional: bool,
    pub default_value: Option<Box<valref!()>>,
}

#[derive(Debug, Clone)]
pub struct Structure {
    // Always either Sequence, SequenceOf, Set, or SetOf
    pub ty: TagType,
    pub components: Vec<StructureComponent>,
}

#[derive(Debug, Clone)]
pub struct ChoiceAlternative {
    pub name: String,
    pub ty: Box<typeref!()>,
}

#[derive(Debug, Clone)]
pub struct Choice {
    pub alternatives: Vec<ChoiceAlternative>,
}
