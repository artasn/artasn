use crate::{compiler::parser::AstElement, types::TagType};

use super::Value;

#[derive(Debug, Clone)]
pub struct ContainingValue {
    /// Always either [`TagType::BitString`] or [`TagType::OctetString`].
    pub container_type: TagType,
    pub value: Box<AstElement<Value>>,
}
