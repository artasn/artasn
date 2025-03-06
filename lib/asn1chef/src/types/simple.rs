use crate::{compiler::parser::AstElement, values::Value};

use super::Constraints;

#[derive(Debug, Clone)]
pub struct IntegerType {
    pub named_values: Option<Vec<NamedNumber>>,
    pub value_constraints: Option<Constraints>,
}

#[derive(Debug, Clone)]
pub struct NamedNumber {
    pub name: AstElement<String>,
    pub value: AstElement<Value>,
}

#[derive(Debug, Clone)]
pub struct BitStringType {
    pub named_bits: Option<Vec<NamedNumber>>,
    pub size_constraints: Option<Constraints>,
}

#[derive(Debug, Clone)]
pub struct OctetStringType {
    pub size_constraints: Option<Constraints>,
}

pub type EnumeratedType = Vec<EnumerationItem>;

#[derive(Debug, Clone)]
pub enum EnumerationItemValue {
    Specified(AstElement<Value>),
    Implied(i64),
}

#[derive(Debug, Clone)]
pub struct EnumerationItem {
    pub name: AstElement<String>,
    pub value: EnumerationItemValue,
}
