use crate::{compiler::parser::AstElement, values::Value};

#[derive(Debug, Clone)]
pub struct IntegerType {
    pub named_values: Option<Vec<NamedNumber>>,
}

#[derive(Debug, Clone)]
pub struct NamedNumber {
    pub name: AstElement<String>,
    pub value: AstElement<Value>,
}

#[derive(Debug, Clone)]
pub struct BitStringType {
    pub named_bits: Option<Vec<NamedNumber>>,
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
