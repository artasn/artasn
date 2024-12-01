use std::fmt::Display;

use anyhow::anyhow;

use crate::types::{TagType, TypeReference};

use super::{valref, Value};

#[derive(Debug, Clone)]
pub struct EnumeratedValue {
    pub ty: TypeReference<{ TagType::Enumerated as u8 }>,
    pub item: Box<valref!(Integer)>,
}

#[derive(Debug, Clone)]
pub struct SequenceValueComponent {
    pub name: String,
    pub value: valref!(),
}

#[derive(Debug, Clone)]
pub struct SequenceValue {
    pub components: Vec<SequenceValueComponent>,
}

#[derive(Debug, Clone)]
pub struct ChoiceValue {
    pub alternative: String,
    pub value: Box<valref!()>,
}

#[derive(Debug, Clone)]
pub struct ObjectIdentifier(pub Vec<ObjectIdentifierComponent>);

impl ObjectIdentifier {
    pub fn resolve_oid(&self) -> anyhow::Result<Oid> {
        let mut nodes = Vec::new();

        for (i, component) in self.0.iter().enumerate() {
            match component {
                ObjectIdentifierComponent::ValueReference(valref) => {
                    let resolved_value = valref.resolve()?;
                    match resolved_value {
                        Value::Integer(integer) => {
                            nodes.push(*integer as u64);
                        }
                        Value::ObjectIdentifier(relative_oid) => {
                            if i != 0 {
                                return Err(anyhow!("relative OBJECT IDENTIFIER can only be used as the first component of another OBJECT IDENTIFIER"));
                            }
                            let resolved_oid = relative_oid.resolve_oid()?;
                            nodes.extend(resolved_oid.0);
                        }
                        other => {
                            return Err(anyhow!(
                                "expecting OBJECT IDENTIFIER or INTEGER; found {}",
                                other.tag_type()
                            ))
                        }
                    }
                }
                ObjectIdentifierComponent::IntegerLiteral(lit) => {
                    nodes.push(*lit);
                }
            }
        }

        Ok(Oid(nodes))
    }
}

#[derive(Debug, Clone)]
pub enum ObjectIdentifierComponent {
    /// Must be either an `OBJECT IDENTIFIER` or `INTEGER` value.
    /// This will be verified when `ObjectIdentifier::resolve_oid` is invoked.
    ValueReference(valref!()),
    IntegerLiteral(u64),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Oid(pub Vec<u64>);

impl Display for Oid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.len() < 2 {
            panic!("illegal oid (must have at least 2 nodes): {:?}", self);
        }

        for i in 0..self.0.len() - 1 {
            f.write_fmt(format_args!("{}.", self.0[i]))?;
        }

        f.write_fmt(format_args!("{}", self.0[self.0.len() - 1]))?;

        Ok(())
    }
}
