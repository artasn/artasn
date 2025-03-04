use std::fmt::Display;

use num::bigint::Sign;

use crate::{
    compiler::parser::{AstElement, Error, ErrorKind, Result},
    types::TaggedType,
};

use super::{valref, Value, ValueResolve};

#[derive(Debug, Clone)]
pub struct EnumeratedValue {
    pub ty: TaggedType,
    pub item: Box<AstElement<valref!(Integer)>>,
}

#[derive(Debug, Clone)]
pub struct SequenceValueComponent {
    pub name: AstElement<String>,
    pub value: AstElement<valref!()>,
    pub is_default: bool,
}

#[derive(Debug, Clone)]
pub struct StructureValue {
    pub components: Vec<SequenceValueComponent>,
}

#[derive(Debug, Clone)]
pub struct ChoiceValue {
    pub alternative: AstElement<String>,
    pub value: Box<AstElement<valref!()>>,
}

#[derive(Debug, Clone)]
pub struct ObjectIdentifier(pub Vec<ObjectIdentifierComponent>);

impl ObjectIdentifier {
    pub fn resolve_oid(&self) -> Result<Oid> {
        let mut nodes = Vec::new();

        for (i, component) in self.0.iter().enumerate() {
            match component {
                ObjectIdentifierComponent::ValueReference(valref) => {
                    let resolved_value = valref.resolve()?;
                    match resolved_value {
                        Value::Integer(integer) => {
                            let (sign, digits) = integer.to_u64_digits();
                            if sign == Sign::Minus {
                                return Err(Error {
                                    kind: ErrorKind::Ast(
                                        "OBJECT IDENTIFIER node must be a non-negative integer"
                                            .to_string(),
                                    ),
                                    loc: valref.loc,
                                });
                            }
                            if digits.len() > 1 {
                                return Err(Error {
                                    kind: ErrorKind::Ast(
                                        "OBJECT IDENTIFIER node is too large to fit in a 64-bit unsigned integer"
                                            .to_string(),
                                    ),
                                    loc: valref.loc,
                                });
                            }
                            nodes.push(digits[0]);
                        }
                        Value::ObjectIdentifier(relative_oid) => {
                            if i != 0 {
                                return Err(Error{
                                    kind: ErrorKind::Ast("relative OBJECT IDENTIFIER can only be used as the first component of another OBJECT IDENTIFIER".to_string()),
                                    loc: valref.loc,
                                });
                            }
                            let resolved_oid = relative_oid.resolve_oid()?;
                            nodes.extend(resolved_oid.0);
                        }
                        other => {
                            return Err(Error {
                                kind: ErrorKind::Ast(format!(
                                    "expecting OBJECT IDENTIFIER or INTEGER; found {}",
                                    other.tag_type()
                                )),
                                loc: valref.loc,
                            });
                        }
                    }
                }
                ObjectIdentifierComponent::IntegerLiteral(lit) => {
                    nodes.push(lit.element);
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
    ValueReference(AstElement<valref!()>),
    IntegerLiteral(AstElement<u64>),
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
