use std::fmt::Display;

use num::{bigint::Sign, BigInt};

use crate::{
    compiler::{
        parser::{AstElement, Error, ErrorKind, Result},
        Context,
    },
    types::{ResolvedType, TagType},
};

use super::{BuiltinValue, Value, ValueResolve};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BitStringValue {
    pub data: Vec<u8>,
    pub unused_bits: u8,
}

#[derive(Debug, Clone)]
pub struct StructureValueComponent {
    pub name: AstElement<String>,
    pub value: AstElement<Value>,
    pub is_default: bool,
}

#[derive(Debug, Clone)]
pub struct StructureValue {
    pub components: Vec<StructureValueComponent>,
}

#[derive(Debug, Clone)]
pub struct ChoiceValue {
    pub alternative: AstElement<String>,
    pub alternative_type: ResolvedType,
    pub value: Box<AstElement<Value>>,
}

#[derive(Debug, Clone)]
pub struct ObjectIdentifier {
    pub ty: TagType,
    pub components: Vec<ObjectIdentifierComponent>,
}

impl ObjectIdentifier {
    pub fn resolve_oid(&self, context: &Context) -> Result<Oid> {
        let mut nodes = Vec::new();

        for (i, component) in self.components.iter().enumerate() {
            match component {
                ObjectIdentifierComponent::ValueReference(valref) => {
                    let resolved_value = valref.resolve(context)?;
                    match resolved_value {
                        BuiltinValue::Integer(integer) => {
                            let (sign, digits) = integer.to_u64_digits();
                            if sign == Sign::Minus {
                                return Err(Error {
                                    kind: ErrorKind::Ast(format!(
                                        "{} node must be a non-negative integer",
                                        self.ty
                                    )),
                                    loc: valref.loc,
                                });
                            }
                            if digits.len() > 1 {
                                return Err(Error {
                                    kind: ErrorKind::Ast(format!(
                                        "{} node is too large to fit in a 64-bit unsigned integer",
                                        self.ty
                                    )),
                                    loc: valref.loc,
                                });
                            }
                            nodes.push(digits[0]);
                        }
                        BuiltinValue::ObjectIdentifier(parent_oid) => {
                            if self.ty != TagType::ObjectIdentifier || i != 0 {
                                return Err(Error{
                                    kind: ErrorKind::Ast("OBJECT IDENTIFIER can only be used as the first component of another OBJECT IDENTIFIER".to_string()),
                                    loc: valref.loc,
                                });
                            }
                            let resolved_oid = parent_oid.resolve_oid(context)?;
                            nodes.extend(resolved_oid.0);
                        }
                        BuiltinValue::RelativeOid(parent_oid) => {
                            if self.ty != TagType::RelativeOid || i != 0 {
                                return Err(Error{
                                    kind: ErrorKind::Ast("RELATIVE-OID can only be used as the first component of another RELATIVE-OID".to_string()),
                                    loc: valref.loc,
                                });
                            }
                            let resolved_oid = parent_oid.resolve_oid(context)?;
                            nodes.extend(resolved_oid.0);
                        }
                        other => {
                            return Err(Error {
                                kind: ErrorKind::Ast(if i == 0 {
                                    format!(
                                        "expecting {} or INTEGER, found {}",
                                        self.ty,
                                        other.tag_type(context)?
                                    )
                                } else {
                                    format!("expecting INTEGER, found {}", other.tag_type(context)?)
                                }),
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
    ValueReference(AstElement<Value>),
    IntegerLiteral(AstElement<u64>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RealLiteral {
    pub mantissa: BigInt,
    pub exponent: BigInt,
}
