use std::{
    fmt::{Display, Write},
    num::ParseIntError,
};

use num::{bigint::Sign, BigInt};

use crate::{
    compiler::{
        parser::{AstElement, Error, ErrorKind, Result},
        Context,
    },
    module::QualifiedIdentifier,
    types::{ResolvedType, TagType},
    values,
};

use super::{BuiltinValue, TypedValue};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BitStringValue {
    pub data: Vec<u8>,
    pub unused_bits: u8,
}

impl Display for BitStringValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut bit_str = String::with_capacity(self.data.len() * 8);
        if !self.data.is_empty() {
            for byte in self.data.iter().take(self.data.len() - 1) {
                // write each byte with zero padding to the left of the number
                write!(&mut bit_str, "{:0<8b}", byte).unwrap();
            }

            // write the last byte with zero padding to the right of the number
            let last = self.data.last().cloned().unwrap();
            write!(&mut bit_str, "{:0>8b}", last).unwrap();
        }
        f.write_str(&bit_str)?;

        Ok(())
    }
}

pub trait ComponentValueLike {
    fn name(&self) -> &AstElement<String>;
    fn value(&self) -> &AstElement<TypedValue>;
}

#[derive(Debug, Clone)]
pub struct StructureValueComponent {
    pub name: AstElement<String>,
    pub value: AstElement<TypedValue>,
    pub is_default: bool,
}

impl ComponentValueLike for StructureValueComponent {
    fn name(&self) -> &AstElement<String> {
        &self.name
    }

    fn value(&self) -> &AstElement<TypedValue> {
        &self.value
    }
}

#[derive(Debug, Clone)]
pub struct StructureValue {
    pub components: Vec<StructureValueComponent>,
}

#[derive(Debug, Clone)]
pub struct ChoiceValue {
    pub alternative: AstElement<String>,
    pub alternative_type: ResolvedType,
    pub value: Box<AstElement<TypedValue>>,
}

impl ComponentValueLike for ChoiceValue {
    fn name(&self) -> &AstElement<String> {
        &self.alternative
    }

    fn value(&self) -> &AstElement<TypedValue> {
        &self.value
    }
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
                    let resolved_value = values::resolve_untyped(context, valref.as_ref())?;
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
                                        "expecting {} or INTEGER, but found {}",
                                        self.ty,
                                        other.tag_type(context)?
                                    )
                                } else {
                                    format!(
                                        "expecting INTEGER, but found {}",
                                        other.tag_type(context)?
                                    )
                                }),
                                loc: valref.loc,
                            });
                        }
                    }
                }
                ObjectIdentifierComponent::IntegerLiteral { int, .. } => {
                    nodes.push(int.element);
                }
            }
        }

        Ok(Oid(nodes))
    }
}

impl Display for ObjectIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("{ ")?;
        for component in &self.components {
            component.fmt(f)?;
            f.write_str(" ")?;
        }
        f.write_str("}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ObjectIdentifierComponent {
    /// Must be either an `OBJECT IDENTIFIER` or `INTEGER` value.
    /// This will be verified when `ObjectIdentifier::resolve_oid` is invoked.
    ValueReference(AstElement<QualifiedIdentifier>),
    IntegerLiteral {
        name: Option<AstElement<String>>,
        int: AstElement<u64>,
    },
}

impl Display for ObjectIdentifierComponent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ValueReference(valref) => valref.element.fmt(f)?,
            Self::IntegerLiteral { name, int } => {
                if let Some(name) = name {
                    f.write_fmt(format_args!("{}({})", name.element, int.element))?
                } else {
                    f.write_str(&int.element.to_string())?
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Oid(pub Vec<u64>);

impl Oid {
    pub fn parse_string(str: &str) -> std::result::Result<Oid, ParseIntError> {
        Ok(Oid(str
            .split('.')
            .map(|node| node.parse::<u64>())
            .collect::<std::result::Result<Vec<u64>, ParseIntError>>(
        )?))
    }
}

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

impl Display for RealLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let exponent: i64 = self.exponent.clone().try_into().unwrap();
        let str = if self.exponent < num::BigInt::ZERO {
            let one = num::BigInt::from(1);
            let denom = one << (-exponent);
            num::BigRational::new(self.mantissa.clone(), denom).to_string()
        } else {
            let mut mantissa = self.mantissa.clone();
            mantissa <<= exponent;
            mantissa.to_string()
        };
        f.write_str(&str)
    }
}
