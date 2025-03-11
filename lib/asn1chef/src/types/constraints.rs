use num::BigInt;

use crate::{
    compiler::{
        parser::{AstElement, Error, ErrorKind, Result},
        Context,
    },
    values::{BuiltinValue, Value, ValueResolve},
};

/// The following example constraint spec...
///
/// ```asn1
/// I ::= INTEGER (0..10 | 15 | 20, ..., 20<..30, ...)
/// ```
///
/// ...would be represented by the following pseudo-`Constraint`:
///
/// ```text
/// vec![
///   vec![
///     ValueRange(Range(Eq(Value(0)), Eq(Value(10)))),
///     SingleValue(15),
///     SingleValue(20),
///   ],
///   vec![
///     ValueRange(Range(Gt(Value(20)), Eq(Value(30)))),
///   ],
/// ]
/// ```
///
/// The outer Vec represents each individual subtype constraint element set.
/// Each inner Vec represents a union between each contained subtype constraint.
///
/// For the purposes of the level of X.680 compliance currently implemented,
/// the elements of each inner Vec can be considered to all be in union with each other,
/// (i.e. a flattened Vec), since only union-like constraints are currently supported.
#[derive(Debug, Clone)]
pub struct Constraint(pub Vec<Vec<AstElement<SubtypeElement>>>);

#[derive(Debug, Clone)]
pub enum SubtypeElement {
    SingleValue(AstElement<Value>),
    ValueRange(ValueRange),
    Size(Constraint),
    InnerType(InnerTypeConstraints),
}

#[derive(Debug, Clone)]
pub enum InnerTypeConstraintsKind {
    Full,
    Partial,
}

#[derive(Debug, Clone)]
pub struct InnerTypeConstraints {
    pub kind: InnerTypeConstraintsKind,
    pub components: Vec<NamedConstraint>,
}

#[derive(Debug, Clone)]
pub struct NamedConstraint {
    pub name: AstElement<String>,
    pub constraint: ComponentConstraint,
}

#[derive(Debug, Clone)]
pub struct ComponentConstraint {
    pub value: Option<Constraint>,
    pub presence: Option<Presence>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Presence {
    Present,
    Absent,
    Optional,
}

#[derive(Debug, Clone)]
pub struct ValueRange {
    pub lower: RangeLowerBound,
    pub upper: RangeUpperBound,
}

#[derive(Debug, Clone)]
pub enum RangeLowerBound {
    Eq(AstElement<Value>),
    Gt(AstElement<Value>),
    Min,
}

#[derive(Debug, Clone)]
pub enum RangeUpperBound {
    Eq(AstElement<Value>),
    Lt(AstElement<Value>),
    Max,
}

#[derive(Debug, Clone, Copy)]
pub enum ConstraintCheckMode {
    Value,
    Size,
}

impl Constraint {
    pub fn flatten(&self) -> Vec<&AstElement<SubtypeElement>> {
        self.0.iter().flatten().collect()
    }

    pub fn includes_value(
        &self,
        context: &Context,
        value: &AstElement<Value>,
    ) -> Result<Option<bool>> {
        let mut has_matching_constraint = false;

        for element in self.flatten() {
            if let SubtypeElement::SingleValue(single_value) = &element.element {
                has_matching_constraint = true;
                if value.try_eq(context, single_value)? {
                    return Ok(Some(true));
                }
            }
        }

        Ok(if has_matching_constraint {
            Some(false)
        } else {
            None
        })
    }

    pub fn includes_integer(
        &self,
        context: &Context,
        mode: ConstraintCheckMode,
        value: &BigInt,
    ) -> Result<Option<bool>> {
        macro_rules! cmp_constant {
            ( $value:expr, $op:tt, $constant:expr ) => {{
                let constant = $constant.resolve(context)?;
                match constant {
                    BuiltinValue::Integer(integer) => {
                        $value $op integer
                    }
                    other => return Err(Error {
                        kind: ErrorKind::Ast(format!("expecting INTEGER in constraint, found {}", other.tag_type(context)?)),
                        loc: $constant.loc,
                    }),
                }
            }};
        }

        let mut has_matching_constraint = false;
        for element in self.flatten() {
            match mode {
                ConstraintCheckMode::Value => match &element.element {
                    SubtypeElement::SingleValue(single_value) => {
                        has_matching_constraint = true;
                        if cmp_constant!(value, ==, single_value) {
                            return Ok(Some(true));
                        }
                    }
                    SubtypeElement::ValueRange(range) => {
                        has_matching_constraint = true;
                        let meets_lower = match &range.lower {
                            // TODO: MIN is not always TRUE; e.g.
                            //
                            // I1 ::= INTEGER (1..10)
                            // I2 ::= I1 (MIN..5)
                            // the valid values for I2 are { 1, 2, 3, 4, 5 }; 0 would not be a valid type
                            // this needs to change once subconstraint intersections are supported
                            // same for MAX below
                            RangeLowerBound::Min => true,
                            RangeLowerBound::Eq(constant) => cmp_constant!(value, >=, constant),
                            RangeLowerBound::Gt(constant) => cmp_constant!(value, >, constant),
                        };
                        if meets_lower {
                            let meets_upper = match &range.upper {
                                RangeUpperBound::Max => true,
                                RangeUpperBound::Eq(constant) => {
                                    cmp_constant!(value, <=, constant)
                                }
                                RangeUpperBound::Lt(constant) => {
                                    cmp_constant!(value, <, constant)
                                }
                            };
                            if meets_upper {
                                return Ok(Some(true));
                            }
                        }
                    }
                    _ => (),
                },
                ConstraintCheckMode::Size => if let SubtypeElement::Size(size_constraint) = &element.element {
                    has_matching_constraint = true;
                    if let Some(result) = size_constraint.includes_integer(
                        context,
                        ConstraintCheckMode::Value,
                        value,
                    )? {
                        return Ok(Some(result));
                    }
                },
            }
        }

        // If there are no constraints found for the given ConstraintCheckMode,
        // then all values for that ConstraintCheckMode are valid, and None should be returned
        // Otherwise, if there are constraints found for the given ConstraintCheckMode,
        // but none of then matched, Some(false) should be returned
        Ok(if has_matching_constraint {
            Some(false)
        } else {
            None
        })
    }
}
