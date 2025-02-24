use std::fmt::Display;

use crate::{
    compiler::parser::{AstElement, Result},
    values::{valref, Value, ValueResolve},
};

#[derive(Debug, Clone)]
pub enum Constraint {
    ConstantSeries(Vec<AstElement<valref!(Integer)>>),
    Range(Range),
}

#[derive(Debug, Clone)]
pub struct Range {
    pub lower: RangeLowerBound,
    pub upper: RangeUpperBound,
}

#[derive(Debug, Clone)]
pub enum RangeLowerBound {
    Constant(AstElement<valref!(Integer)>),
    GtConstant(AstElement<valref!(Integer)>),
    Min,
}

#[derive(Debug, Clone)]
pub enum RangeUpperBound {
    Constant(AstElement<valref!(Integer)>),
    LtConstant(AstElement<valref!(Integer)>),
    Max,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ConstraintsKind {
    Value,
    Size,
}

impl Display for ConstraintsKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Value => "value",
            Self::Size => "size",
        })
    }
}

#[derive(Debug, Clone)]
pub struct Constraints {
    pub kind: ConstraintsKind,
    pub components: Vec<Constraint>,
}

impl Constraints {
    pub fn includes_value(&self, value: i64) -> Result<bool> {
        macro_rules! cmp_constant {
            ( $value:expr, $op:tt, $constant:expr ) => {{
                let constant = $constant.resolve()?;
                match constant {
                    Value::Integer(integer) => {
                        $value $op *integer
                    }
                    // the valref!(Integer) type will ensure the value
                    // is a Value::Integer when resolve() is called
                    _ => unreachable!(),
                }
            }};
        }

        for constraint in &self.components {
            match constraint {
                Constraint::ConstantSeries(series) => {
                    for constant in series {
                        if cmp_constant!(value, ==, constant) {
                            return Ok(true);
                        }
                    }
                }
                Constraint::Range(range) => {
                    let meets_lower = match &range.lower {
                        RangeLowerBound::Min => true,
                        RangeLowerBound::Constant(constant) => cmp_constant!(value, >=, constant),
                        RangeLowerBound::GtConstant(constant) => cmp_constant!(value, >, constant),
                    };
                    if meets_lower {
                        let meets_upper = match &range.upper {
                            RangeUpperBound::Max => true,
                            RangeUpperBound::Constant(constant) => {
                                cmp_constant!(value, <=, constant)
                            }
                            RangeUpperBound::LtConstant(constant) => {
                                cmp_constant!(value, <, constant)
                            }
                        };
                        if meets_upper {
                            return Ok(true);
                        }
                    }
                }
            }
        }

        Ok(false)
    }
}
