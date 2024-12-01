use crate::values::valref;

#[derive(Debug, Clone)]
pub struct IntegerType {
    pub named_values: Option<Vec<NamedNumber>>,
    pub value_constraints: Option<Constraints>,
}

#[derive(Debug, Clone)]
pub struct NamedNumber {
    pub name: String,
    pub value: valref!(Integer),
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

pub type Constraints = Vec<Constraint>;

#[derive(Debug, Clone)]
pub enum Constraint {
    Constant(valref!(Integer)),
    Range(Range),
}

#[derive(Debug, Clone)]
pub struct Range {
    pub lower: RangeLowerBound,
    pub upper: RangeUpperBound,
}

#[derive(Debug, Clone)]
pub enum RangeLowerBound {
    Constant(valref!(Integer)),
    Min,
}

#[derive(Debug, Clone)]
pub enum RangeUpperBound {
    Constant(valref!(Integer)),
    Max,
}

pub type EnumeratedType = Vec<EnumerationItem>;

#[derive(Debug, Clone)]
pub struct EnumerationItem {
    pub name: String,
    pub value: Option<valref!(Integer)>,
}
