use std::{
    cmp::Ordering,
    collections::HashSet,
    fmt::{self, Display},
};

use itertools::Itertools;
use num::{bigint::Sign, BigInt};

use crate::{
    compiler::{
        parser::{AstElement, Error, ErrorKind, Loc, Result},
        Context,
    },
    module::QualifiedIdentifier,
    values::{BuiltinValue, ResolvedValue, TryEq, TypedValue, ValueResolve},
};

use super::{BuiltinType, TagType, TaggedType};

macro_rules! resolve_integer {
    ( $context:expr, $constant:expr ) => {{
        let context = $context;
        let constant = $constant.resolve(context)?;
        match constant.value {
            BuiltinValue::Integer(integer) => integer,
            other => {
                return Err(Error {
                    kind: ErrorKind::Ast(format!(
                        "expecting INTEGER in constraint, but found {}",
                        other.tag_type(context)?
                    )),
                    loc: $constant.loc,
                })
            }
        }
    }};
}

#[derive(Debug, Clone)]
pub struct ResolvedConstraint {
    pub specs: Vec<ConstraintSpec>,
    /// `true` if the constraint has an extensiblity marker.
    pub is_extensible: bool,
    pub loc: Loc,
}

impl ResolvedConstraint {
    /// Returns the `(lower, upper)` bounds of the constraint.
    /// If there are no single value or range constraints, None is returned.
    pub fn integer_value_bounds(&self) -> Result<Option<ConstraintBounds>> {
        self.extensible_integer_value_bound(false)
    }

    fn extensible_integer_value_bound(
        &self,
        mut is_extension: bool,
    ) -> Result<Option<ConstraintBounds>> {
        let mut bounds: Option<ConstraintBounds> = None;

        for spec in &self.specs {
            if !is_extension && spec.is_extension {
                is_extension = true;
            }
            for item in &spec.items {
                if let ConstraintSpecItem::Value(value_spec) = item {
                    match &value_spec.element {
                        ValueConstraint::SingleValue(single_value) => {
                            let single_value = match &single_value.value {
                                BuiltinValue::Integer(int) => int,
                                _ => {
                                    return Err(Error {
                                        kind: ErrorKind::Ast(format!(
                                            "expecting INTEGER, but found {}",
                                            single_value.ty.ty,
                                        )),
                                        loc: value_spec.loc,
                                    })
                                }
                            };
                            match &mut bounds {
                                Some(constraint_bounds) => {
                                    match &mut constraint_bounds.lower_bound {
                                        Bound::Integer(lower_bound)
                                            if single_value < lower_bound =>
                                        {
                                            *lower_bound = single_value.clone();
                                        }
                                        _ => (),
                                    };
                                    if !is_extension {
                                        match &mut constraint_bounds.upper_bound {
                                            Some(Bound::Integer(upper_bound))
                                                if single_value > upper_bound =>
                                            {
                                                *upper_bound = single_value.clone();
                                            }
                                            _ => (),
                                        };
                                    }
                                    match &mut constraint_bounds.extended_upper_bound {
                                        Bound::Integer(extended_upper_bound)
                                            if single_value > extended_upper_bound =>
                                        {
                                            *extended_upper_bound = single_value.clone();
                                        }
                                        _ => (),
                                    };
                                }
                                None => {
                                    bounds = Some(ConstraintBounds {
                                        lower_bound: Bound::Integer(single_value.clone()),
                                        upper_bound: if is_extension {
                                            None
                                        } else {
                                            Some(Bound::Integer(single_value.clone()))
                                        },
                                        extended_upper_bound: Bound::Integer(single_value.clone()),
                                    })
                                }
                            }
                        }
                        ValueConstraint::Range(range) => {
                            let (lower_value, upper_value) = (&range.lower, &range.upper);
                            match &mut bounds {
                                Some(constraint_bounds) => {
                                    if lower_value == &Bound::Unbounded {
                                        constraint_bounds.lower_bound = Bound::Unbounded;
                                    } else if let (
                                        Bound::Integer(lower_value),
                                        Bound::Integer(lower_bound),
                                    ) = (lower_value, &mut constraint_bounds.lower_bound)
                                    {
                                        if lower_value < lower_bound {
                                            *lower_bound = lower_value.clone();
                                        }
                                    }

                                    if !is_extension {
                                        if upper_value == &Bound::Unbounded {
                                            constraint_bounds.upper_bound = Some(Bound::Unbounded);
                                        } else if let (
                                            Bound::Integer(upper_value),
                                            Some(Bound::Integer(upper_bound)),
                                        ) = (
                                            upper_value.clone(),
                                            &mut constraint_bounds.upper_bound,
                                        ) {
                                            if &upper_value > upper_bound {
                                                *upper_bound = upper_value;
                                            }
                                        }
                                    }
                                    if upper_value == &Bound::Unbounded {
                                        constraint_bounds.extended_upper_bound = Bound::Unbounded;
                                    } else if let (
                                        Bound::Integer(upper_value),
                                        Bound::Integer(extended_upper_bound),
                                    ) =
                                        (upper_value, &mut constraint_bounds.extended_upper_bound)
                                    {
                                        if upper_value > extended_upper_bound {
                                            *extended_upper_bound = upper_value.clone();
                                        }
                                    }
                                }
                                None => {
                                    bounds = Some(ConstraintBounds {
                                        lower_bound: lower_value.clone(),
                                        upper_bound: if is_extension {
                                            None
                                        } else {
                                            Some(upper_value.clone())
                                        },
                                        extended_upper_bound: upper_value.clone(),
                                    })
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(bounds)
    }

    pub fn size_bounds(&self) -> Result<Option<ConstraintBounds>> {
        let mut bounds: Option<ConstraintBounds> = None;
        let mut is_extension = false;
        for spec in &self.specs {
            if !is_extension && spec.is_extension {
                is_extension = true;
            }
            for item in &spec.items {
                if let ConstraintSpecItem::Size(constraint) = item {
                    let size_bounds = constraint.extensible_integer_value_bound(is_extension)?;
                    if let Some(size_bounds) = size_bounds {
                        match &mut bounds {
                            Some(bounds) => {
                                bounds.apply_bounds(&size_bounds);
                            }
                            None => bounds = Some(size_bounds),
                        }
                    }
                }
            }
        }
        if let Some(bounds) = &bounds {
            if let Bound::Integer(lower_bound) = &bounds.lower_bound {
                if lower_bound.sign() == Sign::Minus {
                    return Err(Error {
                        kind: ErrorKind::Ast("illegal negative SIZE constraint".to_string()),
                        loc: self.loc,
                    });
                }
            }
        }
        Ok(bounds)
    }

    pub fn includes_value(
        &self,
        context: &Context,
        value: &AstElement<TypedValue>,
    ) -> Result<Option<bool>> {
        let mut has_matching_constraint = false;

        let value = AstElement::new(value.resolve(context)?, value.loc);
        for spec in &self.specs {
            for item in &spec.items {
                if let ConstraintSpecItem::Value(value_spec) = item {
                    has_matching_constraint = true;
                    if let ValueConstraint::SingleValue(single_value) = &value_spec.element {
                        if value
                            .try_eq(context, &value_spec.as_ref().map(|_| single_value.clone()))?
                        {
                            return Ok(Some(true));
                        }
                    }
                }
            }
        }

        Ok(if has_matching_constraint {
            Some(false)
        } else {
            None
        })
    }

    /// Returns true if the constraint is extensible.
    /// If `mode` is `None`, extensibility of any constraints are included.
    /// If `mode` is [`ConstraintCheckMode::Value`], extensibility of value constraints only are included.
    /// If `mode` is [`ConstraintCheckMode::Size`], extensibility of `SIZE` constraints only are included.
    pub fn is_extensible(&self, mode: Option<ConstraintCheckMode>) -> bool {
        if self.is_extensible && matches!(mode, None | Some(ConstraintCheckMode::Value)) {
            return true;
        }
        if matches!(mode, None | Some(ConstraintCheckMode::Size)) {
            for spec in &self.specs {
                for item in &spec.items {
                    if let ConstraintSpecItem::Size(size) = item {
                        if size.is_extensible(None) {
                            return true;
                        }
                    }
                }
            }
        }

        false
    }

    pub fn includes_integer(
        &self,
        mode: ConstraintCheckMode,
        value: &BigInt,
    ) -> Result<Option<IntegerInclusion>> {
        let mut has_matching_constraint = false;
        for spec in &self.specs {
            for item in &spec.items {
                match mode {
                    ConstraintCheckMode::Value => {
                        if let ConstraintSpecItem::Value(value_spec) = item {
                            has_matching_constraint = true;
                            match &value_spec.element {
                                ValueConstraint::SingleValue(single_value) => {
                                    let single_value = match &single_value.value {
                                        BuiltinValue::Integer(integer) => integer,
                                        _ => {
                                            return Err(Error {
                                                kind: ErrorKind::Ast(format!(
                                                    "expecting INTEGER in constraint, but found {}",
                                                    single_value.ty.ty
                                                )),
                                                loc: value_spec.loc,
                                            })
                                        }
                                    };
                                    if value == single_value {
                                        return Ok(Some(IntegerInclusion::Included {
                                            is_extension: spec.is_extension,
                                        }));
                                    }
                                }
                                ValueConstraint::Range(range) => {
                                    let meets_lower = match &range.lower {
                                        // TODO: MIN is not always TRUE; e.g.
                                        //
                                        // I1 ::= INTEGER (1..10)
                                        // I2 ::= I1 (MIN..5)
                                        // the valid values for I2 are { 1, 2, 3, 4, 5 }; 0 would not be a valid type
                                        // this needs to change once subconstraint intersections are supported
                                        // same for MAX below
                                        Bound::Unbounded => true,
                                        Bound::Integer(constant) => value >= constant,
                                    };
                                    if meets_lower {
                                        let meets_upper = match &range.upper {
                                            Bound::Unbounded => true,
                                            Bound::Integer(constant) => value <= constant,
                                        };
                                        if meets_upper {
                                            return Ok(Some(IntegerInclusion::Included {
                                                is_extension: spec.is_extension,
                                            }));
                                        }
                                    }
                                }
                            }
                        }
                    }
                    ConstraintCheckMode::Size => {
                        if let ConstraintSpecItem::Size(size_constraint) = item {
                            has_matching_constraint = true;
                            if let Some(IntegerInclusion::Included {
                                is_extension: is_size_constraint_extension,
                            }) = size_constraint
                                .includes_integer(ConstraintCheckMode::Value, value)?
                            {
                                return Ok(Some(IntegerInclusion::Included {
                                    is_extension: spec.is_extension || is_size_constraint_extension,
                                }));
                            }
                        }
                    }
                }
            }
        }

        // If there are no constraints found for the given ConstraintCheckMode,
        // then all values for that ConstraintCheckMode are valid, and None should be returned
        // Otherwise, if there are constraints found for the given ConstraintCheckMode,
        // but none of then matched, Some(false) should be returned
        Ok(if has_matching_constraint {
            Some(IntegerInclusion::NotIncluded)
        } else {
            None
        })
    }

    pub fn has_value_constraint(&self) -> bool {
        self.specs.iter().any(|spec| {
            spec.items
                .iter()
                .any(|item| matches!(item, ConstraintSpecItem::Value(_)))
        })
    }
}

#[derive(Debug, Clone)]
pub struct ConstraintSpec {
    pub items: Vec<ConstraintSpecItem>,
    pub is_extension: bool,
}

#[derive(Debug, Clone)]
pub enum ConstraintSpecItem {
    Value(AstElement<ValueConstraint>),
    Size(ResolvedConstraint),
    InnerType(InnerTypeConstraints),
    Contents(ContentsConstraint),
    Table(TableConstraint),
    UserDefined,
}

#[derive(Debug, Hash, PartialEq, Eq)]
enum ConstraintSpecItemKind {
    Value,
    Size,
    InnerType,
    Other,
}

fn collect_spec_item_kinds(items: &[ConstraintSpecItem]) -> HashSet<ConstraintSpecItemKind> {
    let mut kinds = HashSet::with_capacity(items.len());
    for item in items {
        kinds.insert(match item {
            ConstraintSpecItem::Value(_) => ConstraintSpecItemKind::Value,
            ConstraintSpecItem::Size(_) => ConstraintSpecItemKind::Size,
            ConstraintSpecItem::InnerType(_) => ConstraintSpecItemKind::InnerType,
            _ => ConstraintSpecItemKind::Other,
        });
    }
    kinds
}

fn spec_item_kinds_contains_only(
    kinds: &HashSet<ConstraintSpecItemKind>,
    contains: &[ConstraintSpecItemKind],
) -> bool {
    kinds.iter().all(|kind| contains.contains(kind))
}

impl ConstraintSpecItem {
    pub fn get_name(&self) -> &'static str {
        match self {
            Self::Value(_) => "value",
            Self::Size(_) => "size",
            Self::InnerType(_) => "inner type",
            Self::Contents(_) => "contents",
            Self::Table(_) => "reference table",
            Self::UserDefined => "user-defined",
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueConstraint {
    SingleValue(ResolvedValue),
    Range(ResolvedValueRange),
}

#[derive(Debug, Clone)]
pub struct ResolvedValueRange {
    pub lower: Bound,
    pub upper: Bound,
}

#[derive(Debug, Clone)]
pub enum ConstraintTree {
    BinaryExpr {
        lhs: Box<ConstraintTree>,
        op: AstElement<ConstraintTreeOperator>,
        rhs: Box<ConstraintTree>,
    },
    Element(AstElement<SubtypeElement>),
}

impl ConstraintTree {
    pub fn resolve(
        &self,
        context: &Context,
        constrained_type: &BuiltinType,
    ) -> Result<Vec<ConstraintSpecItem>> {
        Ok(match self {
            Self::BinaryExpr {
                lhs: lhs_tree,
                op,
                rhs: rhs_tree,
            } => {
                let lhs = lhs_tree.resolve(context, constrained_type)?;
                let lhs_kinds = collect_spec_item_kinds(&lhs);

                let rhs = rhs_tree.resolve(context, constrained_type)?;
                let rhs_kinds = collect_spec_item_kinds(&rhs);

                if spec_item_kinds_contains_only(
                    &lhs_kinds,
                    &[ConstraintSpecItemKind::Value, ConstraintSpecItemKind::Size],
                ) && spec_item_kinds_contains_only(
                    &rhs_kinds,
                    &[ConstraintSpecItemKind::Value, ConstraintSpecItemKind::Size],
                ) {
                    eval_value_and_size_set_binary_expr(&lhs, op, &rhs, constrained_type)?
                } else if spec_item_kinds_contains_only(
                    &lhs_kinds,
                    &[ConstraintSpecItemKind::InnerType],
                ) && spec_item_kinds_contains_only(
                    &rhs_kinds,
                    &[ConstraintSpecItemKind::InnerType],
                ) {
                    if op.element != ConstraintTreeOperator::Union {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "cannot use {} operator with inner type constraints",
                                op.element
                            )),
                            loc: op.loc,
                        });
                    }

                    let mut all = Vec::with_capacity(lhs.len() + rhs.len());
                    all.extend(lhs);
                    all.extend(rhs);
                    all
                } else {
                    let lhs_names = lhs
                        .into_iter()
                        .map(|item| item.get_name())
                        .unique()
                        .collect::<Vec<_>>()
                        .join(", ");
                    let rhs_names = rhs
                        .into_iter()
                        .map(|item| item.get_name())
                        .unique()
                        .collect::<Vec<_>>()
                        .join(", ");
                    return Err(Error {
                        kind: ErrorKind::Ast(if lhs_kinds == rhs_kinds {
                            format!(
                                "cannot use {} operator with {} constraints",
                                op.element, lhs_names,
                            )
                        } else {
                            format!(
                                "cannot use {} operator with {} and {} constraints",
                                op.element, lhs_names, rhs_names,
                            )
                        }),
                        loc: op.loc,
                    });
                }
            }
            Self::Element(element) => match &element.element {
                SubtypeElement::SingleValue(single_value) => {
                    let value = single_value.resolve(context)?;
                    vec![ConstraintSpecItem::Value(AstElement::new(
                        ValueConstraint::SingleValue(value),
                        single_value.loc,
                    ))]
                }
                SubtypeElement::ValueRange(range) => {
                    let range = range.resolve(context)?;
                    vec![ConstraintSpecItem::Value(AstElement::new(
                        ValueConstraint::Range(range),
                        element.loc,
                    ))]
                }
                SubtypeElement::ContainedSubtype(subtype) => {
                    let subtype = subtype.resolve(context)?;
                    match &subtype.constraint {
                        Some(constraint) => {
                            let resolved = constraint.resolve(context, constrained_type)?;
                            let specs = &resolved.specs;

                            // only inherit the base constraint of a contained subtype;
                            // i.e. do not inherit the extensions
                            // see X.680 clause I.4.3.5 for details
                            specs[0].items.clone()
                        }
                        None => vec![ConstraintSpecItem::Value(AstElement::new(
                            ValueConstraint::Range(ResolvedValueRange {
                                lower: Bound::Unbounded,
                                upper: Bound::Unbounded,
                            }),
                            element.loc,
                        ))],
                    }
                }
                SubtypeElement::Size(size) => {
                    let size_constraint =
                        size.resolve(context, &BuiltinType::universal(TagType::Integer))?;
                    vec![ConstraintSpecItem::Size(size_constraint)]
                }
                SubtypeElement::InnerType(inner_type) => {
                    vec![ConstraintSpecItem::InnerType(inner_type.clone())]
                }
                SubtypeElement::Contents(contents) => {
                    vec![ConstraintSpecItem::Contents(contents.clone())]
                }
                SubtypeElement::Table(table) => vec![ConstraintSpecItem::Table(table.clone())],
                SubtypeElement::UserDefined => vec![ConstraintSpecItem::UserDefined],
            },
        })
    }
}

fn get_value_constraint_bounds(item: &ValueConstraint) -> ResolvedValueRange {
    match item {
        ValueConstraint::Range(range) => range.clone(),
        ValueConstraint::SingleValue(value) => match &value.value {
            BuiltinValue::Integer(integer) => ResolvedValueRange {
                lower: Bound::Integer(integer.clone()),
                upper: Bound::Integer(integer.clone()),
            },
            _ => unreachable!(),
        },
    }
}

fn iter_exactly_zero_or_one<Item, I: Iterator<Item = Item>>(mut iter: I) -> Option<Item> {
    match iter.next() {
        Some(item) => match iter.next() {
            Some(_) => {
                panic!("expecting zero or one items in iterator, but found more than one");
            }
            None => Some(item),
        },
        None => None,
    }
}

fn eval_value_and_size_set_binary_expr(
    lhs: &[ConstraintSpecItem],
    op: &AstElement<ConstraintTreeOperator>,
    rhs: &[ConstraintSpecItem],
    constrained_type: &BuiltinType,
) -> Result<Vec<ConstraintSpecItem>> {
    // step 1: op all the values
    let values = eval_value_set_binary_expr(
        lhs.iter().filter_map(|item| match item {
            ConstraintSpecItem::Value(value) => Some(value.clone()),
            _ => None,
        }),
        op.element,
        rhs.iter().filter_map(|item| match item {
            ConstraintSpecItem::Value(value) => Some(value.clone()),
            _ => None,
        }),
        constrained_type,
    )?
    .into_iter()
    .map(ConstraintSpecItem::Value)
    .collect::<Vec<_>>();

    // step 2: op all the sizes
    let lhs_size = iter_exactly_zero_or_one(lhs.iter().filter_map(|item| match item {
        ConstraintSpecItem::Size(size) => Some(size),
        _ => None,
    }));
    let rhs_size = iter_exactly_zero_or_one(rhs.iter().filter_map(|item| match item {
        ConstraintSpecItem::Size(size) => Some(size),
        _ => None,
    }));

    let size_spec = match (lhs_size, rhs_size) {
        (Some(lhs_size), Some(rhs_size)) => {
            let mut size_specs = Vec::with_capacity(2);

            let lhs_base = lhs_size.specs[0].items.iter().map(|item| match item {
                ConstraintSpecItem::Value(value) => value.clone(),
                _ => unreachable!(),
            });
            let rhs_base = rhs_size.specs[0].items.iter().map(|item| match item {
                ConstraintSpecItem::Value(value) => value.clone(),
                _ => unreachable!(),
            });

            size_specs.push(ConstraintSpec {
                items: eval_value_set_binary_expr(
                    lhs_base,
                    op.element,
                    rhs_base,
                    &BuiltinType::universal(TagType::Integer),
                )?
                .into_iter()
                .map(ConstraintSpecItem::Value)
                .collect(),
                is_extension: false,
            });

            if lhs_size.specs.len() > 1 || rhs_size.specs.len() > 1 {
                let lhs_ext = lhs_size.specs.iter().skip(1).flat_map(|spec| {
                    spec.items.iter().map(|item| match item {
                        ConstraintSpecItem::Value(value) => value.clone(),
                        _ => unreachable!(),
                    })
                });
                let rhs_ext = rhs_size.specs.iter().skip(1).flat_map(|spec| {
                    spec.items.iter().map(|item| match item {
                        ConstraintSpecItem::Value(value) => value.clone(),
                        _ => unreachable!(),
                    })
                });
                size_specs.push(ConstraintSpec {
                    items: eval_value_set_binary_expr(
                        lhs_ext,
                        op.element,
                        rhs_ext,
                        &BuiltinType::universal(TagType::Integer),
                    )?
                    .into_iter()
                    .map(ConstraintSpecItem::Value)
                    .collect(),
                    is_extension: true,
                });
            }

            Some(ConstraintSpecItem::Size(ResolvedConstraint {
                is_extensible: size_specs.len() > 1
                    || lhs_size.is_extensible
                    || rhs_size.is_extensible,
                specs: size_specs,
                loc: Loc::new(
                    lhs_size.loc.offset,
                    rhs_size.loc.offset + rhs_size.loc.len - lhs_size.loc.offset,
                ),
            }))
        }
        (Some(size), None) | (None, Some(size)) => Some(ConstraintSpecItem::Size(size.clone())),
        _ => None,
    };

    // step 3: op the results of each
    match op.element {
        ConstraintTreeOperator::Union => {
            let mut union = Vec::with_capacity(values.len() + 1);
            union.extend(values);
            if let Some(size_spec) = size_spec {
                union.push(size_spec);
            }
            Ok(union)
        }
        ConstraintTreeOperator::Intersection => todo!(),
    }
}

fn eval_value_set_binary_expr<
    I1: Iterator<Item = AstElement<ValueConstraint>>,
    I2: Iterator<Item = AstElement<ValueConstraint>>,
>(
    lhs: I1,
    op: ConstraintTreeOperator,
    rhs: I2,
    constrained_type: &BuiltinType,
) -> Result<Vec<AstElement<ValueConstraint>>> {
    let mut all_items = Vec::new();
    for item in lhs {
        all_items.push(item.clone());
    }
    for item in rhs {
        all_items.push(item.clone());
    }

    let mut items: Vec<AstElement<ValueConstraint>> = Vec::with_capacity(all_items.len());
    match op {
        ConstraintTreeOperator::Union => {
            for item in all_items {
                match constrained_type {
                    BuiltinType::Integer(_) => {
                        if !items.is_empty() {
                            let last = items.last_mut().unwrap();
                            let last_item_bounds = get_value_constraint_bounds(&last.element);
                            let item_bounds = get_value_constraint_bounds(&item.element);

                            match last_item_bounds.upper {
                                Bound::Unbounded => continue,
                                last_item_upper_bound @ Bound::Integer(_) => {
                                    let new_upper_bound =
                                        last_item_upper_bound.upper_max(&item_bounds.upper);
                                    if PositionalBound::new(
                                        last_item_upper_bound,
                                        BoundPosition::Upper,
                                    ) >= PositionalBound::new(
                                        item_bounds.lower,
                                        BoundPosition::Lower,
                                    ) {
                                        match &mut last.element {
                                            ValueConstraint::Range(range) => {
                                                range.upper = new_upper_bound;
                                            }
                                            ValueConstraint::SingleValue(value) => {
                                                match &mut value.value {
                                                    BuiltinValue::Integer(int) => {
                                                        *int = match new_upper_bound {
                                                            Bound::Integer(int) => int,
                                                            Bound::Unbounded => panic!("attempt to assign Unbounded to a single value"),
                                                        };
                                                    }
                                                    _ => unreachable!(),
                                                }
                                            }
                                        }
                                        continue;
                                    }
                                }
                            }
                        }

                        items.push(item);
                    }
                    other => match &item.element {
                        ValueConstraint::Range(_) => {
                            return Err(Error {
                                kind: ErrorKind::Ast(format!(
                                    "expecting {} value, but found a range",
                                    other
                                )),
                                loc: item.loc,
                            })
                        }
                        ValueConstraint::SingleValue(_) => {
                            items.push(item.clone());
                        }
                    },
                }
            }
        }
        ConstraintTreeOperator::Intersection => todo!("INTERSECTION"),
    }
    Ok(items)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstraintTreeOperator {
    Union,
    Intersection,
}

impl Display for ConstraintTreeOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Union => "union",
            Self::Intersection => "intersection",
        })
    }
}

#[derive(Debug, Clone)]
pub struct Constraint {
    pub element_sets: Vec<SubtypeElementSet>,
    pub is_extensible: bool,
    pub loc: Loc,
}

impl Constraint {
    pub fn resolve(
        &self,
        context: &Context,
        constrained_type: &BuiltinType,
    ) -> Result<ResolvedConstraint> {
        let mut specs = Vec::with_capacity(self.element_sets.len());
        for element_set in &self.element_sets {
            specs.push(ConstraintSpec {
                items: element_set.tree.resolve(context, constrained_type)?,
                is_extension: element_set.is_extension,
            });
        }
        Ok(ResolvedConstraint {
            specs,
            is_extensible: self.is_extensible,
            loc: self.loc,
        })
    }

    pub fn get_contents_constraint(&self) -> Option<&ContentsConstraint> {
        if self.element_sets.len() == 1 {
            let set = &self.element_sets[0];
            if let ConstraintTree::Element(element) = &set.tree {
                if let SubtypeElement::Contents(contents) = &element.element {
                    return Some(contents);
                }
            }
        }

        None
    }

    pub fn get_table_constraint(&self) -> Option<&TableConstraint> {
        if self.element_sets.len() == 1 {
            let set = &self.element_sets[0];
            if let ConstraintTree::Element(element) = &set.tree {
                if let SubtypeElement::Table(table) = &element.element {
                    return Some(table);
                }
            }
        }

        None
    }
}

#[derive(Debug, Clone)]
pub struct SubtypeElementSet {
    pub tree: ConstraintTree,
    pub is_extension: bool,
}

#[derive(Debug, Clone)]
pub enum SubtypeElement {
    SingleValue(AstElement<TypedValue>),
    ContainedSubtype(TaggedType),
    ValueRange(ValueRange),
    Size(Constraint),
    InnerType(InnerTypeConstraints),
    Contents(ContentsConstraint),
    Table(TableConstraint),
    UserDefined,
}

#[derive(Debug, Clone)]
pub struct TableConstraint {
    pub set_ref: AstElement<QualifiedIdentifier>,
    pub component_ref: Option<ComponentReference>,
}

#[derive(Debug, Clone)]
pub struct ComponentReference {
    pub is_relative: bool,
    pub component_series: Vec<AstElement<String>>,
}

#[derive(Debug, Clone)]
pub struct ContentsConstraint {
    pub content_type: TaggedType,
    pub encoded_by: Option<AstElement<TypedValue>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InnerTypeConstraintsKind {
    Full,
    Partial,
}

#[derive(Debug, Clone)]
pub struct InnerTypeConstraints {
    pub kind: InnerTypeConstraintsKind,
    pub components: Vec<NamedConstraint>,
}

impl InnerTypeConstraints {
    fn lookup_constraint<'a>(&'a self, name: &str) -> Option<&'a ComponentConstraint> {
        self.components.iter().find_map(|component| {
            if &component.name.element == name {
                Some(&component.constraint)
            } else {
                None
            }
        })
    }

    pub fn is_satisfied_by_value(
        &self,
        context: &Context,
        value_type: &BuiltinType,
        value: &BuiltinValue,
    ) -> Result<bool> {
        match (value_type, value) {
            (BuiltinType::Structure(ty), BuiltinValue::Structure(_, val)) => {
                match self.kind {
                    InnerTypeConstraintsKind::Full => {
                        for value_component in &val.components {
                            if self
                                .lookup_constraint(&value_component.name.element)
                                .is_none()
                            {
                                return Ok(false);
                            }
                        }
                    }
                    InnerTypeConstraintsKind::Partial => (),
                }

                for component_constraint in &self.components {
                    let ty_component = ty.components.iter().find(|component| {
                        component.name.element == component_constraint.name.element
                    });
                    let value_component = val.components.iter().find(|component| {
                        component.name.element == component_constraint.name.element
                    });
                    let constraint = &component_constraint.constraint;

                    let presence = match self.kind {
                        InnerTypeConstraintsKind::Full => {
                            // X.680 clause 51.8.10.3:
                            // in full spec, unspecified presence indicates PRESENT
                            constraint.presence.or(Some(Presence::Present))
                        }
                        InnerTypeConstraintsKind::Partial => constraint.presence,
                    };
                    match (presence, value_component) {
                        (Some(Presence::Present), None) | (Some(Presence::Absent), Some(_)) => {
                            return Ok(false)
                        }
                        _ => (),
                    }

                    if let (constraint @ Some(_), Some(component)) =
                        (&constraint.value, value_component)
                    {
                        // TODO: ensure that (component's original constraint) INTERSECTION (component constraint in ITC) != None
                        let constrained_type =
                            ty_component.unwrap().component_type.resolve(context)?;

                        if constrained_type
                            .ty
                            .ensure_satisfied_by_value(
                                context,
                                &component.value,
                                constraint.as_ref(),
                            )
                            .is_err()
                        {
                            return Ok(false);
                        }
                    }
                }

                Ok(true)
            }
            (BuiltinType::Choice(_ty), BuiltinValue::Choice(_val)) => {
                todo!("inner type constraints w/ CHOICE")
            }
            _ => unreachable!(),
        }
    }
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

impl ValueRange {
    pub fn resolve(&self, context: &Context) -> Result<ResolvedValueRange> {
        Ok(ResolvedValueRange {
            lower: self.lower_value(context)?,
            upper: self.upper_value(context)?,
        })
    }

    pub fn lower_value(&self, context: &Context) -> Result<Bound> {
        Ok(match &self.lower {
            RangeLowerBound::Min => Bound::Unbounded,
            RangeLowerBound::Eq(value) => Bound::Integer(resolve_integer!(context, value).clone()),
            RangeLowerBound::Gt(value) => Bound::Integer(resolve_integer!(context, value) + 1),
        })
    }

    pub fn upper_value(&self, context: &Context) -> Result<Bound> {
        Ok(match &self.upper {
            RangeUpperBound::Max => Bound::Unbounded,
            RangeUpperBound::Eq(value) => Bound::Integer(resolve_integer!(context, value).clone()),
            RangeUpperBound::Lt(value) => Bound::Integer(resolve_integer!(context, value) - 1),
        })
    }
}

#[derive(Debug, Clone)]
pub enum RangeLowerBound {
    Eq(AstElement<TypedValue>),
    Gt(AstElement<TypedValue>),
    Min,
}

#[derive(Debug, Clone)]
pub enum RangeUpperBound {
    Eq(AstElement<TypedValue>),
    Lt(AstElement<TypedValue>),
    Max,
}

#[derive(Debug, Clone, Copy)]
pub enum ConstraintCheckMode {
    Value,
    Size,
}

#[derive(Debug, Clone)]
pub enum IntegerInclusion {
    NotIncluded,
    Included { is_extension: bool },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bound {
    /// Represents an unbounded range element, which is either `MIN` or `MAX`.
    Unbounded,
    /// Represents an integer value in either a single value or range constraint.
    Integer(BigInt),
}

impl Bound {
    /// Returns the maximum of two upper bounds.
    /// [`Bound::Unbounded`] represents `MAX` and will be returned if either value is unbounded.
    pub fn upper_max(&self, b: &Self) -> Self {
        match (self, b) {
            (Self::Unbounded, _) | (_, Self::Unbounded) => Self::Unbounded,
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a.max(b).clone()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BoundPosition {
    Lower,
    Upper,
}

#[derive(Eq)]
struct PositionalBound {
    pub bound: Bound,
    pub kind: BoundPosition,
}

impl PositionalBound {
    pub fn new(bound: Bound, kind: BoundPosition) -> PositionalBound {
        PositionalBound { bound, kind }
    }
}

impl PartialEq for PositionalBound {
    fn eq(&self, rhs: &Self) -> bool {
        self.bound == rhs.bound && self.kind == rhs.kind
    }
}

impl PartialOrd for PositionalBound {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        if self == rhs {
            Some(Ordering::Equal)
        } else {
            Some(match (&self.bound, &rhs.bound) {
                (Bound::Unbounded, _) => {
                    match self.kind {
                        BoundPosition::Upper => Ordering::Greater, // MAX > anything
                        BoundPosition::Lower => Ordering::Less,    // MIN < anything
                    }
                }
                (_, Bound::Unbounded) => {
                    match rhs.kind {
                        BoundPosition::Upper => Ordering::Less,    // anything < MAX
                        BoundPosition::Lower => Ordering::Greater, // anything > MIN
                    }
                }
                (Bound::Integer(lhs), Bound::Integer(rhs)) => lhs.cmp(rhs),
            })
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstraintBounds {
    /// The lower bound of the constraint.
    pub lower_bound: Bound,
    /// The 'unextended' upper bound of the constraint.
    /// Any higher bounds defined as extensions to the original constraint are not included.
    /// If the entire constraint is an extension, for example:
    ///
    /// ```asn1
    /// O ::= OCTET STRING (SIZE(10..20), ..., SIZE(20<..30))
    /// ```
    ///
    /// If this instance of ConstraintBounds represented exclusively the second SIZE constraint,
    /// `upper_bound` would be `None` because the entire subtype element is an extension.
    pub upper_bound: Option<Bound>,
    /// The upper bound of the constraint.
    /// This includes bounds defined as extensions to the original constraint.
    pub extended_upper_bound: Bound,
}

impl ConstraintBounds {
    /// Applies the values of another `ConstraintBounds` to this one.
    /// If the applied constraint contains any lower or higher bounds,
    /// they are applied to the bounds of this `ConstraintBounds`.
    pub(crate) fn apply_bounds(&mut self, other_bounds: &ConstraintBounds) {
        macro_rules! apply_bound {
            ( $other_bound:expr, $op:tt, $bound:expr ) => {{
                match (&$other_bound, &$bound) {
                    (Bound::Unbounded, _) => $bound = Bound::Unbounded,
                    (other_bound @ Bound::Integer(other_bound_value), Bound::Integer(bound)) => {
                        if other_bound_value $op bound {
                            $bound = other_bound.clone();
                        }
                    }
                    _ => (),
                }
            }};
        }

        apply_bound!(other_bounds.lower_bound, <, self.lower_bound);
        match (&other_bounds.upper_bound, &mut self.upper_bound) {
            (Some(other_bound), Some(bound)) => apply_bound!(*other_bound, >, *bound),
            (other_bound @ Some(_), None) => self.upper_bound = other_bound.clone(),
            _ => (),
        }
        apply_bound!(other_bounds.extended_upper_bound, >, self.extended_upper_bound);
    }
}

#[cfg(test)]
mod test {
    use crate::{
        compiler::{
            ast::{self, types::TypeAssignmentParseMode, AstParser},
            options::CompilerConfig,
            parser::*,
            Compiler, Context,
        },
        module::ModuleIdentifier,
        types::{Bound, ConstraintBounds},
    };

    use super::ResolvedConstraint;

    fn parse_constraint(context: &mut Context, text: &str) -> ResolvedConstraint {
        let mut stream = TokenStream::from_string(
            &format!(
                "ConstraintTest DEFINITIONS IMPLICIT TAGS ::= BEGIN\n{}\nEND\n",
                text
            ),
            false,
        );
        let pc = ParseContext::new(&mut stream);
        let ast_program = match AstProgram::parse(pc) {
            ParseResult::Ok(ast_constraint) => ast_constraint,
            ParseResult::Fail(err) | ParseResult::Error(err) => panic!("{}", err.kind.message()),
        };

        let config = CompilerConfig::default();
        let compiler = Compiler::new(config);
        {
            assert_eq!(
                ast::register_all_modules(context, &compiler, &ast_program).len(),
                0
            );
        }

        let ast_module = &ast_program.element.0[0];

        macro_rules! make_parser {
            () => {{
                &AstParser {
                    context,
                    compiler: &compiler,
                    config: &compiler.config,
                    ast_module,
                    module: ModuleIdentifier::with_name(String::from("ConstraintTest")),
                }
            }};
        }

        match &ast_module.element.body.element.0[0].element {
            AstAssignment::TypeAssignment(type_assignment) => {
                let ident = {
                    let (ident, tagged_type) = ast::types::parse_type_assignment(
                        make_parser!(),
                        type_assignment,
                        &TypeAssignmentParseMode::Normal,
                    )
                    .unwrap()
                    .unwrap();
                    context.register_type(ident.clone(), tagged_type);
                    ident
                };
                let pending = {
                    let (_, pending) = ast::constraints::parse_type_assignment_constraint(
                        make_parser!(),
                        type_assignment,
                    )
                    .unwrap()
                    .unwrap();
                    pending
                };

                {
                    let decl = context.lookup_type_mut(&ident).unwrap();
                    ast::constraints::apply_pending_constraint(&mut decl.ty, pending);
                }

                let decl = context.lookup_type(&ident).unwrap();
                decl.ty
                    .constraint
                    .as_ref()
                    .unwrap()
                    .resolve(context, &decl.ty.resolve(context).unwrap().ty)
                    .expect("failed to resolve constraint")
            }
            _ => panic!(),
        }
    }

    #[test]
    pub fn test_integer_value_bounds() {
        let mut context = Context::new();
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-2..2)")
                .integer_value_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer((-2).into()),
                upper_bound: Some(Bound::Integer(2.into())),
                extended_upper_bound: Bound::Integer(2.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-2<..2)")
                .integer_value_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer((-1).into()),
                upper_bound: Some(Bound::Integer(2.into())),
                extended_upper_bound: Bound::Integer(2.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-2..<2)")
                .integer_value_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer((-2).into()),
                upper_bound: Some(Bound::Integer(1.into())),
                extended_upper_bound: Bound::Integer(1.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-2<..<2)")
                .integer_value_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer((-1).into()),
                upper_bound: Some(Bound::Integer(1.into())),
                extended_upper_bound: Bound::Integer(1.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (10)")
                .integer_value_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(10.into()),
                upper_bound: Some(Bound::Integer(10.into())),
                extended_upper_bound: Bound::Integer(10.into()),
            })
        );
        assert_eq!(
            parse_constraint(
                &mut context,
                "I ::= INTEGER (2 | 3 | 4 | 5 | 6 | 7 | 80..100)"
            )
            .integer_value_bounds()
            .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(2.into()),
                upper_bound: Some(Bound::Integer(100.into())),
                extended_upper_bound: Bound::Integer(100.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (MIN..MAX)")
                .integer_value_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Unbounded,
                upper_bound: Some(Bound::Unbounded),
                extended_upper_bound: Bound::Unbounded,
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (MIN..0 | 10..20)")
                .integer_value_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Unbounded,
                upper_bound: Some(Bound::Integer(20.into())),
                extended_upper_bound: Bound::Integer(20.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-20..-10 | 0..MAX)")
                .integer_value_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer((-20).into()),
                upper_bound: Some(Bound::Unbounded),
                extended_upper_bound: Bound::Unbounded,
            })
        );
    }

    #[test]
    pub fn test_size_bounds() {
        let mut context = Context::new();
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(0..2))")
                .size_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(0.into()),
                upper_bound: Some(Bound::Integer(2.into())),
                extended_upper_bound: Bound::Integer(2.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(0<..2))")
                .size_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(1.into()),
                upper_bound: Some(Bound::Integer(2.into())),
                extended_upper_bound: Bound::Integer(2.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(0..<2))")
                .size_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(0.into()),
                upper_bound: Some(Bound::Integer(1.into())),
                extended_upper_bound: Bound::Integer(1.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(0<..<2))")
                .size_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(1.into()),
                upper_bound: Some(Bound::Integer(1.into())),
                extended_upper_bound: Bound::Integer(1.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(1))")
                .size_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(1.into()),
                upper_bound: Some(Bound::Integer(1.into())),
                extended_upper_bound: Bound::Integer(1.into()),
            })
        );
        assert_eq!(
            parse_constraint(
                &mut context,
                "O ::= OCTET STRING (SIZE(2 | 3 | 4 | 5 | 6 | 7 | 80..100))"
            )
            .size_bounds()
            .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(2.into()),
                upper_bound: Some(Bound::Integer(100.into())),
                extended_upper_bound: Bound::Integer(100.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(MIN..MAX))")
                .size_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Unbounded,
                upper_bound: Some(Bound::Unbounded),
                extended_upper_bound: Bound::Unbounded,
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(MIN..0 | 10..20))")
                .size_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Unbounded,
                upper_bound: Some(Bound::Integer(20.into())),
                extended_upper_bound: Bound::Integer(20.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(10..20 | 30..MAX))")
                .size_bounds()
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(10.into()),
                upper_bound: Some(Bound::Unbounded),
                extended_upper_bound: Bound::Unbounded,
            })
        );
    }

    #[test]
    pub fn test_extensible_size_bounds() {
        let mut context = Context::new();
        assert_eq!(
            parse_constraint(
                &mut context,
                "O ::= OCTET STRING (SIZE(0..10, ..., 20..30))"
            )
            .size_bounds()
            .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(0.into()),
                upper_bound: Some(Bound::Integer(10.into())),
                extended_upper_bound: Bound::Integer(30.into()),
            })
        );
        assert_eq!(
            parse_constraint(
                &mut context,
                "O ::= OCTET STRING (SIZE(0..10), ..., SIZE(20..30))"
            )
            .size_bounds()
            .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(0.into()),
                upper_bound: Some(Bound::Integer(10.into())),
                extended_upper_bound: Bound::Integer(30.into()),
            })
        );
    }
}
