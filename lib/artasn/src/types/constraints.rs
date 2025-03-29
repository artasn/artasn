use num::{bigint::Sign, BigInt};

use crate::{
    compiler::{
        parser::{AstElement, Error, ErrorKind, Loc, Result},
        Context,
    },
    module::QualifiedIdentifier,
    values::{BuiltinValue, TypedValue, ValueResolve},
};

use super::TaggedType;

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
pub struct Constraint {
    pub elements: Vec<Vec<AstElement<SubtypeElement>>>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum SubtypeElement {
    Extensible,
    SingleValue(AstElement<TypedValue>),
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
    pub ty: TaggedType,
    pub encoded_by: Option<AstElement<TypedValue>>,
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

impl ValueRange {
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

impl Constraint {
    pub fn find<F: Fn(&SubtypeElement) -> bool>(&self, f: F) -> Option<&SubtypeElement> {
        self.flatten().iter().find_map(|subtype| {
            if f(&subtype.element) {
                Some(&subtype.element)
            } else {
                None
            }
        })
    }

    pub fn flatten(&self) -> Vec<&AstElement<SubtypeElement>> {
        self.elements.iter().flatten().collect()
    }

    /// Returns the `(lower, upper)` bounds of the constraint.
    /// If there are no single value or range constraints, None is returned.
    pub fn integer_value_bounds(&self, context: &Context) -> Result<Option<ConstraintBounds>> {
        self.extensible_integer_value_bound(context, false)
    }

    fn extensible_integer_value_bound(
        &self,
        context: &Context,
        mut is_extension: bool,
    ) -> Result<Option<ConstraintBounds>> {
        let mut bounds: Option<ConstraintBounds> = None;

        for subtype in self.flatten() {
            match &subtype.element {
                SubtypeElement::Extensible => {
                    is_extension = true;
                }
                SubtypeElement::SingleValue(single_value) => {
                    let single_value = resolve_integer!(context, single_value);
                    match &mut bounds {
                        Some(constraint_bounds) => {
                            match &mut constraint_bounds.lower_bound {
                                Bound::Integer(lower_bound) if &single_value < lower_bound => {
                                    *lower_bound = single_value.clone();
                                }
                                _ => (),
                            };
                            if !is_extension {
                                match &mut constraint_bounds.upper_bound {
                                    Some(Bound::Integer(upper_bound))
                                        if &single_value > upper_bound =>
                                    {
                                        *upper_bound = single_value.clone();
                                    }
                                    _ => (),
                                };
                            }
                            match &mut constraint_bounds.extended_upper_bound {
                                Bound::Integer(extended_upper_bound)
                                    if &single_value > extended_upper_bound =>
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
                SubtypeElement::ValueRange(range) => {
                    let (lower_value, upper_value) =
                        (range.lower_value(context)?, range.upper_value(context)?);
                    match &mut bounds {
                        Some(constraint_bounds) => {
                            if lower_value == Bound::Unbounded {
                                constraint_bounds.lower_bound = Bound::Unbounded;
                            } else if let (
                                Bound::Integer(lower_value),
                                Bound::Integer(lower_bound),
                            ) = (lower_value, &mut constraint_bounds.lower_bound)
                            {
                                if &lower_value < lower_bound {
                                    *lower_bound = lower_value;
                                }
                            }

                            if !is_extension {
                                if upper_value == Bound::Unbounded {
                                    constraint_bounds.upper_bound = Some(Bound::Unbounded);
                                } else if let (
                                    Bound::Integer(upper_value),
                                    Some(Bound::Integer(upper_bound)),
                                ) =
                                    (upper_value.clone(), &mut constraint_bounds.upper_bound)
                                {
                                    if &upper_value > upper_bound {
                                        *upper_bound = upper_value;
                                    }
                                }
                            }
                            if upper_value == Bound::Unbounded {
                                constraint_bounds.extended_upper_bound = Bound::Unbounded;
                            } else if let (
                                Bound::Integer(upper_value),
                                Bound::Integer(extended_upper_bound),
                            ) = (upper_value, &mut constraint_bounds.extended_upper_bound)
                            {
                                if &upper_value > extended_upper_bound {
                                    *extended_upper_bound = upper_value.clone();
                                }
                            }
                        }
                        None => {
                            bounds = Some(ConstraintBounds {
                                lower_bound: lower_value,
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
                _ => (),
            }
        }

        Ok(bounds)
    }

    pub fn size_bounds(&self, context: &Context) -> Result<Option<ConstraintBounds>> {
        let mut bounds: Option<ConstraintBounds> = None;
        let mut extensible = false;
        for subtype in self.flatten() {
            match &subtype.element {
                SubtypeElement::Extensible => extensible = true,
                SubtypeElement::Size(constraint) => {
                    let size_bounds =
                        constraint.extensible_integer_value_bound(context, extensible)?;
                    if let Some(size_bounds) = size_bounds {
                        match &mut bounds {
                            Some(bounds) => {
                                bounds.apply_bounds(&size_bounds);
                            }
                            None => bounds = Some(size_bounds),
                        }
                    }
                }
                _ => (),
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

    /// Returns true if the constraint is extensible.
    /// If `mode` is `None`, extensibility of any constraints are included.
    /// If `mode` is [`ConstraintCheckMode::Value`], extensibility of value constraints only are included.
    /// If `mode` is [`ConstraintCheckMode::Size`], extensibility of `SIZE` constraints only are included.
    pub fn is_extensible(&self, mode: Option<ConstraintCheckMode>) -> bool {
        for element in self.flatten() {
            match &element.element {
                SubtypeElement::Size(size) => {
                    if matches!(mode, None | Some(ConstraintCheckMode::Size))
                        && size.is_extensible(None)
                    {
                        return true;
                    }
                }
                SubtypeElement::Extensible => {
                    if matches!(mode, None | Some(ConstraintCheckMode::Value)) {
                        return true;
                    }
                }
                _ => (),
            }
        }

        false
    }

    pub fn includes_integer(
        &self,
        context: &Context,
        mode: ConstraintCheckMode,
        value: &BigInt,
    ) -> Result<Option<IntegerInclusion>> {
        macro_rules! cmp_constant {
            ( $value:expr, $op:tt, $constant:expr ) => {{
                $value $op &resolve_integer!(context, $constant)
            }};
        }

        let mut has_matching_constraint = false;
        let mut is_extension = false;
        for element in self.flatten() {
            if let SubtypeElement::Extensible = &element.element {
                is_extension = true;
            }
            match mode {
                ConstraintCheckMode::Value => match &element.element {
                    SubtypeElement::SingleValue(single_value) => {
                        has_matching_constraint = true;
                        if cmp_constant!(value, ==, single_value) {
                            return Ok(Some(IntegerInclusion::Included { is_extension }));
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
                                return Ok(Some(IntegerInclusion::Included { is_extension }));
                            }
                        }
                    }
                    _ => (),
                },
                ConstraintCheckMode::Size => {
                    if let SubtypeElement::Size(size_constraint) = &element.element {
                        has_matching_constraint = true;
                        if let Some(IntegerInclusion::Included {
                            is_extension: is_size_constraint_extension,
                        }) = size_constraint.includes_integer(
                            context,
                            ConstraintCheckMode::Value,
                            value,
                        )? {
                            return Ok(Some(IntegerInclusion::Included {
                                is_extension: is_extension || is_size_constraint_extension,
                            }));
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

    use super::Constraint;

    fn parse_constraint(context: &mut Context, text: &str) -> Constraint {
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

                let decl = context.lookup_type_mut(&ident).unwrap();
                ast::constraints::apply_pending_constraint(&mut decl.ty, pending);

                decl.ty.constraint.as_ref().unwrap().clone()
            }
            _ => panic!(),
        }
    }

    #[test]
    pub fn test_integer_value_bounds() {
        let mut context = Context::new();
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-2..2)")
                .integer_value_bounds(&context)
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer((-2).into()),
                upper_bound: Some(Bound::Integer(2.into())),
                extended_upper_bound: Bound::Integer(2.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-2<..2)")
                .integer_value_bounds(&context)
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer((-1).into()),
                upper_bound: Some(Bound::Integer(2.into())),
                extended_upper_bound: Bound::Integer(2.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-2..<2)")
                .integer_value_bounds(&context)
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer((-2).into()),
                upper_bound: Some(Bound::Integer(1.into())),
                extended_upper_bound: Bound::Integer(1.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-2<..<2)")
                .integer_value_bounds(&context)
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer((-1).into()),
                upper_bound: Some(Bound::Integer(1.into())),
                extended_upper_bound: Bound::Integer(1.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (10)")
                .integer_value_bounds(&context)
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
            .integer_value_bounds(&context)
            .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(2.into()),
                upper_bound: Some(Bound::Integer(100.into())),
                extended_upper_bound: Bound::Integer(100.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (MIN..MAX)")
                .integer_value_bounds(&context)
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Unbounded,
                upper_bound: Some(Bound::Unbounded),
                extended_upper_bound: Bound::Unbounded,
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (MIN..0 | 10..20)")
                .integer_value_bounds(&context)
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Unbounded,
                upper_bound: Some(Bound::Integer(20.into())),
                extended_upper_bound: Bound::Integer(20.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-20..-10 | 0..MAX)")
                .integer_value_bounds(&context)
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
                .size_bounds(&context)
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(0.into()),
                upper_bound: Some(Bound::Integer(2.into())),
                extended_upper_bound: Bound::Integer(2.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(0<..2))")
                .size_bounds(&context)
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(1.into()),
                upper_bound: Some(Bound::Integer(2.into())),
                extended_upper_bound: Bound::Integer(2.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(0..<2))")
                .size_bounds(&context)
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(0.into()),
                upper_bound: Some(Bound::Integer(1.into())),
                extended_upper_bound: Bound::Integer(1.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(0<..<2))")
                .size_bounds(&context)
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(1.into()),
                upper_bound: Some(Bound::Integer(1.into())),
                extended_upper_bound: Bound::Integer(1.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(1))")
                .size_bounds(&context)
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
            .size_bounds(&context)
            .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(2.into()),
                upper_bound: Some(Bound::Integer(100.into())),
                extended_upper_bound: Bound::Integer(100.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(MIN..MAX))")
                .size_bounds(&context)
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Unbounded,
                upper_bound: Some(Bound::Unbounded),
                extended_upper_bound: Bound::Unbounded,
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(MIN..0 | 10..20))")
                .size_bounds(&context)
                .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Unbounded,
                upper_bound: Some(Bound::Integer(20.into())),
                extended_upper_bound: Bound::Integer(20.into()),
            })
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(10..20 | 30..MAX))")
                .size_bounds(&context)
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
            .size_bounds(&context)
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
            .size_bounds(&context)
            .unwrap(),
            Some(ConstraintBounds {
                lower_bound: Bound::Integer(0.into()),
                upper_bound: Some(Bound::Integer(10.into())),
                extended_upper_bound: Bound::Integer(30.into()),
            })
        );
    }
}
