use num::BigInt;

use crate::{
    compiler::{
        parser::{AstElement, Error, ErrorKind, Result},
        Context,
    },
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
    pub extensible: bool,
}

#[derive(Debug, Clone)]
pub enum SubtypeElement {
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
    pub set_name: AstElement<String>,
    pub field_ref: Option<AstElement<String>>,
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
    pub fn lower_value(&self, context: &Context) -> Result<Option<BigInt>> {
        Ok(match &self.lower {
            RangeLowerBound::Min => None,
            RangeLowerBound::Eq(value) => Some(resolve_integer!(context, value).clone()),
            RangeLowerBound::Gt(value) => Some(resolve_integer!(context, value) + 1),
        })
    }

    pub fn upper_value(&self, context: &Context) -> Result<Option<BigInt>> {
        Ok(match &self.upper {
            RangeUpperBound::Max => None,
            RangeUpperBound::Eq(value) => Some(resolve_integer!(context, value).clone()),
            RangeUpperBound::Lt(value) => Some(resolve_integer!(context, value) - 1),
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
    /// If the lower bound is `MIN`, the first element of the returned tuple is None.
    /// If the upper bound is `MAX`, the second element of the returned tuple is None.
    pub fn integer_value_bounds(
        &self,
        context: &Context,
    ) -> Result<Option<(Option<BigInt>, Option<BigInt>)>> {
        let mut bounds = None;

        for subtype in self.flatten() {
            match &subtype.element {
                SubtypeElement::SingleValue(single_value) => {
                    let single_value = resolve_integer!(context, single_value);
                    match &mut bounds {
                        Some((lower_bound, upper_bound)) => {
                            match lower_bound {
                                Some(lower_bound) if &single_value < lower_bound => {
                                    *lower_bound = single_value.clone();
                                }
                                _ => (),
                            };
                            match upper_bound {
                                Some(upper_bound) if &single_value > upper_bound => {
                                    *upper_bound = single_value.clone();
                                }
                                _ => (),
                            };
                        }
                        None => {
                            bounds = Some((Some(single_value.clone()), Some(single_value.clone())))
                        }
                    }
                }
                SubtypeElement::ValueRange(range) => {
                    let (lower_value, upper_value) =
                        (range.lower_value(context)?, range.upper_value(context)?);
                    match &mut bounds {
                        Some((lower_bound, upper_bound)) => {
                            if lower_value.is_none() {
                                *lower_bound = None;
                            } else if let (Some(lower_value), Some(lower_bound)) =
                                (lower_value, lower_bound)
                            {
                                if &lower_value < lower_bound {
                                    *lower_bound = lower_value;
                                }
                            }
                            if upper_value.is_none() {
                                *upper_bound = None;
                            } else if let (Some(upper_value), Some(upper_bound)) =
                                (upper_value, upper_bound)
                            {
                                if &upper_value > upper_bound {
                                    *upper_bound = upper_value.clone();
                                }
                            }
                        }
                        None => bounds = Some((lower_value, upper_value)),
                    }
                }
                _ => (),
            }
        }

        Ok(bounds)
    }

    pub fn size_bounds(
        &self,
        context: &Context,
    ) -> Result<Option<(Option<BigInt>, Option<BigInt>)>> {
        let mut bounds = None;
        for subtype in self.flatten() {
            if let SubtypeElement::Size(constraint) = &subtype.element {
                let size_bounds = constraint.integer_value_bounds(context)?;
                if let Some(size_bounds) = size_bounds {
                    let (lower_size, upper_size) = size_bounds;
                    match &mut bounds {
                        Some((lower_bound, upper_bound)) => {
                            if lower_size.is_none() {
                                *lower_bound = None;
                            } else if let (Some(lower_size), Some(lower_bound)) =
                                (lower_size, lower_bound)
                            {
                                if &lower_size < lower_bound {
                                    *lower_bound = lower_size;
                                }
                            }
                            if upper_size.is_none() {
                                *upper_bound = None;
                            } else if let (Some(upper_size), Some(upper_bound)) =
                                (upper_size, upper_bound)
                            {
                                if &upper_size > upper_bound {
                                    *upper_bound = upper_size.clone();
                                }
                            }
                        }
                        None => bounds = Some((lower_size, upper_size)),
                    }
                }
            }
        }
        Ok(bounds)
    }

    pub fn is_size_extensible(&self) -> bool {
        if self.extensible {
            return true;
        }

        for subtype in self.flatten() {
            if let SubtypeElement::Size(constraint) = &subtype.element {
                if constraint.extensible {
                    return true;
                }
            }
        }

        false
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

    pub fn includes_integer(
        &self,
        context: &Context,
        mode: ConstraintCheckMode,
        value: &BigInt,
    ) -> Result<Option<bool>> {
        macro_rules! cmp_constant {
            ( $value:expr, $op:tt, $constant:expr ) => {{
                $value $op &resolve_integer!(context, $constant)
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
                ConstraintCheckMode::Size => {
                    if let SubtypeElement::Size(size_constraint) = &element.element {
                        has_matching_constraint = true;
                        if let Some(result) = size_constraint.includes_integer(
                            context,
                            ConstraintCheckMode::Value,
                            value,
                        )? {
                            return Ok(Some(result));
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
            Some(false)
        } else {
            None
        })
    }
}

#[cfg(test)]
mod test {
    use crate::{
        compiler::{
            ast::{
                self,
                types::{ParsedTypeAssignment, TypeAssignmentParseMode},
                AstParser,
            },
            options::CompilerConfig,
            parser::*,
            Context,
        },
        module::ModuleIdentifier,
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
        {
            assert_eq!(
                ast::register_all_modules(context, &config, &ast_program).len(),
                0
            );
        }

        let ast_module = &ast_program.element.0[0];

        macro_rules! make_parser {
            () => {{
                &AstParser {
                    context,
                    config: &config,
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
                    let tagged_type = match tagged_type {
                        ParsedTypeAssignment::DeclaredType(decl) => decl,
                        ParsedTypeAssignment::InformationObjectClass(_) => {
                            panic!("unexpected CLASS")
                        }
                    };
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
            Some((Some((-2).into()), Some(2.into())))
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-2<..2)")
                .integer_value_bounds(&context)
                .unwrap(),
            Some((Some((-1).into()), Some(2.into())))
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-2..<2)")
                .integer_value_bounds(&context)
                .unwrap(),
            Some((Some((-2).into()), Some(1.into())))
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-2<..<2)")
                .integer_value_bounds(&context)
                .unwrap(),
            Some((Some((-1).into()), Some(1.into())))
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (10)")
                .integer_value_bounds(&context)
                .unwrap(),
            Some((Some(10.into()), Some(10.into())))
        );
        assert_eq!(
            parse_constraint(
                &mut context,
                "I ::= INTEGER (2 | 3 | 4 | 5 | 6 | 7 | 80..100)"
            )
            .integer_value_bounds(&context)
            .unwrap(),
            Some((Some(2.into()), Some(100.into())))
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (MIN..MAX)")
                .integer_value_bounds(&context)
                .unwrap(),
            Some((None, None))
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (MIN..0 | 10..20)")
                .integer_value_bounds(&context)
                .unwrap(),
            Some((None, Some(20.into())))
        );
        assert_eq!(
            parse_constraint(&mut context, "I ::= INTEGER (-20..-10 | 0..MAX)")
                .integer_value_bounds(&context)
                .unwrap(),
            Some((Some((-20).into()), None))
        );
    }

    #[test]
    pub fn test_size_bounds() {
        let mut context = Context::new();
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(0..2))")
                .size_bounds(&context)
                .unwrap(),
            Some((Some(0.into()), Some(2.into())))
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(0<..2))")
                .size_bounds(&context)
                .unwrap(),
            Some((Some(1.into()), Some(2.into())))
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(0..<2))")
                .size_bounds(&context)
                .unwrap(),
            Some((Some(0.into()), Some(1.into())))
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(0<..<2))")
                .size_bounds(&context)
                .unwrap(),
            Some((Some(1.into()), Some(1.into())))
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(1))")
                .size_bounds(&context)
                .unwrap(),
            Some((Some(1.into()), Some(1.into())))
        );
        assert_eq!(
            parse_constraint(
                &mut context,
                "O ::= OCTET STRING (SIZE(2 | 3 | 4 | 5 | 6 | 7 | 80..100))"
            )
            .size_bounds(&context)
            .unwrap(),
            Some((Some(2.into()), Some(100.into())))
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(MIN..MAX))")
                .size_bounds(&context)
                .unwrap(),
            Some((None, None))
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(MIN..0 | 10..20))")
                .size_bounds(&context)
                .unwrap(),
            Some((None, Some(20.into())))
        );
        assert_eq!(
            parse_constraint(&mut context, "O ::= OCTET STRING (SIZE(-20..-10 | 0..MAX))")
                .size_bounds(&context)
                .unwrap(),
            Some((Some((-20).into()), None))
        );
    }
}
