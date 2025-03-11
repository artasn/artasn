use crate::{compiler::parser::*, module::QualifiedIdentifier, types::*};

use super::{values, AstParser};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConstraintContext {
    Contextless,
    WithinSize,
}

fn parse_constraint(
    parser: &AstParser<'_>,
    constraint: &AstElement<AstConstraint>,
    constrained_type: &ResolvedType,
    ctx: ConstraintContext,
) -> Result<Constraint> {
    let element_sets = &constraint.element.0.element.0;
    let mut constraint = Vec::with_capacity(element_sets.len());
    for element_set in element_sets {
        let element_set = &element_set.element.0;
        let mut elements = Vec::with_capacity(element_set.len());
        for element in element_set {
            elements.push(AstElement::new(
                parse_subtype_element(parser, element, constrained_type, ctx)?,
                element.loc,
            ));
        }
        constraint.push(elements);
    }

    Ok(Constraint(constraint))
}

fn parse_subtype_element(
    parser: &AstParser<'_>,
    ast_subtype_element: &AstElement<AstSubtypeElement>,
    constrained_type: &ResolvedType,
    ctx: ConstraintContext,
) -> Result<SubtypeElement> {
    Ok(match &ast_subtype_element.element {
        AstSubtypeElement::SingleValueConstraint(single_value) => SubtypeElement::SingleValue(
            values::parse_value(parser, &single_value.element.0, constrained_type)?,
        ),
        AstSubtypeElement::ValueRangeConstraint(value_range) => {
            SubtypeElement::ValueRange(ValueRange {
                lower: match &value_range.element.lower.element {
                    AstRangeLowerBound::Value(value) => {
                        RangeLowerBound::Eq(values::parse_value(parser, value, constrained_type)?)
                    }
                    AstRangeLowerBound::GtValue(value) => RangeLowerBound::Gt(values::parse_value(
                        parser,
                        &value.element.0,
                        constrained_type,
                    )?),
                    AstRangeLowerBound::Min(_) => RangeLowerBound::Min,
                },
                upper: match &value_range.element.upper.element {
                    AstRangeUpperBound::Value(value) => {
                        RangeUpperBound::Eq(values::parse_value(parser, value, constrained_type)?)
                    }
                    AstRangeUpperBound::LtValue(value) => RangeUpperBound::Lt(values::parse_value(
                        parser,
                        &value.element.0,
                        constrained_type,
                    )?),
                    AstRangeUpperBound::Max(_) => RangeUpperBound::Max,
                },
            })
        }
        AstSubtypeElement::SizeConstraint(size_constraint) => {
            if ctx == ConstraintContext::WithinSize {
                return Err(Error {
                    kind: ErrorKind::Ast("SIZE constraints cannot be nested".to_string()),
                    loc: ast_subtype_element.loc,
                });
            }
            SubtypeElement::Size(parse_constraint(
                parser,
                &size_constraint.element.0,
                constrained_type,
                ConstraintContext::WithinSize,
            )?)
        }
        AstSubtypeElement::InnerTypeConstraints(itc) => {
            SubtypeElement::InnerType(parse_inner_type_constraints(parser, itc, constrained_type)?)
        }
    })
}

fn parse_inner_type_constraints(
    parser: &AstParser<'_>,
    itc: &AstElement<AstInnerTypeConstraints>,
    constrained_type: &ResolvedType,
) -> Result<InnerTypeConstraints> {
    let components = match &constrained_type.ty {
        BuiltinType::Structure(structure) => &structure.components,
        other => {
            return Err(Error {
                kind: ErrorKind::Ast(format!(
                    "inner type constraints cannot be applied to type {}",
                    other
                )),
                loc: itc.loc,
            })
        }
    };

    let (kind, ast_components) = match &itc.element.0.element {
        AstTypeConstraintSpec::FullSpec(spec) => (InnerTypeConstraintsKind::Full, &spec.element.0),
        AstTypeConstraintSpec::PartialSpec(spec) => {
            (InnerTypeConstraintsKind::Partial, &spec.element.0)
        }
    };
    let components = ast_components
        .element
        .0
        .iter()
        .map(|ast_component| {
            Ok(NamedConstraint {
                name: ast_component
                    .element
                    .name
                    .as_ref()
                    .map(|name| name.0.clone()),
                constraint: {
                    let component_type = match components.iter().find(|component| {
                        component.name.element == ast_component.element.name.element.0
                    }) {
                        Some(component) => component.component_type.resolve(parser.context)?,
                        None => {
                            return Err(Error {
                                kind: ErrorKind::Ast(format!(
                                    "constrained type does not contain a component named '{}'",
                                    ast_component.element.name.element.0
                                )),
                                loc: ast_component.element.name.loc,
                            })
                        }
                    };
                    let (value, presence) = match &ast_component.element.constraint.element {
                        AstComponentConstraint::Constraint(constraint) => (
                            Some(parse_constraint(
                                parser,
                                constraint,
                                &component_type,
                                ConstraintContext::Contextless,
                            )?),
                            None,
                        ),
                        AstComponentConstraint::PresenceConstraint(presence) => {
                            (None, Some(parse_presence(presence)))
                        }
                        AstComponentConstraint::ValuedPresenceConstraint(valued_presence) => (
                            Some(parse_constraint(
                                parser,
                                &valued_presence.element.value,
                                &component_type,
                                ConstraintContext::Contextless,
                            )?),
                            Some(parse_presence(&valued_presence.element.presence)),
                        ),
                    };
                    ComponentConstraint { value, presence }
                },
            })
        })
        .collect::<Result<Vec<NamedConstraint>>>()?;

    Ok(InnerTypeConstraints { kind, components })
}

fn parse_presence(presence: &AstElement<AstPresenceConstraint>) -> Presence {
    match &presence.element {
        AstPresenceConstraint::PresencePresent(_) => Presence::Present,
        AstPresenceConstraint::PresenceAbsent(_) => Presence::Absent,
        AstPresenceConstraint::PresenceOptional(_) => Presence::Optional,
    }
}

fn parse_type_with_constraint(
    of: &AstElement<AstStructureOf>,
) -> Option<AstElement<AstConstraint>> {
    of.element
        .constraint
        .as_ref()
        .map(|constraint| match &constraint.element {
            AstConstraintOrSizeConstraint::Constraint(constraint) => constraint.clone(),
            AstConstraintOrSizeConstraint::SizeConstraint(size_constraint) => {
                let loc = size_constraint.loc;
                AstElement::new(
                    AstConstraint(AstElement::new(
                        AstSubtypeConstraint(vec![AstElement::new(
                            AstSubtypeElementSet(vec![AstElement::new(
                                AstSubtypeElement::SizeConstraint(size_constraint.clone()),
                                loc,
                            )]),
                            loc,
                        )]),
                        loc,
                    )),
                    loc,
                )
            }
        })
}

fn parse_constrained_type(
    parser: &AstParser<'_>,
    ast_constrained_type: &AstElement<AstConstrainedType>,
    constrained_type: &ResolvedType,
) -> Result<Option<Constraint>> {
    let ast_constraint = match &ast_constrained_type.element {
        AstConstrainedType::Suffixed(suffixed) => suffixed.element.constraint.clone(),
        AstConstrainedType::TypeWithConstraint(twc) => parse_type_with_constraint(&twc.element.0),
    };
    match ast_constraint {
        Some(ast_constraint) => Ok(Some(parse_constraint(
            parser,
            &ast_constraint,
            constrained_type,
            ConstraintContext::Contextless,
        )?)),
        None => Ok(None),
    }
}

fn parse_type_constraint(
    parser: &AstParser<'_>,
    ty: &AstElement<AstType>,
    constrained_type: &ResolvedType,
) -> Result<Option<Constraint>> {
    Ok(match &ty.element {
        AstType::TaggedType(tagged_type) => {
            parse_constrained_type(parser, &tagged_type.element.ty, constrained_type)?
        }
        AstType::ConstrainedType(constrained) => {
            parse_constrained_type(parser, constrained, constrained_type)?
        }
    })
}

pub fn parse_type_assignment_constraint(
    parser: &AstParser<'_>,
    type_assignment: &AstElement<AstTypeAssignment>,
) -> Result<Option<(QualifiedIdentifier, Constraint)>> {
    let name = type_assignment.element.name.element.0.clone();
    let ident = QualifiedIdentifier {
        module: parser.module.clone(),
        name,
    };

    let constrained_type = parser.context.lookup_type(&ident).expect("lookup_type");
    let constrained_type = constrained_type.resolve(parser.context)?;

    let constraint = parse_type_constraint(parser, &type_assignment.element.ty, &constrained_type)?;

    match constraint {
        Some(constraint) => Ok(Some((ident, constraint))),
        None => Ok(None),
    }
}
