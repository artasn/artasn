use crate::{compiler::parser::*, module::QualifiedIdentifier, types::*};

use super::{
    types::{self, TypeContext},
    values, AstParser,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConstraintContext {
    Contextless,
    WithinSize,
}

fn parse_constraint(
    parser: &AstParser<'_>,
    ast_constraint: &AstElement<AstConstraint>,
    constrained_type: &ResolvedType,
    ctx: ConstraintContext,
) -> Result<Constraint> {
    let element_sets = &ast_constraint.element.0.element.element_sets;
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

    Ok(Constraint {
        elements: constraint,
        extensible: ast_constraint.element.0.element.extensible,
    })
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
        AstSubtypeElement::ContentsConstraint(contents) => SubtypeElement::Contents(
            parse_contents_constraint(parser, contents, constrained_type)?,
        ),
    })
}

fn parse_contents_constraint(
    parser: &AstParser<'_>,
    contents: &AstElement<AstContentsConstraint>,
    constrained_type: &ResolvedType,
) -> Result<ContentsConstraint> {
    match &constrained_type.ty {
        BuiltinType::BitString(_) | BuiltinType::OctetString => (),
        other => {
            return Err(Error {
                kind: ErrorKind::Ast(format!("contents constraint is invalid for {}", other,)),
                loc: contents.loc,
            })
        }
    };

    let (ty, encoded_by) = match &contents.element {
        AstContentsConstraint::Containing(containing) => (&containing.element.0, None),
        AstContentsConstraint::EncodedBy(eb) => (&eb.element.ty, Some(&eb.element.value)),
    };
    let ty = types::parse_type(parser, ty, TypeContext::Contextless)?;
    let encoded_by = match encoded_by {
        Some(encoded_by) => Some(values::parse_value(
            parser,
            encoded_by,
            &ResolvedType::universal(TagType::ObjectIdentifier),
        )?),
        None => None,
    };
    Ok(ContentsConstraint { ty, encoded_by })
}

fn parse_inner_type_constraints(
    parser: &AstParser<'_>,
    itc: &AstElement<AstInnerTypeConstraints>,
    constrained_type: &ResolvedType,
) -> Result<InnerTypeConstraints> {
    let components: Vec<(&AstElement<String>, &Box<TaggedType>)> = match &constrained_type.ty {
        BuiltinType::Structure(structure) => structure
            .components
            .iter()
            .map(|component| (&component.name, &component.component_type))
            .collect(),
        BuiltinType::Choice(choice) => choice
            .alternatives
            .iter()
            .map(|alternative| (&alternative.name, &alternative.alternative_type))
            .collect(),
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
                        component.0.element == ast_component.element.name.element.0
                    }) {
                        Some(component) => component.1.resolve(parser.context)?,
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
                        AstSubtypeConstraint {
                            element_sets: vec![AstElement::new(
                                AstSubtypeElementSet(vec![AstElement::new(
                                    AstSubtypeElement::SizeConstraint(size_constraint.clone()),
                                    loc,
                                )]),
                                loc,
                            )],
                            extensible: false,
                        },
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
) -> Result<PendingConstraint> {
    let ast_constraint = match &ast_constrained_type.element {
        AstConstrainedType::Suffixed(suffixed) => suffixed.element.constraint.clone(),
        AstConstrainedType::TypeWithConstraint(twc) => parse_type_with_constraint(&twc.element.0),
    };
    let constraint = match ast_constraint {
        Some(ast_constraint) => Some(parse_constraint(
            parser,
            &ast_constraint,
            constrained_type,
            ConstraintContext::Contextless,
        )?),
        None => None,
    };
    let component_constraints = match &ast_constrained_type.element {
        AstConstrainedType::Suffixed(suffixed) => match &suffixed.element.ty.element {
            AstUntaggedType::BuiltinType(builtin) => match &builtin.element {
                AstBuiltinType::Structure(structure) => {
                    let resolved_components = match &constrained_type.ty {
                        BuiltinType::Structure(structure) => &structure.components,
                        _ => unreachable!(),
                    };

                    let mut component_constraints =
                        Vec::with_capacity(structure.element.components.len());
                    for component in &structure.element.components {
                        let resolved_component = resolved_components
                            .iter()
                            .find(|resolved_component| {
                                resolved_component.name.element == component.element.name.element.0
                            })
                            .expect("resolved type missing component from ast type");
                        let component_type =
                            resolved_component.component_type.resolve(parser.context)?;
                        component_constraints.push((
                            resolved_component.name.element.clone(),
                            parse_type_constraint(parser, &component.element.ty, &component_type)?,
                        ));
                    }
                    component_constraints
                }
                AstBuiltinType::Choice(choice) => {
                    let resolved_alternatives = match &constrained_type.ty {
                        BuiltinType::Choice(choice) => &choice.alternatives,
                        _ => unreachable!(),
                    };

                    let mut alternative_constraints = Vec::with_capacity(choice.element.0.len());
                    for alternative in &choice.element.0 {
                        let resolved_alternative = resolved_alternatives
                            .iter()
                            .find(|resolved_alternative| {
                                resolved_alternative.name.element
                                    == alternative.element.name.element.0
                            })
                            .expect("resolved type missing alternative from ast type");
                        let component_type = resolved_alternative
                            .alternative_type
                            .resolve(parser.context)?;
                        alternative_constraints.push((
                            resolved_alternative.name.element.clone(),
                            parse_type_constraint(
                                parser,
                                &alternative.element.ty,
                                &component_type,
                            )?,
                        ));
                    }
                    alternative_constraints
                }
                _ => Vec::new(),
            },
            _ => Vec::new(),
        },
        _ => Vec::new(),
    };
    Ok(PendingConstraint {
        constraint,
        component_constraints,
    })
}

fn parse_type_constraint(
    parser: &AstParser<'_>,
    ty: &AstElement<AstType>,
    constrained_type: &ResolvedType,
) -> Result<PendingConstraint> {
    Ok(match &ty.element {
        AstType::TaggedType(tagged_type) => {
            parse_constrained_type(parser, &tagged_type.element.ty, constrained_type)?
        }
        AstType::ConstrainedType(constrained) => {
            parse_constrained_type(parser, constrained, constrained_type)?
        }
    })
}

pub struct PendingConstraint {
    pub constraint: Option<Constraint>,
    pub component_constraints: Vec<(String, PendingConstraint)>,
}

pub fn parse_type_assignment_constraint(
    parser: &AstParser<'_>,
    type_assignment: &AstElement<AstTypeAssignment>,
) -> Result<(QualifiedIdentifier, PendingConstraint)> {
    let name = type_assignment.element.name.element.0.clone();
    let ident = QualifiedIdentifier::new(parser.module.clone(), name);

    let constrained_type = parser.context.lookup_type(&ident).expect("lookup_type");
    let constrained_type = constrained_type.resolve(parser.context)?;

    let pending = parse_type_constraint(parser, &type_assignment.element.ty, &constrained_type)?;
    Ok((ident, pending))
}

pub fn apply_pending_constraint(tagged_type: &mut TaggedType, pending: PendingConstraint) {
    if let Some(constraint) = pending.constraint {
        tagged_type.constraint = Some(constraint);
    }
    if !pending.component_constraints.is_empty() {
        match &mut tagged_type.ty {
            UntaggedType::BuiltinType(builtin) => match builtin {
                BuiltinType::Structure(structure) => {
                    for (component_name, pending) in pending.component_constraints {
                        let component = structure
                            .components
                            .iter_mut()
                            .find(|component| component.name.element == component_name)
                            .expect("pending constraint component not found in type");
                        apply_pending_constraint(&mut component.component_type, pending);
                    }
                }
                BuiltinType::Choice(choice) => {
                    for (alternative_name, pending) in pending.component_constraints {
                        let alternative = choice
                            .alternatives
                            .iter_mut()
                            .find(|alternative| alternative.name.element == alternative_name)
                            .expect("pending constraint alternative not found in type");
                        apply_pending_constraint(&mut alternative.alternative_type, pending);
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!("typereference aliases can't have constrained components"),
        }
    }
}
