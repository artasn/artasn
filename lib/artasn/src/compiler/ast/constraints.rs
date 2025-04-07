use crate::{
    compiler::{parser::*, Context},
    module::QualifiedIdentifier,
    types::*,
    values::TypedValue,
};

use super::{
    types::{self, Parameter},
    values::{self, ParseValueAssignmentStage},
    AstParser,
};

use super::constraint_tree::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConstraintContext {
    Contextless,
    WithinSize,
}

fn parse_constraint(
    parser: &AstParser<'_>,
    ast_constraint: &AstElement<AstConstraint>,
    constrained_type: &ResolvedType,
    parameters: &[(&String, &Parameter)],
    ctx: ConstraintContext,
) -> Result<Constraint> {
    enum ElementKind {
        Extensiblity,
        SubtypeElementSet,
    }

    let ast_element_sets = &ast_constraint.element.0.element.element_sets;
    let mut element_sets = Vec::with_capacity(ast_element_sets.len());
    let mut is_extension = false;
    let mut last_element_kind = None;
    for element_set in ast_element_sets {
        match &element_set.element {
            AstConstraintElement::Extensible(ast) => {
                if last_element_kind.is_none() {
                    return Err(Error {
                        kind: ErrorKind::Ast(
                            "extensibility marker must appear after the base set of constraints"
                                .to_string(),
                        ),
                        loc: ast.loc,
                    });
                }

                if is_extension {
                    return Err(Error {
                        kind: ErrorKind::Ast(
                            "extensibility marker can only appear once in a constraint".to_string(),
                        ),
                        loc: ast.loc,
                    });
                }

                is_extension = true;
                last_element_kind = Some(ElementKind::Extensiblity);
            }
            AstConstraintElement::SubtypeElementSet(element_set) => {
                if matches!(last_element_kind, Some(ElementKind::SubtypeElementSet)) {
                    return Err(Error {
                        kind: ErrorKind::Ast("expecting set operator ('|', 'UNION', '^', or 'INTERSECTION'), but found operator ','".to_string()),
                        loc: element_set.loc,
                    });
                }

                let tree = parse_subtype_element_set(
                    parser,
                    element_set,
                    constrained_type,
                    parameters,
                    ctx,
                )?;
                last_element_kind = Some(ElementKind::SubtypeElementSet);
                element_sets.push(SubtypeElementSet { tree, is_extension });
            }
        };
    }

    Ok(Constraint {
        element_sets,
        is_extensible: is_extension,
        loc: ast_constraint.loc,
    })
}

fn parse_subtype_element_set(
    parser: &AstParser<'_>,
    ast_element_set: &AstElement<AstSubtypeElementSet>,
    constrained_type: &ResolvedType,
    parameters: &[(&String, &Parameter)],
    ctx: ConstraintContext,
) -> Result<ConstraintTree> {
    let token_count =
        ast_element_set.element.elements.len() + ast_element_set.element.operators.len();
    let mut tokens = Vec::with_capacity(token_count);
    for i in 0..token_count {
        if i % 2 == 0 {
            tokens.push(ElementSetToken::Operand(
                &ast_element_set.element.elements[i / 2],
            ));
        } else {
            let operator = ast_element_set.element.operators[i / 2]
                .as_ref()
                .map(|op| match op {
                    AstConstraintOperator::Union(_) | AstConstraintOperator::KeywordUnion(_) => {
                        SetOperator::Union
                    }
                    AstConstraintOperator::Intersection(_)
                    | AstConstraintOperator::KeywordIntersection(_) => SetOperator::Intersection,
                });
            tokens.push(ElementSetToken::Operator(operator));
        }
    }

    PRATT
        .map_primary(|operand| match &operand.element {
            AstSubtypeElement::SubtypeElementGroup(group) => parse_subtype_element_set(
                parser,
                &group.element.0,
                constrained_type,
                parameters,
                ctx,
            ),
            _ => parse_subtype_element(parser, operand, constrained_type, parameters, ctx)
                .map(|element| ConstraintTree::Element(AstElement::new(element, operand.loc))),
        })
        .map_infix(|lhs, op, rhs| {
            let lhs = lhs?;
            let rhs = rhs?;
            let op = op.as_ref().map(|op| match op {
                SetOperator::Union => ConstraintTreeOperator::Union,
                SetOperator::Intersection => ConstraintTreeOperator::Intersection,
            });

            Ok(ConstraintTree::BinaryExpr {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            })
        })
        .parse(tokens.into_iter())
}

fn resolve_value_parameter(
    parameters: &[(&String, &Parameter)],
    constraint_value: &AstElement<AstValue>,
) -> Result<Option<AstElement<TypedValue>>> {
    match &constraint_value.element {
        AstValue::DefinedValue(defined_value)
            if defined_value.element.external_module.is_none() =>
        {
            let param = parameters.iter().find_map(|(name, param)| {
                if *name == &defined_value.element.value.element.0 {
                    Some(param)
                } else {
                    None
                }
            });
            if let Some(param) = param {
                match param {
                    Parameter::Value { value, .. } => return Ok(Some(value.clone())),
                    other => {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "expecting value parameter, but found {} parameter",
                                other.get_name()
                            )),
                            loc: constraint_value.loc,
                        })
                    }
                }
            }
        }
        _ => (),
    }

    Ok(None)
}

fn resolve_named_number(
    constrained_type: &ResolvedType,
    constraint_value: &AstElement<AstValue>,
) -> Result<Option<AstElement<TypedValue>>> {
    match &constraint_value.element {
        AstValue::DefinedValue(defined_value)
            if defined_value.element.external_module.is_none() =>
        {
            if let BuiltinType::Integer(integer) = &constrained_type.ty {
                if let Some(named_values) = &integer.named_values {
                    for named_value in named_values {
                        if defined_value.element.value.element.0 == named_value.name.element {
                            return Ok(Some(named_value.value.clone()));
                        }
                    }
                }
            }
        }
        _ => (),
    }

    Ok(None)
}

fn parse_subtype_element(
    parser: &AstParser<'_>,
    ast_subtype_element: &AstElement<AstSubtypeElement>,
    constrained_type: &ResolvedType,
    parameters: &[(&String, &Parameter)],
    ctx: ConstraintContext,
) -> Result<SubtypeElement> {
    macro_rules! resolve_value {
        ( $value:expr, $or:tt ) => {{
            let value = $value;
            match resolve_named_number(constrained_type, value)? {
                Some(named_number) => named_number,
                None => match resolve_value_parameter(parameters, value)? {
                    Some(param_value) => param_value,
                    None => $or,
                },
            }
        }};
    }

    if ctx == ConstraintContext::WithinSize
        && !matches!(
            &ast_subtype_element.element,
            AstSubtypeElement::SingleValueConstraint(_)
                | AstSubtypeElement::ValueRangeConstraint(_)
        )
    {
        return Err(Error {
            kind: ErrorKind::Ast(
                "only single value and value range constraints are allowed in SIZE constraints"
                    .to_string(),
            ),
            loc: ast_subtype_element.loc,
        });
    }

    Ok(match &ast_subtype_element.element {
        AstSubtypeElement::SubtypeElementGroup(_) => {
            unreachable!("parsed within PRATT.map_primary")
        }
        AstSubtypeElement::SingleValueConstraint(single_value) => {
            let single_value = resolve_value!(&single_value.element.0, {
                values::parse_value(
                    parser,
                    ParseValueAssignmentStage::Normal,
                    &single_value.element.0,
                    constrained_type,
                )?
            });
            SubtypeElement::SingleValue(single_value)
        }
        AstSubtypeElement::ContainedSubtype(contained_subtype) => SubtypeElement::ContainedSubtype(
            types::parse_type(parser, &contained_subtype.element.0, parameters)?,
        ),
        AstSubtypeElement::ValueRangeConstraint(value_range) => {
            match &constrained_type.ty {
                BuiltinType::Integer(_) => (),
                other => {
                    if ctx != ConstraintContext::WithinSize {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "value range constraints cannot be applied to type {}",
                                other,
                            )),
                            loc: value_range.loc,
                        });
                    }
                }
            }
            SubtypeElement::ValueRange(ValueRange {
                lower: match &value_range.element.lower.element {
                    AstRangeLowerBound::RangeLowerBoundValue(value) => {
                        RangeLowerBound::Eq(match &value.element {
                            AstRangeLowerBoundValue::Min(_) => RangeBoundValue::Unbounded,
                            AstRangeLowerBoundValue::Value(value) => {
                                RangeBoundValue::Integer(resolve_value!(value, {
                                    values::parse_value(
                                        parser,
                                        ParseValueAssignmentStage::Normal,
                                        value,
                                        constrained_type,
                                    )?
                                }))
                            }
                        })
                    }
                    AstRangeLowerBound::GtValue(value) => {
                        RangeLowerBound::Gt(match &value.element.0.element {
                            AstRangeLowerBoundValue::Min(_) => RangeBoundValue::Unbounded,
                            AstRangeLowerBoundValue::Value(value) => {
                                RangeBoundValue::Integer(resolve_value!(value, {
                                    values::parse_value(
                                        parser,
                                        ParseValueAssignmentStage::Normal,
                                        value,
                                        constrained_type,
                                    )?
                                }))
                            }
                        })
                    }
                },
                upper: match &value_range.element.upper.element {
                    AstRangeUpperBound::RangeUpperBoundValue(value) => {
                        RangeUpperBound::Eq(match &value.element {
                            AstRangeUpperBoundValue::Max(_) => RangeBoundValue::Unbounded,
                            AstRangeUpperBoundValue::Value(value) => {
                                RangeBoundValue::Integer(resolve_value!(value, {
                                    values::parse_value(
                                        parser,
                                        ParseValueAssignmentStage::Normal,
                                        value,
                                        constrained_type,
                                    )?
                                }))
                            }
                        })
                    }
                    AstRangeUpperBound::LtValue(value) => {
                        RangeUpperBound::Lt(match &value.element.0.element {
                            AstRangeUpperBoundValue::Max(_) => RangeBoundValue::Unbounded,
                            AstRangeUpperBoundValue::Value(value) => {
                                RangeBoundValue::Integer(resolve_value!(value, {
                                    values::parse_value(
                                        parser,
                                        ParseValueAssignmentStage::Normal,
                                        value,
                                        constrained_type,
                                    )?
                                }))
                            }
                        })
                    }
                },
            })
        }
        AstSubtypeElement::SizeConstraint(size_constraint) => {
            if ctx == ConstraintContext::WithinSize {
                return Err(Error {
                    kind: ErrorKind::Ast("size constraints cannot be nested".to_string()),
                    loc: ast_subtype_element.loc,
                });
            }
            match &constrained_type.ty {
                BuiltinType::BitString(_)
                | BuiltinType::OctetString
                | BuiltinType::StructureOf(_)
                | BuiltinType::CharacterString(_) => (),
                other => {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "size constraints cannot be applied to type {}",
                            other,
                        )),
                        loc: size_constraint.loc,
                    })
                }
            }
            SubtypeElement::Size(parse_constraint(
                parser,
                &size_constraint.element.0,
                constrained_type,
                parameters,
                ConstraintContext::WithinSize,
            )?)
        }
        AstSubtypeElement::InnerTypeConstraints(itc) => SubtypeElement::InnerType(
            parse_inner_type_constraints(parser, itc, constrained_type, parameters)?,
        ),
        AstSubtypeElement::ContentsConstraint(contents) => SubtypeElement::Contents(
            parse_contents_constraint(parser, contents, constrained_type, parameters)?,
        ),
        AstSubtypeElement::TableConstraint(table) => {
            match &constrained_type.ty {
                BuiltinType::Any => (),
                _ => {
                    return Err(Error {
                        kind: ErrorKind::Ast(
                            "table constraints can only be applied to information object class field references".to_string()
                        ),
                        loc: table.loc,
                    });
                }
            }
            SubtypeElement::Table(parse_table_constraint(parser, table, parameters)?)
        }
        AstSubtypeElement::UserDefinedConstraint(_) => SubtypeElement::UserDefined,
    })
}

fn parse_table_constraint(
    parser: &AstParser<'_>,
    table: &AstElement<AstTableConstraint>,
    parameters: &[(&String, &Parameter)],
) -> Result<TableConstraint> {
    let set_name = &table.element.set_name.element.ty;
    let set_parameter = if table.element.set_name.element.external_module.is_none() {
        parameters.iter().find_map(|(name, parameter)| {
            if *name == &set_name.element.0 {
                Some(*parameter)
            } else {
                None
            }
        })
    } else {
        None
    };
    let set_ref = match set_parameter {
        Some(param) => match param {
            Parameter::ObjectSet { set_ref, .. } => set_ref.clone(),
            other => {
                return Err(Error {
                    kind: ErrorKind::Ast(format!(
                    "expecting {} to be an information object class set parameter, but found {}",
                    set_name.element.0,
                    other.get_name()
                )),
                    loc: set_name.loc,
                })
            }
        },
        None => types::resolve_defined_type(parser, &table.element.set_name)?,
    };
    Ok(TableConstraint {
        set_ref,
        component_ref: table.element.component_ref.as_ref().map(|component_ref| {
            ComponentReference {
                is_relative: component_ref.element.relative,
                component_series: component_ref
                    .element
                    .elements
                    .iter()
                    .map(|element| element.as_ref().map(|element| element.0.clone()))
                    .collect(),
            }
        }),
    })
}

fn parse_contents_constraint(
    parser: &AstParser<'_>,
    contents: &AstElement<AstContentsConstraint>,
    constrained_type: &ResolvedType,
    parameters: &[(&String, &Parameter)],
) -> Result<ContentsConstraint> {
    match &constrained_type.ty {
        BuiltinType::BitString(_) | BuiltinType::OctetString => (),
        other => {
            return Err(Error {
                kind: ErrorKind::Ast(format!(
                    "contents constraints cannot be applied to type {}",
                    other,
                )),
                loc: contents.loc,
            })
        }
    };

    let (ty, encoded_by) = match &contents.element {
        AstContentsConstraint::Containing(containing) => (&containing.element.0, None),
        AstContentsConstraint::EncodedBy(eb) => (&eb.element.ty, Some(&eb.element.value)),
    };
    let ty = types::parse_type(parser, ty, parameters)?;
    let encoded_by = match encoded_by {
        Some(encoded_by) => Some(values::parse_value(
            parser,
            ParseValueAssignmentStage::Normal,
            encoded_by,
            &ResolvedType::universal(TagType::ObjectIdentifier),
        )?),
        None => None,
    };
    Ok(ContentsConstraint {
        content_type: ty,
        encoded_by,
    })
}

fn parse_inner_type_constraints(
    parser: &AstParser<'_>,
    itc: &AstElement<AstInnerTypeConstraints>,
    constrained_type: &ResolvedType,
    parameters: &[(&String, &Parameter)],
) -> Result<InnerTypeConstraints> {
    Ok(match &itc.element {
        AstInnerTypeConstraints::SingleTypeConstraint(single) => {
            let component_type = match &constrained_type.ty {
                BuiltinType::StructureOf(of) => of.component_type.resolve(parser.context)?,
                other => {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "single inner type constraint cannot be applied to type {}",
                            other
                        )),
                        loc: itc.loc,
                    })
                }
            };

            InnerTypeConstraints::Single(parse_constraint(
                parser,
                &single.element.0,
                &component_type,
                parameters,
                ConstraintContext::Contextless,
            )?)
        }
        AstInnerTypeConstraints::MultipleTypeConstraints(multiple) => {
            let (is_choice, components): (bool, Vec<(AstElement<String>, Box<TaggedType>, bool)>) =
                match &constrained_type.ty {
                    BuiltinType::Structure(structure) => (
                        false,
                        structure
                            .resolve_components(parser.context)?
                            .into_iter()
                            .map(|component| {
                                (component.name, component.component_type, component.optional)
                            })
                            .collect(),
                    ),
                    BuiltinType::Choice(choice) => (
                        true,
                        choice
                            .resolve_alternatives(parser.context)?
                            .into_iter()
                            .map(|alternative| {
                                (alternative.name, alternative.alternative_type, false)
                            })
                            .collect(),
                    ),
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

            let (kind, ast_components) = match &multiple.element.0.element {
                AstTypeConstraintSpec::FullSpec(spec) => {
                    (MultipleTypeConstraintsKind::Full, &spec.element.0)
                }
                AstTypeConstraintSpec::PartialSpec(spec) => {
                    (MultipleTypeConstraintsKind::Partial, &spec.element.0)
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
                            let (component_type, is_optional) =
                                match components.iter().find(|component| {
                                    component.0.element == ast_component.element.name.element.0
                                }) {
                                    Some(component) => {
                                        (component.1.resolve(parser.context)?, component.2)
                                    }
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
                            let component_constraint = &ast_component.element.constraint.element;
                            let value = match &component_constraint.value {
                                Some(constraint) => Some(parse_constraint(
                                    parser,
                                    constraint,
                                    &component_type,
                                    parameters,
                                    ConstraintContext::Contextless,
                                )?),
                                None => None,
                            };
                            let presence = component_constraint.presence.as_ref().map(|presence| {
                                match &presence.element {
                                    AstPresenceConstraint::PresencePresent(_) => Presence::Present,
                                    AstPresenceConstraint::PresenceAbsent(_) => Presence::Absent,
                                    AstPresenceConstraint::PresenceOptional(_) => {
                                        Presence::Optional
                                    }
                                }
                            });
                            if !is_choice && presence.is_some() && !is_optional {
                                return Err(Error {
                                    kind: ErrorKind::Ast(format!(
                                    "component '{}' must be OPTIONAL to have a presence constraint",
                                    ast_component.element.name.element.0
                                )),
                                    loc: ast_component.loc,
                                });
                            }
                            ComponentConstraint { value, presence }
                        },
                    })
                })
                .collect::<Result<Vec<NamedConstraint>>>()?;

            InnerTypeConstraints::Multiple(MultipleTypeConstraints { kind, components })
        }
    })
}

fn parse_type_with_constraint(
    of: &AstElement<AstStructureOf>,
) -> Option<Vec<AstElement<AstConstraint>>> {
    of.element
        .constraint
        .as_ref()
        .map(|constraint| match &constraint.element {
            AstConstraintOrSizeConstraint::Constraint(constraint) => vec![constraint.clone()],
            AstConstraintOrSizeConstraint::SizeConstraint(size_constraint) => {
                let loc = size_constraint.loc;
                vec![AstElement::new(
                    AstConstraint(AstElement::new(
                        AstSubtypeConstraint {
                            element_sets: vec![AstElement::new(
                                AstConstraintElement::SubtypeElementSet(AstElement::new(
                                    AstSubtypeElementSet {
                                        elements: vec![AstElement::new(
                                            AstSubtypeElement::SizeConstraint(
                                                size_constraint.clone(),
                                            ),
                                            loc,
                                        )],
                                        operators: Vec::new(),
                                    },
                                    loc,
                                )),
                                loc,
                            )],
                        },
                        loc,
                    )),
                    loc,
                )]
            }
        })
}

fn parse_constrained_type(
    parser: &AstParser<'_>,
    ast_constrained_type: &AstElement<AstConstrainedType>,
    constrained_type: &ResolvedType,
    parameters: &[(&String, &Parameter)],
) -> Result<PendingConstraint> {
    let ast_constraints = match &ast_constrained_type.element {
        AstConstrainedType::Suffixed(suffixed) => {
            let ast_constraints = suffixed.element.constraints.clone();
            if ast_constraints.is_empty() {
                None
            } else {
                Some(ast_constraints)
            }
        }
        AstConstrainedType::TypeWithConstraint(twc) => parse_type_with_constraint(&twc.element.0),
    };
    let constraints = match ast_constraints {
        Some(ast_constraints) => Some(
            ast_constraints
                .into_iter()
                .map(|ast_constraint| {
                    parse_constraint(
                        parser,
                        &ast_constraint,
                        constrained_type,
                        parameters,
                        ConstraintContext::Contextless,
                    )
                })
                .collect::<Result<Vec<_>>>()?,
        ),
        None => match &ast_constrained_type.element {
            AstConstrainedType::Suffixed(suffixed) => match &suffixed.element.ty.element {
                AstUntaggedType::DefinedType(typeref) => {
                    if typeref.element.external_module.is_none() {
                        let param = parameters.iter().find_map(|(name, param)| match param {
                            Parameter::Type { tagged_type, .. } => {
                                if *name == &typeref.element.ty.element.0 {
                                    Some(tagged_type)
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        });
                        match param {
                            Some(param) => param.constraints.clone(),
                            None => None,
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            },
            AstConstrainedType::TypeWithConstraint(_) => None,
        },
    };
    let component_constraints = match &ast_constrained_type.element {
        AstConstrainedType::Suffixed(suffixed) => match &suffixed.element.ty.element {
            AstUntaggedType::BuiltinType(builtin) => match &builtin.element {
                AstBuiltinType::Structure(structure) => {
                    let resolved_components = match &constrained_type.ty {
                        BuiltinType::Structure(structure) => {
                            &structure.resolve_components(parser.context)?
                        }
                        _ => unreachable!(),
                    };

                    let components = types::flatten_structure_components(structure);
                    let mut component_constraints = Vec::with_capacity(components.len());
                    for component in components {
                        let resolved_component = resolved_components
                            .iter()
                            .find(|resolved_component| {
                                resolved_component.name.element == component.element.name.element.0
                            })
                            .expect("resolved type missing component from ast type");
                        let component_type = match &resolved_component.component_type.ty {
                            UntaggedType::ObjectClassField(_) => {
                                ResolvedType::universal(TagType::Any)
                            }
                            _ => resolved_component.component_type.resolve(parser.context)?,
                        };
                        if let Some(typeref) =
                            types::ast_type_as_parameterized_type_reference(&component.element.ty)
                        {
                            let (type_assignment, parameters) =
                                resolve_type_assignment_from_parameterized_type_reference(
                                    parser,
                                    typeref,
                                    parameters.to_vec(),
                                )?;
                            let pending = parse_type_assignment_constraint_with_resolved_type(
                                parser,
                                type_assignment,
                                &component_type,
                                parameters
                                    .iter()
                                    .map(|(name, param)| (name, param))
                                    .collect(),
                            )?;

                            component_constraints
                                .push((resolved_component.name.element.clone(), pending));
                        } else {
                            component_constraints.push((
                                resolved_component.name.element.clone(),
                                parse_type_constraint(
                                    parser,
                                    &component.element.ty,
                                    &component_type,
                                    parameters,
                                )?,
                            ));
                        }
                    }
                    component_constraints
                }
                AstBuiltinType::Choice(choice) => {
                    let resolved_alternatives = match &constrained_type.ty {
                        BuiltinType::Choice(choice) => &choice.resolve_alternatives(parser.context)?,
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
                        let component_type = match &resolved_alternative.alternative_type.ty {
                            UntaggedType::ObjectClassField(_) => {
                                ResolvedType::universal(TagType::Any)
                            }
                            _ => resolved_alternative
                                .alternative_type
                                .resolve(parser.context)?,
                        };
                        alternative_constraints.push((
                            resolved_alternative.name.element.clone(),
                            parse_type_constraint(
                                parser,
                                &alternative.element.ty,
                                &component_type,
                                parameters,
                            )?,
                        ));
                    }
                    alternative_constraints
                }
                _ => {
                    return Ok(PendingConstraint {
                        constraints,
                        component_constraints: Vec::new(),
                    })
                }
            },
            _ => Vec::new(),
        },
        AstConstrainedType::TypeWithConstraint(twc) => {
            let component_type = match &constrained_type.ty {
                BuiltinType::StructureOf(of) => of.component_type.resolve(parser.context)?,
                _ => unreachable!(),
            };

            let component_constraint = parse_type_constraint(
                parser,
                &twc.element.0.element.ty,
                &component_type,
                parameters,
            )?;

            vec![(String::new(), component_constraint)]
        }
    };
    Ok(PendingConstraint {
        constraints,
        component_constraints,
    })
}

pub(crate) fn parse_type_constraint(
    parser: &AstParser<'_>,
    ty: &AstElement<AstType>,
    constrained_type: &ResolvedType,
    parameters: &[(&String, &Parameter)],
) -> Result<PendingConstraint> {
    Ok(match &ty.element {
        AstType::TaggedType(tagged_type) => parse_type_constraint(
            parser,
            &tagged_type.element.ty,
            constrained_type,
            parameters,
        )?,
        AstType::ConstrainedType(constrained) => {
            parse_constrained_type(parser, constrained, constrained_type, parameters)?
        }
    })
}

pub(crate) fn resolve_type_assignment_from_parameterized_type_reference<'a>(
    parser: &'a AstParser<'_>,
    typeref: &AstElement<AstParameterizedDefinedType>,
    parameters: Vec<(&String, &Parameter)>,
) -> Result<(&'a AstElement<AstTypeAssignment>, Vec<(String, Parameter)>)> {
    let (ast, parameters) = types::resolve_parameterized_type_reference(
        parser,
        typeref,
        &parameters
            .iter()
            .map(|(name, param)| (*name, *param))
            .collect::<Vec<_>>(),
    )?;

    let names = ast
        .element
        .parameters
        .as_ref()
        .expect("resolved a non-parameterized type assignment AST")
        .element
        .0
        .iter()
        .map(|parameter| match &parameter.element {
            AstParameterDecl::TypeParameterDecl(ast) => ast.element.0.element.0.clone(),
            AstParameterDecl::ValueParameterDecl(ast) => ast.element.name.element.0.clone(),
            AstParameterDecl::ObjectSetParameterDecl(ast) => ast.element.name.element.0.clone(),
        })
        .collect::<Vec<String>>();
    let named_parameters = names.iter().zip(&parameters).collect::<Vec<_>>();
    Ok((
        ast,
        named_parameters
            .iter()
            .map(|(name, param)| ((*name).clone(), (*param).clone()))
            .collect(),
    ))
}

fn resolve_type_assignment<'a>(
    parser: &'a AstParser<'_>,
    type_assignment: &'a AstElement<AstTypeAssignment>,
    parameters: Vec<(&String, &Parameter)>,
) -> Result<(&'a AstElement<AstTypeAssignment>, Vec<(String, Parameter)>)> {
    let ast_type = match &type_assignment.element.subject.element {
        AstTypeAssignmentSubject::Type(ast_type) => ast_type,
        _ => panic!("parameterized type resolves to a CLASS"),
    };

    let untagged = {
        let mut ast_type = ast_type;
        loop {
            match &ast_type.element {
                AstType::TaggedType(tagged_type) => {
                    ast_type = &tagged_type.element.ty;
                }
                AstType::ConstrainedType(constrained_type) => match &constrained_type.element {
                    AstConstrainedType::Suffixed(suffixed) => {
                        break &suffixed.element.ty;
                    }
                    AstConstrainedType::TypeWithConstraint(_) => {
                        return Ok((
                            type_assignment,
                            parameters
                                .iter()
                                .map(|(name, param)| ((*name).clone(), (*param).clone()))
                                .collect(),
                        ));
                    }
                },
            }
        }
    };

    Ok(match &untagged.element {
        AstUntaggedType::ParameterizedDefinedType(typeref) => {
            // recursively resolve the AST until we reach the root type
            // e.g:
            //
            // ```
            // A{K, V} ::= SEQUENCE { k K, v V }
            // B{K} ::= A{K, INTEGER}
            // C ::= B{UTF8String}
            // ```
            //
            // resolve_type_assignment(C) -> (
            //   A{K, V} ::= SEQUENCE { k K, v V },
            //   [("K", UTF8String), ("V", INTEGER)]
            // )
            let (ast, named_parameters) =
                resolve_type_assignment_from_parameterized_type_reference(
                    parser, typeref, parameters,
                )?;
            resolve_type_assignment(
                parser,
                ast,
                named_parameters
                    .iter()
                    .map(|(name, parameter)| (name, parameter))
                    .collect(),
            )?
        }
        _ => (
            type_assignment,
            parameters
                .iter()
                .map(|(name, param)| ((*name).clone(), (*param).clone()))
                .collect(),
        ),
    })
}

#[derive(Debug)]
pub struct PendingConstraint {
    pub constraints: Option<Vec<Constraint>>,
    pub component_constraints: Vec<(String, PendingConstraint)>,
}

pub(crate) fn parse_type_assignment_constraint_with_resolved_type(
    parser: &AstParser<'_>,
    type_assignment: &AstElement<AstTypeAssignment>,
    constrained_type: &ResolvedType,
    parameters: Vec<(&String, &Parameter)>,
) -> Result<PendingConstraint> {
    let (type_assignment, mut params) =
        resolve_type_assignment(parser, type_assignment, parameters)?;

    for (_, param) in &mut params {
        if let Parameter::Type { ast, tagged_type } = param {
            let constrained_type = tagged_type.resolve(parser.context)?;
            let constraint = parse_type_constraint(parser, ast, &constrained_type, &[])?;
            apply_pending_constraint(parser.context, tagged_type, constraint)?;
        }
    }

    let pending = parse_type_constraint(
        parser,
        match &type_assignment.element.subject.element {
            AstTypeAssignmentSubject::Type(ast_type) => ast_type,
            _ => panic!("parameterized type resolves to a CLASS"),
        },
        constrained_type,
        &params
            .iter()
            .map(|(name, param)| (name, param))
            .collect::<Vec<_>>(),
    )?;
    Ok(pending)
}

pub fn parse_type_assignment_constraint(
    parser: &AstParser<'_>,
    type_assignment: &AstElement<AstTypeAssignment>,
) -> Result<Option<(QualifiedIdentifier, PendingConstraint)>> {
    match &type_assignment.element.subject.element {
        AstTypeAssignmentSubject::Type(_) => {
            let name = type_assignment.element.name.element.0.clone();
            let parameter_names = types::parse_type_assignment_parameters(parser, type_assignment)?;
            if parameter_names
                .map(|parameters| parameters.len())
                .unwrap_or(0)
                > 0
            {
                Ok(None)
            } else {
                let ident = QualifiedIdentifier::new(parser.module.clone(), name);
                let constrained_type = parser.context.lookup_type(&ident).expect("lookup_type");
                let constrained_type = constrained_type.ty.resolve(parser.context)?;
                let pending = parse_type_assignment_constraint_with_resolved_type(
                    parser,
                    type_assignment,
                    &constrained_type,
                    Vec::new(),
                )?;

                Ok(Some((ident, pending)))
            }
        }
        _ => Ok(None),
    }
}

pub fn apply_pending_constraint(
    context: &Context,
    tagged_type: &mut TaggedType,
    pending: PendingConstraint,
) -> Result<()> {
    if let Some(constraints) = pending.constraints {
        tagged_type.constraints = Some(constraints);
    }
    if !pending.component_constraints.is_empty() {
        match &mut tagged_type.ty {
            UntaggedType::BuiltinType(builtin) => match builtin {
                BuiltinType::Structure(structure) => {
                    let mut components = structure.resolve_components(context)?;
                    for (component_name, pending) in pending.component_constraints {
                        let component = components
                            .iter_mut()
                            .find(|component| component.name.element == component_name)
                            .expect("pending constraint component not found in type");
                        apply_pending_constraint(context, &mut component.component_type, pending)?;
                    }
                    structure.set_resolved_components(components);
                }
                BuiltinType::StructureOf(of) => {
                    if pending.component_constraints.len() != 1 {
                        panic!(
                            "expecting 1 component constraint, found {}",
                            pending.component_constraints.len()
                        );
                    }
                    let (_, component_constraint) =
                        pending.component_constraints.into_iter().next().unwrap();
                    apply_pending_constraint(
                        context,
                        &mut of.component_type,
                        component_constraint,
                    )?;
                }
                BuiltinType::Choice(choice) => {
                    for (alternative_name, pending) in pending.component_constraints {
                        let alternative = choice
                            .alternatives
                            .iter_mut()
                            .find(|alternative| alternative.name.element == alternative_name)
                            .expect("pending constraint alternative not found in type");
                        apply_pending_constraint(
                            context,
                            &mut alternative.alternative_type,
                            pending,
                        )?;
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!("typereference aliases can't have constrained components"),
        }
    }

    Ok(())
}
