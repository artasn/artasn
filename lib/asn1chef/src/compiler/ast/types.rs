use super::{values, AstParser};
use crate::{compiler::parser::*, module::*, types::*, values::*};

fn parse_constant(
    parser: &AstParser<'_>,
    ast_constant: &AstElement<AstConstant>,
    kind: Option<ConstraintsKind>,
) -> Result<AstElement<Value>> {
    Ok(AstElement::new(
        match &ast_constant.element {
            AstConstant::IntegerValue(num) => {
                if kind
                    .map(|kind| kind == ConstraintsKind::Size)
                    .unwrap_or(false)
                {
                    if let Some(sign) = &num.element.sign {
                        return Err(Error {
                            kind: ErrorKind::Ast(
                                "size constraint must be a non-negative integer".to_owned(),
                            ),
                            loc: sign.loc,
                        });
                    }
                }
                Value::BuiltinValue(values::parse_integer_value(num)?)
            }
            AstConstant::DefinedValue(value) => {
                values::parse_valuereference(parser, &value.element.value).element
            }
        },
        ast_constant.loc,
    ))
}

fn parse_constraints(
    parser: &AstParser<'_>,
    kind: ConstraintsKind,
    ast_constraints: &AstElement<AstConstraints>,
) -> Result<Constraints> {
    let mut constraints = Vec::new();

    for ast_constraint in &ast_constraints.element.constraints {
        match &ast_constraint.element {
            AstConstraint::ConstantSeries(ast_series) => {
                let mut series = Vec::new();
                for ast_constant in &ast_series.element.0 {
                    series.push(parse_constant(parser, ast_constant, Some(kind))?);
                }
                constraints.push(Constraint::ConstantSeries(series));
            }
            AstConstraint::Extensible(_) => {
                // TODO: verify that there is an Extensible between each definition
            }
            AstConstraint::Range(ast_range) => {
                constraints.push(Constraint::Range(Range {
                    lower: match &ast_range.element.lower.element {
                        AstRangeLowerBound::Constant(ast_constant) => RangeLowerBound::Constant(
                            parse_constant(parser, ast_constant, Some(kind))?,
                        ),
                        AstRangeLowerBound::GtConstant(ast_constant) => {
                            RangeLowerBound::GtConstant(parse_constant(
                                parser,
                                &ast_constant.element.0,
                                Some(kind),
                            )?)
                        }
                        AstRangeLowerBound::Min(_) => RangeLowerBound::Min,
                    },
                    upper: match &ast_range.element.upper.element {
                        AstRangeUpperBound::Constant(ast_constant) => RangeUpperBound::Constant(
                            parse_constant(parser, ast_constant, Some(kind))?,
                        ),
                        AstRangeUpperBound::LtConstant(ast_constant) => {
                            RangeUpperBound::LtConstant(parse_constant(
                                parser,
                                &ast_constant.element.0,
                                Some(kind),
                            )?)
                        }
                        AstRangeUpperBound::Max(_) => RangeUpperBound::Max,
                    },
                }));
            }
        }
    }

    Ok(Constraints {
        kind,
        components: constraints,
    })
}

fn parse_structure_components(
    parser: &AstParser<'_>,
    components: &[AstElement<AstStructureComponent>],
) -> Result<Vec<StructureComponent>> {
    let has_tags = components
        .iter()
        .any(|component| match &component.element.ty.element {
            AstType::TaggedType(_) => true,
            AstType::UntaggedType(_) => false,
        });
    components
        .iter()
        .enumerate()
        .map(|(i, component)| {
            let ty = parse_type(
                parser,
                &component.element.ty,
                TypeContext::StructureComponent {
                    index: i as u16,
                    has_tags,
                },
            )?;

            Ok(StructureComponent {
                name: component.element.name.as_ref().map(|name| name.0.clone()),
                default_value: match component.element.default.as_ref().map(|default| {
                    // TODO: deferred execution of resolve() until after stage 2 finishes
                    let resolved_ty = ty.resolve(parser.context)?;

                    Ok(Box::new(values::parse_value(
                        parser,
                        default,
                        &resolved_ty,
                    )?))
                }) {
                    Some(result) => Some(result?),
                    None => None,
                },
                optional: component.element.optional.is_some(),
                component_type: Box::new(ty),
            })
        })
        .collect::<Result<Vec<StructureComponent>>>()
}

fn parse_size_constraints(
    parser: &AstParser<'_>,
    size_constraints: Option<&AstElement<AstSizeConstraints>>,
) -> Result<Option<Constraints>> {
    Ok(match size_constraints {
        Some(sc) => Some(parse_constraints(
            parser,
            ConstraintsKind::Size,
            &sc.element.0,
        )?),
        None => None,
    })
}

fn parse_structure_type(
    parser: &AstParser<'_>,
    tag_type: TagType,
    structure: &AstElement<AstStructureKind>,
) -> Result<BuiltinType> {
    Ok(match &structure.element {
        AstStructureKind::SingleStructure(structure) => BuiltinType::Structure(Structure {
            ty: tag_type,
            components: parse_structure_components(parser, &structure.element.components)?,
        }),
        AstStructureKind::StructureOf(structure_of) => BuiltinType::StructureOf(StructureOf {
            ty: tag_type,
            size_constraints: parse_size_constraints(
                parser,
                structure_of.element.size_constraints.as_ref(),
            )?,
            component_type: Box::new(parse_type(
                parser,
                &structure_of.element.ty,
                TypeContext::Contextless,
            )?),
        }),
    })
}

fn parse_choice_type(
    parser: &AstParser<'_>,
    alternatives: &[AstElement<AstChoiceAlternative>],
) -> Result<BuiltinType> {
    let has_tags = alternatives
        .iter()
        .any(|alternative| match &alternative.element.ty.element {
            AstType::TaggedType(_) => true,
            AstType::UntaggedType(_) => false,
        });
    Ok(BuiltinType::Choice(Choice {
        alternatives: alternatives
            .iter()
            .enumerate()
            .map(|(i, alternative)| {
                let ty = parse_type(
                    parser,
                    &alternative.element.ty,
                    TypeContext::StructureComponent {
                        index: i as u16,
                        has_tags,
                    },
                )?;

                Ok(ChoiceAlternative {
                    name: alternative.element.name.as_ref().map(|name| name.0.clone()),
                    ty: Box::new(ty),
                })
            })
            .collect::<Result<Vec<ChoiceAlternative>>>()?,
    }))
}

fn parse_enumerated_type(
    parser: &AstParser<'_>,
    enumerated: &AstElement<AstEnumerated>,
) -> Result<BuiltinType> {
    let mut items = Vec::new();
    let mut implied_index = 0;
    for ast_item in &enumerated.element.0 {
        let value = match &ast_item.element.num {
            Some(num) => EnumerationItemValue::Specified(parse_constant(parser, num, None)?),
            None => {
                let value = EnumerationItemValue::Implied(implied_index);
                implied_index += 1;
                value
            }
        };
        items.push(EnumerationItem {
            name: ast_item.element.name.as_ref().map(|name| name.0.clone()),
            value,
        });
    }
    Ok(BuiltinType::Enumerated(items))
}

fn parse_builtin_type(
    parser: &AstParser<'_>,
    builtin: &AstElement<AstBuiltinType>,
) -> Result<BuiltinType> {
    Ok(match &builtin.element {
        AstBuiltinType::Boolean(_) => BuiltinType::Boolean,
        AstBuiltinType::Integer(integer) => BuiltinType::Integer(IntegerType {
            named_values: None, // TODO
            value_constraints: match &integer.element.value_constraints {
                Some(sc) => Some(parse_constraints(parser, ConstraintsKind::Value, sc)?),
                None => None,
            },
        }),
        AstBuiltinType::BitString(bit_string) => BuiltinType::BitString(BitStringType {
            named_bits: None, // TODO
            size_constraints: parse_size_constraints(
                parser,
                bit_string
                    .element
                    .size_constraints
                    .as_ref()
                    .map(|sc| &sc.element.0),
            )?,
        }),
        AstBuiltinType::OctetString(octet_string) => BuiltinType::OctetString(OctetStringType {
            size_constraints: parse_size_constraints(
                parser,
                octet_string
                    .element
                    .size_constraints
                    .as_ref()
                    .map(|sc| &sc.element.0),
            )?,
        }),
        AstBuiltinType::Null(_) => BuiltinType::Null,
        AstBuiltinType::ObjectIdentifier(_) => BuiltinType::ObjectIdentifier,
        AstBuiltinType::ObjectDescriptor(_) => {
            BuiltinType::CharacterString(TagType::ObjectDescriptor)
        }
        AstBuiltinType::External(_) => todo!("External"),
        AstBuiltinType::Real(_) => todo!("Real"),
        AstBuiltinType::Enumerated(enumerated) => parse_enumerated_type(parser, enumerated)?,
        AstBuiltinType::EmbeddedPDV(_) => todo!("EmbeddedPDV"),
        AstBuiltinType::UTF8String(_) => BuiltinType::CharacterString(TagType::UTF8String),
        AstBuiltinType::RelativeOid(_) => todo!("RelativeOid"),
        AstBuiltinType::Time(_) => BuiltinType::Time,
        AstBuiltinType::Sequence(sequence) => {
            parse_structure_type(parser, TagType::Sequence, &sequence.element.0)?
        }
        AstBuiltinType::Set(sequence) => {
            parse_structure_type(parser, TagType::Set, &sequence.element.0)?
        }
        AstBuiltinType::Choice(choice) => parse_choice_type(parser, &choice.element.0)?,
        AstBuiltinType::NumericString(_) => BuiltinType::CharacterString(TagType::NumericString),
        AstBuiltinType::PrintableString(_) => {
            BuiltinType::CharacterString(TagType::PrintableString)
        }
        AstBuiltinType::TeletexString(_) => BuiltinType::CharacterString(TagType::TeletexString),
        AstBuiltinType::VideotexString(_) => BuiltinType::CharacterString(TagType::VideotexString),
        AstBuiltinType::IA5String(_) => BuiltinType::CharacterString(TagType::IA5String),
        AstBuiltinType::UTCTime(_) => BuiltinType::UTCTime,
        AstBuiltinType::GeneralizedTime(_) => todo!("GeneralizedTime"),
        AstBuiltinType::GraphicString(_) => BuiltinType::CharacterString(TagType::GraphicString),
        AstBuiltinType::VisibleString(_) => BuiltinType::CharacterString(TagType::VisibleString),
        AstBuiltinType::GeneralString(_) => BuiltinType::CharacterString(TagType::GeneralString),
        AstBuiltinType::UniversalString(_) => {
            BuiltinType::CharacterString(TagType::UniversalString)
        }
        AstBuiltinType::CharacterString(_) => {
            BuiltinType::CharacterString(TagType::CharacterString)
        }
        AstBuiltinType::BMPString(_) => BuiltinType::CharacterString(TagType::BMPString),
        AstBuiltinType::Date(_) => BuiltinType::Date,
        AstBuiltinType::TimeOfDay(_) => BuiltinType::TimeOfDay,
        AstBuiltinType::DateTime(_) => BuiltinType::DateTime,
        AstBuiltinType::Duration(_) => BuiltinType::Duration,
    })
}

fn parse_untagged_type(
    parser: &AstParser<'_>,
    ty: &AstElement<AstUntaggedType>,
) -> Result<UntaggedType> {
    Ok(match ty.element {
        AstUntaggedType::BuiltinType(ref builtin) => {
            UntaggedType::BuiltinType(parse_builtin_type(parser, builtin)?)
        }
        AstUntaggedType::TypeReference(ref typeref) => UntaggedType::Reference(AstElement::new(
            parser
                .context
                .lookup_module(&parser.module)
                .expect("lookup_module")
                .resolve_symbol(&typeref.element.0),
            typeref.loc,
        )),
    })
}

pub enum TypeContext {
    Contextless,
    StructureComponent { index: u16, has_tags: bool },
}

// ASN.1 Tagging Rules
//
// Any TagDefault + [CLASS N] IMPLICIT = encoded as [CLASS N]
// Any TagDefault + [CLASS N] EXPLICIT = encoded as [CLASS N]+Structured, [UNIVERSAL T]
// TagDefault + [CLASS N] = [CLASS N] TagDefault
// Any TagDefault + Untagged Type = encoded as [UNIVERSAL T]
//
// Exception to these rules:
//     1. when TagDefault = AUTOMATIC, and
//     2. there are NO tags for any of the types in a structure,
//     then the tags for the component types are [CONTEXT-SPECIFIC ComponentIndex]
pub fn parse_type(
    parser: &AstParser<'_>,
    ty: &AstElement<AstType>,
    type_context: TypeContext,
) -> Result<TaggedType> {
    let tag_default = parser
        .context
        .lookup_module(&parser.module)
        .expect("lookup_module")
        .tag_default;
    Ok(match &ty.element {
        AstType::TaggedType(tagged_type) => {
            let ty = parse_untagged_type(parser, &tagged_type.element.ty)?;
            let tag = &tagged_type.element.tag;
            let class = match tag.element.class {
                Some(ref class) => match class.element {
                    AstClass::Universal(_) => Class::Universal,
                    AstClass::Application(_) => Class::Application,
                    AstClass::Private(_) => Class::Private,
                },
                None => Class::ContextSpecific,
            };
            let class_number = &tag.element.class_number;
            if class_number.element.0 > Tag::MAX_TAG as u64 {
                return Err(Error {
                    kind: ErrorKind::Ast(format!("tag number must not exceed {}", Tag::MAX_TAG)),
                    loc: class_number.loc,
                });
            }
            let tag_number = class_number.element.0 as u16;
            let (mut kind, source) = match &tagged_type.element.kind {
                Some(kind) => (
                    match kind.element {
                        AstTagKind::TagKindExplicit(_) => TagKind::Explicit,
                        AstTagKind::TagKindImplicit(_) => TagKind::Implicit,
                    },
                    TagSource::KindSpecified,
                ),
                None => (
                    match tag_default {
                        // AUTOMATIC does not always mean IMPLICIT
                        // see checks below
                        TagDefault::Automatic | TagDefault::Implicit => TagKind::Implicit,
                        TagDefault::Explicit => TagKind::Explicit,
                    },
                    TagSource::KindImplied,
                ),
            };

            if let UntaggedType::BuiltinType(BuiltinType::Choice(_)) = ty {
                if kind == TagKind::Implicit {
                    match source {
                        TagSource::KindSpecified => {
                            return Err(Error {
                                kind: ErrorKind::Ast(
                                    "CHOICE is not permitted to have IMPLICIT tagging"
                                        .to_string(),
                                ),
                                loc: tagged_type.element.kind.as_ref().unwrap().loc,
                            })
                        }
                        _ => {
                            kind = TagKind::Explicit;
                        }
                    }
                }
            }

            let tag = Tag::new(class, tag_number, kind, source);
            TaggedType { tag: Some(tag), ty }
        }
        AstType::UntaggedType(untagged) => {
            let ty = parse_untagged_type(parser, untagged)?;
            let index = match type_context {
                TypeContext::StructureComponent { index, has_tags } => match has_tags {
                    true => None,
                    false => Some(index),
                },
                TypeContext::Contextless => None,
            };
            let mut tag = match (tag_default, index) {
                (TagDefault::Automatic, Some(index)) => {
                    // Automatic is not always Implicit, there are special cases where it means Explicit
                    // see below for how this is done with CHOICE
                    Some(Tag::new(
                        Class::ContextSpecific,
                        index,
                        TagKind::Implicit,
                        TagSource::TagImplied,
                    ))
                }
                _ => match &ty {
                    UntaggedType::BuiltinType(builtin) => {
                        builtin.tag_type().map(Tag::universal)
                    }
                    UntaggedType::Reference(_) => None,
                },
            };
            if let Some(tag) = &mut tag {
                if let UntaggedType::BuiltinType(BuiltinType::Choice(_)) = ty {
                    tag.kind = TagKind::Explicit;
                }
            }
            TaggedType { tag, ty }
        }
    })
}

pub fn parse_type_assignment(
    parser: &AstParser<'_>,
    type_assignment: &AstElement<AstTypeAssignment>,
) -> Result<(QualifiedIdentifier, TaggedType)> {
    let name = type_assignment.element.name.element.0.clone();
    let ty = parse_type(
        parser,
        &type_assignment.element.ty,
        TypeContext::Contextless,
    )?;

    Ok((
        QualifiedIdentifier {
            module: parser.module.clone(),
            name,
        },
        ty,
    ))
}
