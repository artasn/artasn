use super::{
    class,
    util::LazyParse,
    values::{self, ParseValueAssignmentStage},
    AstParser,
};
use crate::{
    compiler::{context::DeclaredType, parser::*},
    module::*,
    types::*,
    values::{BuiltinValue, TypedValue, ValueReference},
};

lazy_static::lazy_static! {
    static ref REAL_IDENT: QualifiedIdentifier = QualifiedIdentifier::new(
        ModuleIdentifier::with_name(String::from("Real")),
        String::from("Real"),
    );
    static ref EXTERNAL_IDENT: QualifiedIdentifier = QualifiedIdentifier::new(
        ModuleIdentifier::with_name(String::from("External")),
        String::from("External"),
    );
    static ref EMBEDDED_PDV_IDENT: QualifiedIdentifier = QualifiedIdentifier::new(
        ModuleIdentifier::with_name(String::from("EmbeddedPDV")),
        String::from("EmbeddedPDV"),
    );
    static ref CHARACTER_STRING_IDENT: QualifiedIdentifier = QualifiedIdentifier::new(
        ModuleIdentifier::with_name(String::from("CharacterString")),
        String::from("CharacterString"),
    );
    static ref INSTANCE_OF_IDENT: QualifiedIdentifier = QualifiedIdentifier::new(
        ModuleIdentifier::with_name(String::from("InformationObjectClasses")),
        String::from("InstanceOf"),
    );
}

pub type LazyParsedDefaultValue =
    LazyParse<ResolvedType, AstElement<AstValue>, Result<AstElement<TypedValue>>>;

fn default_value_lazy_parser(
    parser: &AstParser<'_>,
    resolved_type: &ResolvedType,
    default: &AstElement<AstValue>,
) -> Result<AstElement<TypedValue>> {
    values::parse_value(
        parser,
        ParseValueAssignmentStage::Normal,
        default,
        resolved_type,
    )
}

fn parse_structure_components(
    parser: &AstParser<'_>,
    components: &[&AstElement<AstStructureComponent>],
    parameters: &[(&String, &Parameter)],
) -> Result<(Vec<StructureComponent>, bool)> {
    let has_tags = components.iter().any(|component| match &component.element {
        AstStructureComponent::NamedStructureComponent(named) => {
            matches!(&named.element.ty.element, AstType::TaggedType(_))
        }
        AstStructureComponent::ComponentsOf(_) => false,
    });
    let components = components
        .iter()
        .map(|component| match &component.element {
            AstStructureComponent::NamedStructureComponent(component) => {
                let forbidden_ident = match component.element.name.element.0.as_str() {
                    "artasn-special" if parser.module != REAL_IDENT.module => {
                        Some("artasn-special")
                    }
                    "artasn-external" if parser.module != EXTERNAL_IDENT.module => {
                        Some("artasn-external")
                    }
                    _ => None,
                };
                if let Some(forbidden_ident) = forbidden_ident {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "{} is a forbidden identifier",
                            forbidden_ident
                        )),
                        loc: component.element.name.loc,
                    });
                }

                let ty = parse_type(parser, &component.element.ty, parameters)?;

                Ok(StructureComponent::Named(NamedStructureComponent {
                    name: component.element.name.as_ref().map(|name| name.0.clone()),
                    default_value: component.element.default.as_ref().map(|default| {
                        if let AstValue::DefinedValue(valref) = &default.element {
                            if valref.element.external_module.is_none() {
                                let val_param =
                                    parameters.iter().find_map(
                                        |(name, parameter)| match parameter {
                                            Parameter::Value { value_type, value } => {
                                                if *name == &valref.element.value.element.0 {
                                                    Some((value_type, value))
                                                } else {
                                                    None
                                                }
                                            }
                                            _ => None,
                                        },
                                    );
                                if let Some((_, value)) = val_param {
                                    return LazyParsedDefaultValue::precomputed(Ok(value.clone()));
                                }
                            }
                        }

                        LazyParsedDefaultValue::new(default.clone(), default_value_lazy_parser)
                    }),
                    optional: component.element.optional,
                    component_type: Box::new(ty),
                }))
            }
            AstStructureComponent::ComponentsOf(of) => {
                Ok(StructureComponent::ComponentsOf(AstElement::new(
                    Box::new(parse_type(parser, &of.element.0, parameters)?),
                    of.element.0.loc,
                )))
            }
        })
        .collect::<Result<Vec<StructureComponent>>>()?;
    Ok((components, has_tags))
}

// TODO: in the future, versioning should matter; but for now, pretend like the versioning does not exist
pub(crate) fn flatten_structure_components(
    structure: &AstElement<AstStructure>,
) -> Vec<&AstElement<AstStructureComponent>> {
    structure
        .element
        .component_groups
        .iter()
        .filter_map(|component_group| match &component_group.element {
            AstStructureComponentGroup::Extensible(_) => None,
            AstStructureComponentGroup::StructureComponent(component) => Some(vec![component]),
            AstStructureComponentGroup::ComponentExtensionGroup(group) => {
                Some(group.element.components.iter().collect())
            }
        })
        .flatten()
        .collect()
}

fn parse_structure_type(
    parser: &AstParser<'_>,
    structure: &AstElement<AstStructure>,
    parameters: &[(&String, &Parameter)],
) -> Result<BuiltinType> {
    let components = flatten_structure_components(structure);
    let (components, has_tags) = parse_structure_components(parser, &components, parameters)?;
    let tag_default = parser
        .context
        .lookup_module(&parser.module)
        .expect("lookup_module")
        .tag_default;
    let ty = match structure.element.kind.element {
        AstStructureKind::Sequence(_) => TagType::Sequence,
        AstStructureKind::Set(_) => TagType::Set,
    };
    let automatic_tagging = tag_default == TagDefault::Automatic && !has_tags;
    Ok(BuiltinType::Structure(Structure::new(
        ty,
        automatic_tagging,
        components,
    )))
}

fn parse_structure_of_type(
    parser: &AstParser<'_>,
    of: &AstElement<AstStructureOf>,
    parameters: &[(&String, &Parameter)],
) -> Result<UntaggedType> {
    Ok(UntaggedType::BuiltinType(BuiltinType::StructureOf(
        StructureOf {
            ty: match of.element.kind.element {
                AstStructureKind::Sequence(_) => TagType::Sequence,
                AstStructureKind::Set(_) => TagType::Set,
            },
            component_type: Box::new(parse_type(parser, &of.element.ty, parameters)?),
        },
    )))
}

fn parse_choice_type(
    parser: &AstParser<'_>,
    alternatives: &[AstElement<AstChoiceAlternative>],
    parameters: &[(&String, &Parameter)],
) -> Result<BuiltinType> {
    let has_tags = alternatives
        .iter()
        .any(|alternative| match &alternative.element {
            AstChoiceAlternative::NamedChoiceAlternative(alternative) => {
                matches!(&alternative.element.ty.element, AstType::TaggedType(_))
            }
            AstChoiceAlternative::Extensible(_) => false,
        });
    let tag_default = parser
        .context
        .lookup_module(&parser.module)
        .expect("lookup_module")
        .tag_default;
    Ok(BuiltinType::Choice(Choice {
        alternatives: alternatives
            .iter()
            .filter_map(|alternative| match &alternative.element {
                AstChoiceAlternative::NamedChoiceAlternative(alternative) => Some(alternative),
                AstChoiceAlternative::Extensible(_) => None,
            })
            .map(|alternative| {
                let ty = parse_type(parser, &alternative.element.ty, parameters)?;

                Ok(ChoiceAlternative {
                    name: alternative.element.name.as_ref().map(|name| name.0.clone()),
                    alternative_type: Box::new(ty),
                })
            })
            .collect::<Result<Vec<ChoiceAlternative>>>()?,
        automatic_tagging: tag_default == TagDefault::Automatic && !has_tags,
    }))
}

fn parse_enumerated_type(
    parser: &AstParser<'_>,
    enumerated: &AstElement<AstEnumerated>,
) -> Result<BuiltinType> {
    let mut items = Vec::new();
    let mut implied_index = 0;
    for ast_item in &enumerated.element.0 {
        if let AstEnumerationItem::NamedEnumerationItem(ast_item) = &ast_item.element {
            let value = match &ast_item.element.num {
                Some(num) => EnumerationItemValue::Specified(values::parse_value(
                    parser,
                    ParseValueAssignmentStage::Normal,
                    &num.element.0,
                    &ResolvedType {
                        tag: Some(Tag::universal(TagType::Enumerated)),
                        ty: BuiltinType::Enumerated(Vec::new()),
                        constraints: None,
                    },
                )?),
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
    }
    Ok(BuiltinType::Enumerated(items))
}

fn parse_named_numbers(
    parser: &AstParser<'_>,
    ast_named_numbers: &AstElement<AstNamedNumberList>,
) -> Result<Vec<NamedNumber>> {
    let mut named_numbers = Vec::with_capacity(ast_named_numbers.element.0.len());

    for ast_named_number in &ast_named_numbers.element.0 {
        let name = ast_named_number
            .element
            .name
            .as_ref()
            .map(|name| name.0.clone());
        let value = match &ast_named_number.element.num.element {
            AstIntegerValueReference::IntegerValue(int) => AstElement::new(
                TypedValue {
                    resolved_type: ResolvedType::universal(TagType::Integer),
                    value: ValueReference::BuiltinValue(BuiltinValue::Integer(
                        values::parse_integer_value(int)?,
                    )),
                },
                int.loc,
            ),
            AstIntegerValueReference::DefinedValue(valref) => {
                values::resolve_defined_value(parser, valref)?.map(|valref| TypedValue {
                    resolved_type: ResolvedType::universal(TagType::Integer),
                    value: ValueReference::Reference(valref),
                })
            }
        };
        named_numbers.push(NamedNumber { name, value });
    }

    Ok(named_numbers)
}

fn parse_builtin_type(
    parser: &AstParser<'_>,
    builtin: &AstElement<AstBuiltinType>,
    parameters: &[(&String, &Parameter)],
) -> Result<UntaggedType> {
    Ok(UntaggedType::BuiltinType(match &builtin.element {
        AstBuiltinType::Any(_) => BuiltinType::Any,
        AstBuiltinType::Boolean(_) => BuiltinType::Boolean,
        AstBuiltinType::Integer(integer) => BuiltinType::Integer(IntegerType {
            named_values: match &integer.element.named_values {
                Some(named_values) => Some(parse_named_numbers(parser, named_values)?),
                None => None,
            },
        }),
        AstBuiltinType::BitString(bit_string) => BuiltinType::BitString(BitStringType {
            named_bits: match &bit_string.element.named_bits {
                Some(named_bits) => Some(parse_named_numbers(parser, named_bits)?),
                None => None,
            },
        }),
        AstBuiltinType::OctetString(_) => BuiltinType::OctetString,
        AstBuiltinType::Null(_) => BuiltinType::Null,
        AstBuiltinType::ObjectIdentifier(_) => BuiltinType::ObjectIdentifier,
        AstBuiltinType::ObjectDescriptor(_) => {
            BuiltinType::CharacterString(TagType::ObjectDescriptor)
        }
        AstBuiltinType::External(_) => {
            return Ok(UntaggedType::Reference(AstElement::new(
                EXTERNAL_IDENT.clone(),
                builtin.loc,
            )))
        }
        AstBuiltinType::Real(_) => {
            return Ok(UntaggedType::Reference(AstElement::new(
                REAL_IDENT.clone(),
                builtin.loc,
            )))
        }
        AstBuiltinType::Enumerated(enumerated) => parse_enumerated_type(parser, enumerated)?,
        AstBuiltinType::EmbeddedPDV(_) => {
            return Ok(UntaggedType::Reference(AstElement::new(
                EMBEDDED_PDV_IDENT.clone(),
                builtin.loc,
            )))
        }
        AstBuiltinType::UTF8String(_) => BuiltinType::CharacterString(TagType::UTF8String),
        AstBuiltinType::RelativeOid(_) => BuiltinType::RelativeOid,
        AstBuiltinType::Time(_) => BuiltinType::Time,
        AstBuiltinType::Structure(sequence) => parse_structure_type(parser, sequence, parameters)?,
        AstBuiltinType::Choice(choice) => parse_choice_type(parser, &choice.element.0, parameters)?,
        AstBuiltinType::NumericString(_) => BuiltinType::CharacterString(TagType::NumericString),
        AstBuiltinType::PrintableString(_) => {
            BuiltinType::CharacterString(TagType::PrintableString)
        }
        AstBuiltinType::TeletexString(_) => BuiltinType::CharacterString(TagType::TeletexString),
        AstBuiltinType::VideotexString(_) => BuiltinType::CharacterString(TagType::VideotexString),
        AstBuiltinType::IA5String(_) => BuiltinType::CharacterString(TagType::IA5String),
        AstBuiltinType::UTCTime(_) => BuiltinType::UTCTime,
        AstBuiltinType::GeneralizedTime(_) => BuiltinType::GeneralizedTime,
        AstBuiltinType::GraphicString(_) => BuiltinType::CharacterString(TagType::GraphicString),
        AstBuiltinType::VisibleString(_) => BuiltinType::CharacterString(TagType::VisibleString),
        AstBuiltinType::GeneralString(_) => BuiltinType::CharacterString(TagType::GeneralString),
        AstBuiltinType::UniversalString(_) => {
            BuiltinType::CharacterString(TagType::UniversalString)
        }
        AstBuiltinType::CharacterString(_) => {
            return Ok(UntaggedType::Reference(AstElement::new(
                CHARACTER_STRING_IDENT.clone(),
                builtin.loc,
            )))
        }
        AstBuiltinType::BMPString(_) => BuiltinType::CharacterString(TagType::BMPString),
        AstBuiltinType::Date(_) => BuiltinType::Date,
        AstBuiltinType::TimeOfDay(_) => BuiltinType::TimeOfDay,
        AstBuiltinType::DateTime(_) => BuiltinType::DateTime,
        AstBuiltinType::Duration(_) => BuiltinType::Duration,
        AstBuiltinType::InstanceOf(_) => unreachable!(),
    }))
}

pub(crate) fn resolve_defined_type(
    parser: &AstParser<'_>,
    defined_type: &AstElement<AstDefinedType>,
) -> Result<AstElement<QualifiedIdentifier>> {
    match &defined_type.element.external_module {
        Some(external_module) => {
            let header = parser
                .context
                .lookup_module(&parser.module)
                .expect("lookup_module");
            let mut imported_module = header
                .imports
                .iter()
                .find_map(|imports_from_module| {
                    if imports_from_module.module.element.name
                        == external_module.element.0.element.0
                    {
                        Some(imports_from_module.module.clone())
                    } else {
                        None
                    }
                })
                .ok_or_else(|| Error {
                    kind: ErrorKind::Ast(format!(
                        "no such imported module '{}'",
                        external_module.element.0.element.0
                    )),
                    loc: external_module.loc,
                })?;
            // if the module was imported by name only, see if it was declared with an OID
            // if so, apply the module's declared OID to the DeclaredValue's identifier
            match &mut imported_module.element.oid {
                Some(_) => (),
                oid @ None => {
                    let lookup_module = parser
                        .context
                        .lookup_module_by_name(&imported_module.element.name)
                        .expect("lookup_module_by_name");
                    if let Some(lookup_oid) = &lookup_module.ident.oid {
                        *oid = Some(lookup_oid.clone());
                    }
                }
            }
            Ok(defined_type.as_ref().map(|_| {
                QualifiedIdentifier::new(
                    imported_module.element,
                    defined_type.element.ty.element.0.clone(),
                )
            }))
        }
        None => {
            Ok(parser.resolve_symbol(&defined_type.element.ty.as_ref().map(|valref| &valref.0))?)
        }
    }
}

#[derive(Debug, Clone)]
pub enum Parameter {
    Type {
        ast: AstElement<AstType>,
        tagged_type: TaggedType,
    },
    Value {
        value_type: TaggedType,
        value: AstElement<TypedValue>,
    },
    ObjectClass {
        class_ref: AstElement<QualifiedIdentifier>,
    },
    ObjectSet {
        set_ref: AstElement<QualifiedIdentifier>,
    },
}

impl Parameter {
    pub fn get_name(&self) -> &'static str {
        match self {
            Parameter::Type { .. } => "type",
            Parameter::Value { .. } => "value",
            Parameter::ObjectClass { .. } => "information object class",
            Parameter::ObjectSet { .. } => "information object set",
        }
    }
}

pub(crate) fn resolve_parameterized_type_reference<'a>(
    parser: &'a AstParser<'_>,
    typeref: &AstElement<AstParameterizedDefinedType>,
    parameters: &[(&String, &Parameter)],
) -> Result<(&'a AstElement<AstTypeAssignment>, Vec<Parameter>)> {
    let name = resolve_defined_type(parser, &typeref.element.name)?;
    let parameterized_ast = parser
        .context
        .lookup_parameterized_type(&name.element)
        .ok_or_else(|| Error {
            kind: ErrorKind::Ast(format!(
                "undefined reference to parameterized type '{}'",
                name.element
            )),
            loc: name.loc,
        })?;
    let decl_list = &parameterized_ast
        .element
        .parameters
        .as_ref()
        .expect("lookup_parameterized_type returned non-parameterized type")
        .element
        .0;

    let parameters = typeref
        .element
        .parameters
        .element
        .0
        .iter()
        .zip(decl_list)
        .map(|(parameter, decl)| {
            Ok(match (&parameter.element, &decl.element) {
                (AstParameter::TypeParameter(ast), AstParameterDecl::TypeParameterDecl(_)) => {
                    let tagged_type = parse_type(parser, &ast.element.0, parameters)?;
                    if let UntaggedType::Reference(typeref) = &tagged_type.ty {
                        if typeref
                            .element
                            .name
                            .chars()
                            .all(|ch| ch.is_ascii_uppercase() || ch == '-')
                            && class::resolve_information_object_class(parser, typeref).is_ok()
                        {
                            return Ok(Parameter::ObjectClass {
                                class_ref: typeref.clone(),
                            });
                        }
                    }
                    Parameter::Type {
                        ast: ast.element.0.clone(),
                        tagged_type,
                    }
                }
                (AstParameter::ValueParameter(ast), AstParameterDecl::ValueParameterDecl(decl)) => {
                    // TODO: somehow move this to be after we parse all the non-parameterized types?
                    let value_type = parse_type(parser, &decl.element.ty, parameters)?;
                    let resolved_type = value_type.resolve(parser.context)?;
                    let value = values::parse_value(
                        parser,
                        ParseValueAssignmentStage::Normal,
                        &ast.element.0,
                        &resolved_type,
                    )?;
                    Parameter::Value { value_type, value }
                }
                (
                    AstParameter::ObjectSetParameter(ast),
                    AstParameterDecl::ObjectSetParameterDecl(_decl),
                ) => {
                    if ast.element.0.element.external_module.is_none() {
                        if let Some(param) = parameters.iter().find_map(|(name, param)| {
                            if *name == &ast.element.0.element.ty.element.0 {
                                Some(param)
                            } else {
                                None
                            }
                        }) {
                            match param {
                                object_set @ Parameter::ObjectSet { .. } => {
                                    return Ok((*object_set).clone())
                                }
                                other => {
                                    return Err(Error {
                                        kind: ErrorKind::Ast(format!(
                                            "expecting object set, but found {}",
                                            other.get_name(),
                                        )),
                                        loc: parameter.loc,
                                    })
                                }
                            }
                        }
                    }
                    Parameter::ObjectSet {
                        set_ref: resolve_defined_type(parser, &ast.element.0)?,
                    }
                }
                (param, decl) => {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "expecting {} parameter, but found {} parameter",
                            match decl {
                                AstParameterDecl::TypeParameterDecl(_) => "type",
                                AstParameterDecl::ValueParameterDecl(_) => "value",
                                AstParameterDecl::ObjectSetParameterDecl(_) =>
                                    "information object class set",
                            },
                            match param {
                                AstParameter::TypeParameter(_) => "type",
                                AstParameter::ValueParameter(_) => "value",
                                AstParameter::ObjectSetParameter(_) =>
                                    "information object class set",
                            },
                        )),
                        loc: parameter.loc,
                    })
                }
            })
        })
        .collect::<Result<Vec<_>>>()?;

    Ok((parameterized_ast, parameters))
}

fn parse_untagged_type(
    parser: &AstParser<'_>,
    ty: &AstElement<AstUntaggedType>,
    parameters: &[(&String, &Parameter)],
) -> Result<TaggedType> {
    let untagged = match &ty.element {
        AstUntaggedType::BuiltinType(builtin) => match &builtin.element {
            AstBuiltinType::InstanceOf(instance_of) => {
                let class_type = &instance_of.element.class_type;
                let class_ref = parser.resolve_symbol(&class_type.as_ref().map(|name| &name.0))?;

                let ast_instance_of_type = parser
                    .context
                    .lookup_parameterized_type(&INSTANCE_OF_IDENT)
                    .expect("INSTANCE OF implementation not found");

                return parser.run_with_context(&INSTANCE_OF_IDENT.module, |parser| {
                    parse_type(
                        parser,
                        match &ast_instance_of_type.element.subject.element {
                            AstTypeAssignmentSubject::Type(ast_type) => ast_type,
                            _ => unreachable!(),
                        },
                        &[(
                            &String::from("CLASS-TYPE"),
                            &Parameter::ObjectClass {
                                class_ref: class_ref.clone(),
                            },
                        )],
                    )
                });
            }
            _ => parse_builtin_type(parser, builtin, parameters)?,
        },
        AstUntaggedType::ParameterizedDefinedType(typeref) => {
            let (parameterized_ast, parameters) =
                resolve_parameterized_type_reference(parser, typeref, parameters)?;

            let module = resolve_defined_type(parser, &typeref.element.name)?
                .element
                .module;

            let decl = parser.run_with_context(&module, |parser| {
                match parse_type_assignment(
                    parser,
                    parameterized_ast,
                    &TypeAssignmentParseMode::Parameterized {
                        parameters: parameters.clone(), // clone required to move into closure
                    },
                ) {
                    Ok(Some((_, decl))) => Ok(decl),
                    Ok(None) => {
                        panic!("parse_type_assignment for parameterized type returned None")
                    }
                    Err(err) => Err(err),
                }
            })?;
            let resolved = decl.ty.resolve(parser.context)?;

            UntaggedType::BuiltinType(resolved.ty)
        }
        AstUntaggedType::DefinedType(typeref) => {
            if typeref.element.external_module.is_none() {
                if let Some(parameter) = parameters.iter().find_map(|(name, param)| match param {
                    Parameter::Type { tagged_type, .. } => {
                        if *name == &typeref.element.ty.element.0 {
                            Some(tagged_type)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }) {
                    return Ok(parameter.clone());
                }
            }
            UntaggedType::Reference(resolve_defined_type(parser, typeref)?)
        }
        AstUntaggedType::ObjectClassFieldType(ast_ocf) => {
            let class_ref = ast_ocf
                .element
                .class_type
                .as_ref()
                .map(|class_type| &class_type.0);
            let param = parameters.iter().find_map(|(name, param)| {
                if *name == class_ref.element {
                    Some(*param)
                } else {
                    None
                }
            });
            let class_type = match param {
                Some(param) => match param {
                    Parameter::ObjectClass { class_ref } => class_ref.clone(),
                    other => {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "expecting '{}' to be an information object class parameter, but found {}",
                                class_ref.element, other.get_name()
                            )),
                            loc: class_ref.loc,
                        })
                    }
                }
                None => parser.resolve_symbol(&class_ref)?,
            };
            let (kind, field) = match &ast_ocf.element.field.element {
                AstFieldReference::TypeFieldReference(type_field) => (
                    ObjectClassFieldReferenceKind::TypeLike,
                    type_field.element.0.as_ref().map(|name| name.0.clone()),
                ),
                AstFieldReference::ValueFieldReference(value_field) => (
                    ObjectClassFieldReferenceKind::ValueLike,
                    value_field.element.0.as_ref().map(|name| name.0.clone()),
                ),
            };

            UntaggedType::ObjectClassField(ObjectClassFieldReference {
                class_type,
                kind,
                field,
            })
        }
    };
    Ok(TaggedType {
        tag: None,
        ty: untagged,
        constraints: None,
    })
}

fn parse_constrained_type(
    parser: &AstParser<'_>,
    constrained_type: &AstElement<AstConstrainedType>,
    parameters: &[(&String, &Parameter)],
) -> Result<TaggedType> {
    Ok(match &constrained_type.element {
        AstConstrainedType::Suffixed(suffixed) => {
            parse_untagged_type(parser, &suffixed.element.ty, parameters)?
        }
        AstConstrainedType::TypeWithConstraint(twc) => TaggedType {
            tag: None,
            ty: parse_structure_of_type(parser, &twc.element.0, parameters)?,
            constraints: None,
        },
    })
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
    parameters: &[(&String, &Parameter)],
) -> Result<TaggedType> {
    let tag_default = parser
        .context
        .lookup_module(&parser.module)
        .expect("lookup_module")
        .tag_default;
    Ok(match &ty.element {
        AstType::TaggedType(ast_tagged_type) => {
            let tagged_type = parse_type(parser, &ast_tagged_type.element.ty, parameters)?;
            let tag = &ast_tagged_type.element.tag;
            let class = match tag.element.class {
                Some(ref class) => match class.element {
                    AstClass::Universal(_) => Class::Universal,
                    AstClass::Application(_) => Class::Application,
                    AstClass::Private(_) => Class::Private,
                },
                None => Class::ContextSpecific,
            };
            let class_number = &tag.element.class_number;
            if class_number.element.0 > num::BigUint::from(Tag::MAX_TAG as u64) {
                return Err(Error {
                    kind: ErrorKind::Ast(format!("tag number must not exceed {}", Tag::MAX_TAG)),
                    loc: class_number.loc,
                });
            }
            let tag_number = class_number
                .element
                .0
                .clone()
                .try_into()
                .expect("tag is out of bounds");
            let (mut kind, source) = match &ast_tagged_type.element.kind {
                Some(kind) => (
                    match kind.element {
                        AstTagKind::TagKindExplicit(_) => TagKind::Explicit(None),
                        AstTagKind::TagKindImplicit(_) => TagKind::Implicit,
                    },
                    TagSource::KindSpecified,
                ),
                None => (
                    match tag_default {
                        // AUTOMATIC does not always mean IMPLICIT
                        // see checks below
                        TagDefault::Automatic | TagDefault::Implicit => TagKind::Implicit,
                        TagDefault::Explicit => TagKind::Explicit(None),
                    },
                    TagSource::KindImplied,
                ),
            };

            if matches!(kind, TagKind::Implicit) {
                // TODO: this should probably be type-resolved so that references to CHOICE types are checked
                if matches!(
                    &tagged_type.ty,
                    UntaggedType::BuiltinType(BuiltinType::Choice(_))
                ) {
                    match source {
                        TagSource::KindSpecified => {
                            return Err(Error {
                                kind: ErrorKind::Ast(
                                    "CHOICE is not permitted to have IMPLICIT tagging".to_string(),
                                ),
                                loc: ast_tagged_type.element.kind.as_ref().unwrap().loc,
                            })
                        }
                        _ => {
                            kind = TagKind::Explicit(None);
                        }
                    }
                } else if matches!(&tagged_type.ty,
                    UntaggedType::ObjectClassField(ocf) if ocf.kind == ObjectClassFieldReferenceKind::TypeLike
                ) {
                    match source {
                        TagSource::KindSpecified => {
                            return Err(Error {
                                kind: ErrorKind::Ast(
                                    "information object class open type field reference is not permitted to have IMPLICIT tagging".to_string(),
                                ),
                                loc: ast_tagged_type.element.kind.as_ref().unwrap().loc,
                            })
                        }
                        _ => {
                            kind = TagKind::Explicit(None);
                        }
                    }
                }
            }

            let tag = Tag::new(class, tag_number, kind, source);
            TaggedType {
                tag: Some(tag),
                ty: tagged_type.ty,
                constraints: tagged_type.constraints, // TODO: should constraints from parameterized types be applied here?
            }
        }
        AstType::ConstrainedType(constrained) => {
            let tagged_type = parse_constrained_type(parser, constrained, parameters)?;
            TaggedType {
                tag: tagged_type.tag.or(match &tagged_type.ty {
                    UntaggedType::BuiltinType(builtin) => builtin.tag_type().map(Tag::universal),
                    UntaggedType::Reference(_) | UntaggedType::ObjectClassField(_) => None,
                }),
                ty: tagged_type.ty,
                constraints: tagged_type.constraints, // TODO: should constraints from parameterized types be applied here?
            }
        }
    })
}

pub(crate) struct ParameterDecl {
    _kind: ParameterDeclKind,
    name: String,
}

pub(crate) enum ParameterDeclKind {
    Type,
    Value,
    ObjectSet,
}

pub(crate) fn parse_type_assignment_parameters(
    _parser: &AstParser<'_>,
    type_assignment: &AstElement<AstTypeAssignment>,
) -> Result<Option<Vec<ParameterDecl>>> {
    Ok(
        match type_assignment.element.parameters.as_ref().map(|params| {
            params
                .element
                .0
                .iter()
                .map(|param| {
                    Ok(match &param.element {
                        AstParameterDecl::TypeParameterDecl(decl) => ParameterDecl {
                            _kind: ParameterDeclKind::Type,
                            name: decl.element.0.element.0.clone(),
                        },
                        AstParameterDecl::ValueParameterDecl(decl) => ParameterDecl {
                            _kind: ParameterDeclKind::Value,
                            name: decl.element.name.element.0.clone(),
                        },
                        AstParameterDecl::ObjectSetParameterDecl(decl) => ParameterDecl {
                            _kind: ParameterDeclKind::ObjectSet,
                            name: decl.element.name.element.0.clone(),
                        },
                    })
                })
                .collect::<Result<_>>()
        }) {
            Some(res) => Some(res?),
            None => None,
        },
    )
}

pub fn parse_parameterized_type_assignment(
    parser: &AstParser<'_>,
    type_assignment: &AstElement<AstTypeAssignment>,
) -> Result<Option<(QualifiedIdentifier, AstElement<AstTypeAssignment>)>> {
    let name = type_assignment.element.name.element.0.clone();
    let parameters = parse_type_assignment_parameters(parser, type_assignment)?;
    if parameters.map(|parameters| parameters.len()).unwrap_or(0) > 0 {
        Ok(Some((
            QualifiedIdentifier::new(parser.module.clone(), name),
            type_assignment.clone(),
        )))
    } else {
        Ok(None)
    }
}

pub(crate) fn ast_type_as_parameterized_type_reference(
    ast: &AstElement<AstType>,
) -> Option<&AstElement<AstParameterizedDefinedType>> {
    match &ast.element {
        AstType::TaggedType(tagged) => ast_type_as_parameterized_type_reference(&tagged.element.ty),
        AstType::ConstrainedType(constrained) => match &constrained.element {
            AstConstrainedType::Suffixed(suffixed) => match &suffixed.element.ty.element {
                AstUntaggedType::ParameterizedDefinedType(typeref) => Some(typeref),
                _ => None,
            },
            AstConstrainedType::TypeWithConstraint(_) => None,
        },
    }
}

pub enum TypeAssignmentParseMode {
    Normal,
    Parameterized { parameters: Vec<Parameter> },
}

pub fn parse_type_assignment(
    parser: &AstParser<'_>,
    type_assignment: &AstElement<AstTypeAssignment>,
    mode: &TypeAssignmentParseMode,
) -> Result<Option<(QualifiedIdentifier, DeclaredType)>> {
    let ast_name = &type_assignment.element.name;
    let name = ast_name.element.0.clone();
    let ident = QualifiedIdentifier::new(parser.module.clone(), name);

    match &type_assignment.element.subject.element {
        AstTypeAssignmentSubject::Type(ast_type) => {
            let parameter_names = parse_type_assignment_parameters(parser, type_assignment)?;

            match mode {
                TypeAssignmentParseMode::Normal => {
                    if parameter_names
                        .map(|parameters| parameters.len())
                        .unwrap_or(0)
                        > 0
                    {
                        Ok(None)
                    } else {
                        let ty = parse_type(parser, ast_type, &[])?;

                        Ok(Some((ident.clone(), DeclaredType { name: ident, ty })))
                    }
                }
                TypeAssignmentParseMode::Parameterized { parameters } => match parameter_names {
                    Some(names) => {
                        if names.len() != parameters.len() {
                            return Err(Error {
                                kind: ErrorKind::Ast(format!(
                                    "type '{}' expects {} parameters, but found {}",
                                    ident,
                                    names.len(),
                                    parameters.len(),
                                )),
                                loc: ast_name.loc,
                            });
                        }

                        let named_parameters = names
                            .iter()
                            .map(|param| &param.name)
                            .zip(parameters)
                            .collect::<Vec<(&String, &Parameter)>>();
                        let ty = parse_type(parser, ast_type, &named_parameters)?;

                        Ok(Some((ident.clone(), DeclaredType { name: ident, ty })))
                    }
                    None => panic!(
                        "type '{}' is not parameterized, but the parse mode is Parameterized",
                        ident,
                    ),
                },
            }
        }
        AstTypeAssignmentSubject::AbstractSyntax(_)
        | AstTypeAssignmentSubject::TypeIdentifier(_)
        | AstTypeAssignmentSubject::InformationObjectClass(_) => Ok(None),
    }
}
