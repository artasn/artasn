use num::{bigint::Sign, BigInt, BigUint};

use super::{
    context::{context, DeclaredValue},
    context_mut,
    oid_tree::{self, OidTreeNode, OidTreeNodeSearch},
    parser::*,
};
use crate::{module::*, types::*, values::*};

fn lookup_root_oid_component_str(component: &str) -> Option<&OidTreeNode> {
    oid_tree::lookup_root_node(component)
}

fn symbol_list_to_string_list(symbol_list: &AstSymbolList) -> Vec<String> {
    symbol_list
        .0
        .iter()
        .map(|symbol| {
            match match &symbol.element {
                AstSymbol::ParameterizedReference(parameterized) => {
                    parameterized.element.0.element.clone()
                }
                AstSymbol::Reference(reference) => reference.element.clone(),
            } {
                AstReference::TypeReference(type_ref) => type_ref.element.0,
                AstReference::ValueReference(val_ref) => val_ref.element.0,
            }
        })
        .collect()
}

// fn module_ref_to_module_ident(module_ref: &GlobalModuleReference) -> Result<ModuleIdentifier> {
//     Ok(ModuleIdentifier {
//         name: module_ref.name.element.0.clone(),
//         oid: match module_ref.oid {
//             Some(ref oid) => Some(ObjectIdentifier(match oid.element {
//                 Oid::ObjectIdentifierValue(ref oid) => oid
//                     .element
//                     .components
//                     .iter()
//                     .map(|elem| {
//                         Ok(match elem.element {
//                             ObjectIdentifierComponent::Number(ref num) => num.element.0,
//                             ObjectIdentifierComponent::NamedNumber(ref named_num) => {
//                                 named_num.element.num.element.0
//                             }
//                             ObjectIdentifierComponent::DefinedValue(ref val) => {
//                                 lookup_oid_component(&val.element.value)?
//                             }
//                             ObjectIdentifierComponent::ValueReference(ref val_ref) => {
//                                 // this branch should be covered by ObjectIdentifierComponent::DefinedValue
//                                 panic!("ObjectIdentifierComponent::ValueReference: {:#?}", val_ref)
//                             }
//                         })
//                     })
//                     .collect::<Result<Vec<u64>>>()?,
//                 Oid::DefinedValue(ref val) => {
//                     todo!("Oid::DefinedValue: {:#?}", val)
//                 }
//             })),
//             None => None,
//         },
//     })
// }

fn module_ast_to_module_ident(header: &AstElement<AstModuleHeader>) -> Result<ModuleIdentifier> {
    name_and_oid_to_module_ident(
        &header.element.name,
        header.element.oid.as_ref().map(|oid| match &oid.element {
            AstModuleIdentifier::DefinitiveOid(oid) => oid,
            AstModuleIdentifier::DefinitiveOidWithIri(oid_with_iri) => &oid_with_iri.element.oid,
        }),
    )
}

fn module_ref_to_module_ident(
    module_ref: &AstElement<AstGlobalModuleReference>,
) -> Result<ModuleIdentifier> {
    let oid = match &module_ref.element.oid {
        Some(oid) => match &oid.element {
            AstAssignedIdentifier::DefinitiveOid(oid) => Some(oid),
            // TODO: implement import by valuereference for oid
            AstAssignedIdentifier::ValueIdentifier(_) => None,
        },
        None => None,
    };
    name_and_oid_to_module_ident(&module_ref.element.name, oid)
}

pub fn name_and_oid_to_module_ident(
    name: &AstElement<AstTypeReference>,
    oid: Option<&AstElement<AstDefinitiveOid>>,
) -> Result<ModuleIdentifier> {
    Ok(ModuleIdentifier {
        name: name.element.0.clone(),
        oid: match oid {
            Some(oid) => Some({
                let searches = oid
                    .element
                    .0
                    .iter()
                    .map(|elem| match &elem.element {
                        AstDefinitiveOidComponent::Number(num) => {
                            OidTreeNodeSearch::Number(num.element.0)
                        }
                        AstDefinitiveOidComponent::NamedNumber(named_num) => {
                            OidTreeNodeSearch::Number(named_num.element.num.element.0)
                        }
                        AstDefinitiveOidComponent::ValueReference(val_ref) => {
                            OidTreeNodeSearch::Name(
                                val_ref.as_ref().map(|val_ref| val_ref.0.clone()),
                            )
                        }
                    })
                    .collect::<Vec<OidTreeNodeSearch>>();
                oid_tree::search(searches)?
            }),
            None => None,
        },
    })
}

fn parse_object_identifier_component_valref(
    oid: &ModuleIdentifier,
    value: &AstElement<AstValueReference>,
) -> ObjectIdentifierComponent {
    lookup_root_oid_component_str(&value.element.0).map_or_else(
        || ObjectIdentifierComponent::ValueReference(parse_valuereference(oid, value)),
        |lit| ObjectIdentifierComponent::IntegerLiteral(AstElement::new(lit.node, value.loc)),
    )
}

fn parse_object_identifier(
    oid: &ModuleIdentifier,
    object_id: &AstElement<AstObjectIdentifierValue>,
) -> Result<ObjectIdentifier> {
    Ok(ObjectIdentifier(
        object_id
            .element
            .components
            .iter()
            .map(|elem| {
                Ok(match &elem.element {
                    AstObjectIdentifierComponent::Number(num) => {
                        ObjectIdentifierComponent::IntegerLiteral(num.as_ref().map(|num| num.0))
                    }
                    AstObjectIdentifierComponent::NamedNumber(named_num) => {
                        ObjectIdentifierComponent::IntegerLiteral(
                            named_num.as_ref().map(|named_num| named_num.num.element.0),
                        )
                    }
                    AstObjectIdentifierComponent::ValueReference(val_ref) => {
                        parse_object_identifier_component_valref(oid, val_ref)
                    }
                    AstObjectIdentifierComponent::DefinedValue(defined_val) => {
                        parse_object_identifier_component_valref(oid, &defined_val.element.value)
                    }
                })
            })
            .collect::<Result<Vec<ObjectIdentifierComponent>>>()?,
    ))
}

/// First stage: register all module headers.
pub fn register_all_modules(program: &AstElement<AstProgram>) -> Result<Vec<Error>> {
    let mut errors = Vec::new();

    for module in &program.element.0 {
        let header = &module.element.header;
        let oid = match module_ast_to_module_ident(header) {
            Ok(oid) => oid,
            Err(err) => {
                errors.push(err);
                continue;
            }
        };
        let tag_default = match header
            .element
            .tag_default
            .as_ref()
            .map(|e| &e.element.0.element)
        {
            Some(tag_default) => match tag_default {
                AstTagDefaultKind::TagDefaultExplicit(_) => TagDefault::Explicit,
                AstTagDefaultKind::TagDefaultAutomatic(_) => TagDefault::Automatic,
                AstTagDefaultKind::TagDefaultImplicit(_) => TagDefault::Implicit,
            },
            None => TagDefault::Explicit,
        };
        let extensibility_implied = header.element.extension_default.is_some();
        let exports = match header
            .element
            .exports
            .as_ref()
            .map(|exports| &exports.element.0)
        {
            Some(Some(exports)) => match &exports.element {
                AstExportsKind::All(_) => Exports::All,
                AstExportsKind::SymbolList(symbol_list) => {
                    Exports::SymbolList(symbol_list_to_string_list(&symbol_list.element))
                }
            },
            // TODO: should "EXPORTS;" be treated as "EXPORTS ALL;" or an empty symbol list?
            // probably should be a compiler option?
            _ => Exports::All,
        };
        let mut imports = Vec::new();
        if let Some(ref ast_imports) = header.element.imports {
            for symbols_from_module in &ast_imports.element.0 {
                let symbols =
                    symbol_list_to_string_list(&symbols_from_module.element.symbols.element);
                let module = module_ref_to_module_ident(&symbols_from_module.element.module)?;
                for name in symbols {
                    imports.push(QualifiedIdentifier {
                        module: module.clone(),
                        name,
                    });
                }
            }
        }

        context_mut().register_module(ModuleHeader {
            oid,
            tag_default,
            extensibility_implied,
            exports,
            imports,
        });
    }

    Ok(errors)
}

fn parse_constant(
    oid: &ModuleIdentifier,
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
                Value::BuiltinValue(parse_integer_value(num)?)
            }
            AstConstant::DefinedValue(value) => {
                parse_valuereference(oid, &value.element.value).element
            }
        },
        ast_constant.loc,
    ))
}

fn parse_constraints(
    oid: &ModuleIdentifier,
    kind: ConstraintsKind,
    ast_constraints: &AstElement<AstConstraints>,
) -> Result<Constraints> {
    let mut constraints = Vec::new();

    for ast_constraint in &ast_constraints.element.constraints {
        match &ast_constraint.element {
            AstConstraint::ConstantSeries(ast_series) => {
                let mut series = Vec::new();
                for ast_constant in &ast_series.element.0 {
                    series.push(parse_constant(oid, ast_constant, Some(kind))?);
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
                            parse_constant(oid, ast_constant, Some(kind))?,
                        ),
                        AstRangeLowerBound::GtConstant(ast_constant) => {
                            RangeLowerBound::GtConstant(parse_constant(
                                oid,
                                &ast_constant.element.0,
                                Some(kind),
                            )?)
                        }
                        AstRangeLowerBound::Min(_) => RangeLowerBound::Min,
                    },
                    upper: match &ast_range.element.upper.element {
                        AstRangeUpperBound::Constant(ast_constant) => RangeUpperBound::Constant(
                            parse_constant(oid, ast_constant, Some(kind))?,
                        ),
                        AstRangeUpperBound::LtConstant(ast_constant) => {
                            RangeUpperBound::LtConstant(parse_constant(
                                oid,
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
    oid: &ModuleIdentifier,
    components: &Vec<AstElement<AstStructureComponent>>,
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
                oid,
                &component.element.ty,
                TypeContext::StructureComponent {
                    index: i as u16,
                    has_tags,
                },
            )?;
            let resolved_ty = ty.resolve()?;
            Ok(StructureComponent {
                name: component.element.name.as_ref().map(|name| name.0.clone()),
                default_value: match component
                    .element
                    .default
                    .as_ref()
                    .map(|default| Ok(Box::new(parse_value(oid, default, &resolved_ty)?)))
                {
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
    oid: &ModuleIdentifier,
    size_constraints: Option<&AstElement<AstSizeConstraints>>,
) -> Result<Option<Constraints>> {
    Ok(match size_constraints {
        Some(sc) => Some(parse_constraints(
            oid,
            ConstraintsKind::Size,
            &sc.element.0,
        )?),
        None => None,
    })
}

fn parse_structure_type(
    oid: &ModuleIdentifier,
    tag_type: TagType,
    structure: &AstElement<AstStructureKind>,
) -> Result<BuiltinType> {
    Ok(match &structure.element {
        AstStructureKind::SingleStructure(structure) => BuiltinType::Structure(Structure {
            ty: tag_type,
            components: parse_structure_components(oid, &structure.element.components)?,
        }),
        AstStructureKind::StructureOf(structure_of) => BuiltinType::StructureOf(StructureOf {
            ty: tag_type,
            size_constraints: parse_size_constraints(
                oid,
                structure_of.element.size_constraints.as_ref(),
            )?,
            component_type: Box::new(parse_type(
                oid,
                &structure_of.element.ty,
                TypeContext::Contextless,
            )?),
        }),
    })
}

fn parse_enumerated_type(
    oid: &ModuleIdentifier,
    enumerated: &AstElement<AstEnumerated>,
) -> Result<BuiltinType> {
    let mut items = Vec::new();
    let mut implied_index = 0;
    for ast_item in &enumerated.element.0 {
        let value = match &ast_item.element.num {
            Some(num) => EnumerationItemValue::Specified(parse_constant(oid, num, None)?),
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
    oid: &ModuleIdentifier,
    builtin: &AstElement<AstBuiltinType>,
) -> Result<BuiltinType> {
    Ok(match &builtin.element {
        AstBuiltinType::Boolean(_) => BuiltinType::Boolean,
        AstBuiltinType::Integer(integer) => BuiltinType::Integer(IntegerType {
            named_values: None, // TODO
            value_constraints: match &integer.element.value_constraints {
                Some(sc) => Some(parse_constraints(oid, ConstraintsKind::Value, sc)?),
                None => None,
            },
        }),
        AstBuiltinType::BitString(bit_string) => BuiltinType::BitString(BitStringType {
            named_bits: None, // TODO
            size_constraints: parse_size_constraints(
                oid,
                bit_string
                    .element
                    .size_constraints
                    .as_ref()
                    .map(|sc| &sc.element.0),
            )?,
        }),
        AstBuiltinType::OctetString(octet_string) => BuiltinType::OctetString(OctetStringType {
            size_constraints: parse_size_constraints(
                oid,
                octet_string
                    .element
                    .size_constraints
                    .as_ref()
                    .map(|sc| &sc.element.0),
            )?,
        }),
        AstBuiltinType::Null(_) => BuiltinType::Null,
        AstBuiltinType::ObjectIdentifier(_) => BuiltinType::ObjectIdentifier,
        AstBuiltinType::ObjectDescriptor(_) => todo!("ObjectDescriptor"),
        AstBuiltinType::External(_) => todo!("External"),
        AstBuiltinType::Real(_) => todo!("Real"),
        AstBuiltinType::Enumerated(enumerated) => parse_enumerated_type(oid, enumerated)?,
        AstBuiltinType::EmbeddedPDV(_) => todo!("EmbeddedPDV"),
        AstBuiltinType::UTF8String(_) => BuiltinType::CharacterString(TagType::UTF8String),
        AstBuiltinType::RelativeOid(_) => todo!("RelativeOid"),
        AstBuiltinType::Time(_) => todo!("Time"),
        AstBuiltinType::Sequence(sequence) => {
            parse_structure_type(oid, TagType::Sequence, &sequence.element.0)?
        }
        AstBuiltinType::Set(sequence) => {
            parse_structure_type(oid, TagType::Set, &sequence.element.0)?
        }
        AstBuiltinType::NumericString(_) => BuiltinType::CharacterString(TagType::NumericString),
        AstBuiltinType::PrintableString(_) => {
            BuiltinType::CharacterString(TagType::PrintableString)
        }
        AstBuiltinType::TeletexString(_) => BuiltinType::CharacterString(TagType::TeletexString),
        AstBuiltinType::VideotexString(_) => BuiltinType::CharacterString(TagType::VideotexString),
        AstBuiltinType::IA5String(_) => BuiltinType::CharacterString(TagType::IA5String),
        AstBuiltinType::UTCTime(_) => todo!("UTCTime"),
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
        AstBuiltinType::Date(_) => todo!("Date"),
        AstBuiltinType::TimeOfDay(_) => todo!("TimeOfDay"),
        AstBuiltinType::DateTime(_) => todo!("DateTime"),
        AstBuiltinType::Duration(_) => todo!("Duration"),
    })
}

fn parse_untagged_type(
    oid: &ModuleIdentifier,
    ty: &AstElement<AstUntaggedType>,
) -> Result<UntaggedType> {
    Ok(match ty.element {
        AstUntaggedType::BuiltinType(ref builtin) => {
            UntaggedType::BuiltinType(parse_builtin_type(oid, builtin)?)
        }
        AstUntaggedType::TypeReference(ref typeref) => UntaggedType::Reference(AstElement::new(
            context()
                .lookup_module(oid)
                .expect("lookup_module")
                .resolve_symbol(&typeref.element.0),
            typeref.loc,
        )),
    })
}

enum TypeContext {
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
fn parse_type(
    oid: &ModuleIdentifier,
    ty: &AstElement<AstType>,
    type_context: TypeContext,
) -> Result<TaggedType> {
    let tag_default = context()
        .lookup_module(oid)
        .expect("lookup_module")
        .tag_default;
    Ok(match &ty.element {
        AstType::TaggedType(tagged_type) => {
            let ty = parse_untagged_type(oid, &tagged_type.element.ty)?;
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
            let (kind, source) = match tagged_type.element.kind {
                Some(ref kind) => (
                    match kind.element {
                        AstTagKind::TagKindExplicit(_) => TagKind::Explicit,
                        AstTagKind::TagKindImplicit(_) => TagKind::Implicit,
                    },
                    TagSource::KindSpecified,
                ),
                None => (
                    match tag_default {
                        // TODO: Automatic is not always Implicit, there are special cases where it means Explicit
                        TagDefault::Automatic | TagDefault::Implicit => TagKind::Implicit,
                        TagDefault::Explicit => TagKind::Explicit,
                    },
                    TagSource::KindImplied,
                ),
            };
            let tag = Tag::new(class, tag_number, kind, source);
            TaggedType { tag: Some(tag), ty }
        }
        AstType::UntaggedType(untagged) => {
            let ty = parse_untagged_type(oid, untagged)?;
            let index = match type_context {
                TypeContext::StructureComponent { index, has_tags } => match has_tags {
                    true => None,
                    false => Some(index),
                },
                TypeContext::Contextless => None,
            };
            let tag = match (tag_default, index) {
                (TagDefault::Automatic, Some(index)) => {
                    // TODO: Automatic is not always Implicit, there are special cases where it means Explicit
                    Some(Tag::new(
                        Class::ContextSpecific,
                        index,
                        TagKind::Implicit,
                        TagSource::TagImplied,
                    ))
                }
                _ => match &ty {
                    UntaggedType::BuiltinType(builtin) => {
                        Some(Tag::universal(builtin.tag_type().expect("tag_type")))
                    }
                    UntaggedType::Reference(_) => None,
                },
            };
            TaggedType { tag, ty }
        }
    })
}

fn register_type(
    oid: &ModuleIdentifier,
    type_assignment: &AstElement<AstTypeAssignment>,
) -> Result<()> {
    let name = type_assignment.element.name.element.0.clone();
    let ty = parse_type(oid, &type_assignment.element.ty, TypeContext::Contextless)?;

    context_mut().register_type(
        QualifiedIdentifier {
            module: oid.clone(),
            name,
        },
        ty,
    );

    Ok(())
}

/// Second stage: register all declared types.
pub fn register_all_types(program: &AstElement<AstProgram>) -> Result<Vec<Error>> {
    let mut errors = Vec::new();

    for module in &program.element.0 {
        let header = &module.element.header;
        let oid = module_ast_to_module_ident(header)?;

        for assignment in &module.element.body.element.0 {
            if let AstAssignment::TypeAssignment(ref type_assignment) = assignment.element {
                if let Err(err) = register_type(&oid, type_assignment) {
                    errors.push(err);
                }
            }
        }
    }

    Ok(errors)
}

fn parse_valuereference(
    oid: &ModuleIdentifier,
    valref: &AstElement<AstValueReference>,
) -> AstElement<Value> {
    valref.as_ref().map(|valref| {
        Value::Reference(
            context()
                .lookup_module(oid)
                .expect("lookup_module")
                .resolve_symbol(&valref.0),
        )
    })
}

fn parse_integer_value(num: &AstElement<AstIntegerValue>) -> Result<BuiltinValue> {
    if num.element.sign.is_none() {
        let uint = num.element.value.element.0;
        if uint > i64::MAX as u64 {
            return Err(Error {
                kind: ErrorKind::Ast(format!(
                    "number {} too large for a 64-bit signed integer",
                    uint,
                )),
                loc: num.loc,
            });
        }
        Ok(BuiltinValue::Integer(BigInt::from_biguint(
            Sign::Plus,
            BigUint::from(uint),
        )))
    } else {
        let int = num.element.value.element.0;
        if int > i64::MIN as u64 {
            return Err(Error {
                kind: ErrorKind::Ast(format!(
                    "number -{} too small for a 64-bit signed integer",
                    int,
                )),
                loc: num.loc,
            });
        }
        Ok(BuiltinValue::Integer(BigInt::from_biguint(
            Sign::Minus,
            BigUint::from(int),
        )))
    }
}

fn parse_structure_value(
    oid: &ModuleIdentifier,
    struct_val: &AstElement<AstStructureValue>,
    target_type: &ResolvedType,
) -> Result<BuiltinValue> {
    let tag_type = target_type.ty.tag_type().expect("tag_type");
    let struct_ty_components = match &target_type.ty {
        BuiltinType::Structure(ty) => &ty.components,
        ty => {
            return Err(Error {
                kind: ErrorKind::Ast(format!("SEQUENCE value cannot be assigned to {} type", ty)),
                loc: struct_val.loc,
            })
        }
    };
    for val_component in &struct_val.element.components {
        if !struct_ty_components
            .iter()
            .any(|ty_component| ty_component.name.element == val_component.element.name.element.0)
        {
            return Err(Error {
                kind: ErrorKind::Ast(format!(
                    "no such component '{}' in SEQUENCE type",
                    val_component.element.name.element.0
                )),
                loc: val_component.element.name.loc,
            });
        }
    }
    let mut components = Vec::new();
    for ty_component in struct_ty_components {
        let (value, is_default) = {
            if let Some(val_component) =
                struct_val.element.components.iter().find(|val_component| {
                    val_component.element.name.element.0 == ty_component.name.element
                })
            {
                let component_type = ty_component.component_type.resolve()?;
                (
                    parse_value(oid, &val_component.element.value, &component_type)?,
                    false,
                )
            } else if let Some(default_value) = &ty_component.default_value {
                ((**default_value).clone(), true)
            } else {
                if ty_component.optional {
                    continue;
                }
                return Err(Error {
                    kind: ErrorKind::Ast(format!(
                        "{} value missing component '{}' of type '{}'",
                        tag_type, ty_component.name.element, ty_component.component_type,
                    )),
                    loc: struct_val.loc,
                });
            }
        };

        components.push(StructureValueComponent {
            name: ty_component.name.clone(),
            value,
            is_default,
        });
    }
    let value = StructureValue { components };
    Ok(match tag_type {
        TagType::Sequence => BuiltinValue::Sequence(value),
        TagType::Set => BuiltinValue::Set(value),
        _ => unreachable!(),
    })
}

fn parse_character_string(
    str_lit: &AstElement<AstStringLiteral>,
    tag_type: TagType,
) -> Result<BuiltinValue> {
    let cstring = match &str_lit.element.kind {
        StringKind::CString => str_lit.element.data.clone(),
        _ => {
            return Err(Error {
                kind: ErrorKind::Ast(format!(
                    "{} value cannot be assigned to {}",
                    str_lit.element.kind.to_string(),
                    tag_type
                )),
                loc: str_lit.loc,
            });
        }
    };
    let validator = match tag_type {
        TagType::UTF8String
        | TagType::UniversalString
        | TagType::GeneralString
        | TagType::BMPString
        | TagType::CharacterString => |_: char| true,
        TagType::NumericString => |ch: char| ch.is_ascii_digit() || ch == ' ',
        TagType::PrintableString => {
            |ch: char| ch.is_ascii_alphanumeric() || " '()+,-./:=?".contains(ch)
        }
        TagType::TeletexString | TagType::VideotexString => {
            |ch: char| ch.is_ascii_graphic() || ch == ' ' || ch == '\x7f'
        }
        TagType::VisibleString => |ch: char| ch.is_ascii_graphic() || ch == ' ',
        TagType::IA5String => |ch: char| ch <= '\x7f',
        TagType::GraphicString => |ch: char| ch == ' ' || ch.is_alphanumeric(),
        _ => unreachable!(),
    };
    let invalid = cstring.chars().map(validator).any(|valid| !valid);
    if invalid {
        return Err(Error {
            kind: ErrorKind::Ast(format!(
                "provided cstring does not meet the character constraints for {}",
                tag_type
            )),
            loc: str_lit.loc,
        });
    }
    Ok(BuiltinValue::CharacterString(tag_type, cstring))
}

fn parse_value(
    oid: &ModuleIdentifier,
    value: &AstElement<AstValue>,
    target_type: &ResolvedType,
) -> Result<AstElement<Value>> {
    Ok(AstElement::new(
        match &value.element {
            AstValue::BuiltinValue(builtin) => Value::BuiltinValue(match &builtin.element {
                AstBuiltinValue::Null(_) => BuiltinValue::Null,
                AstBuiltinValue::BooleanValue(b) => match b.element {
                    AstBooleanValue::True(_) => BuiltinValue::Boolean(true),
                    AstBooleanValue::False(_) => BuiltinValue::Boolean(false),
                },
                AstBuiltinValue::StringLiteral(str_lit) => match &target_type.ty {
                    BuiltinType::BitString(_) => {
                        let radix = match str_lit.element.kind {
                            StringKind::BString => 2,
                            StringKind::HString => 16,
                            StringKind::CString => {
                                return Err(Error {
                                    kind: ErrorKind::Ast(
                                        "cstring value cannot be assigned to BIT STRING"
                                            .to_string(),
                                    ),
                                    loc: builtin.loc,
                                })
                            }
                        };
                        BuiltinValue::BitString(
                            BigUint::parse_bytes(str_lit.element.data.as_bytes(), radix)
                                .expect("failed BigUint::parse_bytes"),
                        )
                    }
                    BuiltinType::OctetString(_) => {
                        let radix = match str_lit.element.kind {
                            StringKind::BString => 2,
                            StringKind::HString => 16,
                            StringKind::CString => {
                                return Err(Error {
                                    kind: ErrorKind::Ast(
                                        "cstring value cannot be assigned to OCTET STRING"
                                            .to_string(),
                                    ),
                                    loc: builtin.loc,
                                })
                            }
                        };
                        BuiltinValue::OctetString(
                            BigUint::parse_bytes(str_lit.element.data.as_bytes(), radix)
                                .expect("failed BigUint::parse_bytes")
                                .to_bytes_be(),
                        )
                    }
                    BuiltinType::CharacterString(tag_type) => {
                        parse_character_string(str_lit, *tag_type)?
                    }
                    other_type => {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "{} value cannot be assigned to {}",
                                str_lit.element.kind.to_string(),
                                other_type,
                            )),
                            loc: builtin.loc,
                        })
                    }
                },
                AstBuiltinValue::ObjectIdentifierValue(object_id) => {
                    BuiltinValue::ObjectIdentifier(parse_object_identifier(oid, object_id)?)
                }
                AstBuiltinValue::IntegerValue(num) => parse_integer_value(num)?,
                AstBuiltinValue::StructureValue(seq_val) => {
                    parse_structure_value(oid, seq_val, target_type)?
                }
                AstBuiltinValue::StructureOfValue(of_val) => {
                    let component_type = match &target_type.ty {
                        BuiltinType::StructureOf(seq_of) => &seq_of.component_type,
                        other_type => {
                            return Err(Error {
                                kind: ErrorKind::Ast(format!(
                                    "SEQUENCE OF/SET OF value cannot be assigned to {}",
                                    other_type,
                                )),
                                loc: builtin.loc,
                            })
                        }
                    };
                    let component_type = component_type.resolve()?;

                    let ast_elements = &of_val.element.elements;
                    let mut elements = Vec::with_capacity(ast_elements.len());
                    for ast_element in ast_elements {
                        elements.push(parse_value(oid, ast_element, &component_type)?);
                    }
                    BuiltinValue::SequenceOf(elements)
                }
                other_builtin => todo!("{:#?}", other_builtin),
            }),
            AstValue::ValueReference(valref) => match &target_type.ty {
                BuiltinType::Enumerated(items) => 'block: {
                    for item in items {
                        if item.name.element == valref.element.0 {
                            break 'block Value::BuiltinValue(BuiltinValue::Enumerated(Box::new(
                                match &item.value {
                                    EnumerationItemValue::Implied(implied) => AstElement::new(
                                        Value::BuiltinValue(BuiltinValue::Integer(BigInt::from(
                                            *implied,
                                        ))),
                                        value.loc,
                                    ),
                                    EnumerationItemValue::Specified(specified) => specified.clone(),
                                },
                            )));
                        }
                    }

                    parse_valuereference(oid, valref).element
                }
                _ => parse_valuereference(oid, valref).element,
            },
        },
        value.loc,
    ))
}

fn register_value(
    oid: &ModuleIdentifier,
    value_assignment: &AstElement<AstValueAssignment>,
) -> Result<()> {
    let name = value_assignment.element.name.element.0.clone();
    let ty = parse_type(oid, &value_assignment.element.ty, TypeContext::Contextless)?;
    let resolved_ty = ty.resolve()?;
    let val = parse_value(oid, &value_assignment.element.value, &resolved_ty)?;

    context_mut().register_value(
        QualifiedIdentifier {
            module: oid.clone(),
            name,
        },
        DeclaredValue { value: val, ty },
    );

    Ok(())
}

/// Third stage: register all declared values.
pub fn register_all_values(program: &AstElement<AstProgram>) -> Result<Vec<Error>> {
    let mut errors = Vec::new();

    for module in &program.element.0 {
        let oid = module_ast_to_module_ident(&module.element.header)?;

        for assignment in &module.element.body.element.0 {
            if let AstAssignment::ValueAssignment(ref value_assignment) = assignment.element {
                if let Err(err) = register_value(&oid, value_assignment) {
                    errors.push(err);
                }
            }
        }
    }

    Ok(errors)
}

// See X.680 clauses 25.6, 25.6.1, and 27.3 to see exactly what is being enforced here.
fn verify_unique_component_tags(structure: &Structure) -> Result<()> {
    if structure.components.len() <= 1 {
        return Ok(());
    }

    match structure.ty {
        TagType::Sequence => {
            struct ComponentData<'a> {
                pub name: &'a AstElement<String>,
                pub class: Class,
                pub num: u16,
            }

            let mut consecutive_optionals: Vec<ComponentData<'_>> = Vec::new();
            for i in 0..structure.components.len() {
                let component = &structure.components[i];
                let tag = component.component_type.resolve()?.tag;

                let illegal_component = 'block: {
                    for data in &consecutive_optionals {
                        if data.class == tag.class && data.num == tag.num {
                            break 'block Some(data);
                        }
                    }

                    None
                };
                if let Some(illegal_component) = illegal_component {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "SEQUENCE component '{}' and component '{}' must have distinct tags",
                            illegal_component.name.element, component.name.element
                        )),
                        loc: illegal_component.name.loc,
                    });
                }

                if component.optional || component.default_value.is_some() {
                    consecutive_optionals.push(ComponentData {
                        name: &component.name,
                        class: tag.class,
                        num: tag.num,
                    });
                } else {
                    consecutive_optionals.clear();
                }
            }
        }
        TagType::Set => {
            for i in 0..structure.components.len() {
                for j in 0..structure.components.len() {
                    if i == j {
                        continue;
                    }

                    let a = &structure.components[i];
                    let b = &structure.components[j];
                    let tag_a = a.component_type.resolve()?.tag;
                    let tag_b = b.component_type.resolve()?.tag;
                    if tag_a.class == tag_b.class && tag_a.num == tag_b.num {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "SET component '{}' and component '{}' must have distinct tags",
                                a.name.element, b.name.element
                            )),
                            loc: a.name.loc,
                        });
                    }
                }
            }
        }
        _ => unreachable!(),
    }

    Ok(())
}

fn verify_type(declared_type: &TaggedType) -> Result<()> {
    let resolved_ty = declared_type.resolve()?;
    if let BuiltinType::Structure(structure) = &resolved_ty.ty {
        verify_unique_component_tags(structure)?
    }

    Ok(())
}

/// Fourth stage: verify all types.
pub fn verify_all_types(program: &AstElement<AstProgram>) -> Result<Vec<Error>> {
    let mut errors = Vec::new();

    for module in &program.element.0 {
        let oid = module_ast_to_module_ident(&module.element.header)?;

        for (ident, tagged_type) in context().list_types() {
            if ident.module != oid {
                continue;
            }

            if let Err(err) = verify_type(tagged_type) {
                errors.push(err);
            }
        }
    }

    Ok(errors)
}

fn verify_value(declared_value: &DeclaredValue) -> Result<()> {
    let resolved_ty = declared_value.ty.resolve()?;
    resolved_ty
        .ty
        .ensure_satisfied_by_value(&declared_value.value)?;

    Ok(())
}

/// Fifth stage: verify all declared values.
pub fn verify_all_values(program: &AstElement<AstProgram>) -> Result<Vec<Error>> {
    let mut errors = Vec::new();

    for module in &program.element.0 {
        let oid = module_ast_to_module_ident(&module.element.header)?;

        for (ident, declared_value) in context().list_values() {
            if ident.module != oid {
                continue;
            }

            if let Err(err) = verify_value(declared_value) {
                errors.push(err);
            }
        }
    }

    Ok(errors)
}
