use num::BigUint;

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
            Some(defined) => match defined {
                Some(exports) => match &exports.element {
                    AstExportsKind::All(_) => Exports::All,
                    AstExportsKind::SymbolList(symbol_list) => {
                        Exports::SymbolList(symbol_list_to_string_list(&symbol_list.element))
                    }
                },
                None => Exports::All,
            },
            // TODO: should "EXPORTS;" be treated as "EXPORTS ALL;" or an empty symbol list?
            // probably should be a compiler option?
            None => Exports::All,
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

fn parse_constraints_constant(
    oid: &ModuleIdentifier,
    kind: ConstraintsKind,
    ast_constant: &AstElement<AstConstant>,
) -> Result<AstElement<valref!(Integer)>> {
    Ok(AstElement::new(
        match &ast_constant.element {
            AstConstant::IntegerValue(num) => {
                if kind == ConstraintsKind::Size {
                    if let Some(sign) = &num.element.sign {
                        return Err(Error {
                            kind: ErrorKind::Ast(
                                "size constraint must be a non-negative integer".to_owned(),
                            ),
                            loc: sign.loc,
                        });
                    }
                }
                ValueReference::Value(parse_integer_value(num)?)
            }
            AstConstant::DefinedValue(value) => parse_valuereference(oid, &value.element.value)
                .element
                .as_type::<{ TagType::Integer as u8 }>()
                .clone(),
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
                    series.push(parse_constraints_constant(oid, kind, ast_constant)?);
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
                            parse_constraints_constant(oid, kind, ast_constant)?,
                        ),
                        AstRangeLowerBound::GtConstant(ast_constant) => {
                            RangeLowerBound::GtConstant(parse_constraints_constant(
                                oid,
                                kind,
                                &ast_constant.element.0,
                            )?)
                        }
                        AstRangeLowerBound::Min(_) => RangeLowerBound::Min,
                    },
                    upper: match &ast_range.element.upper.element {
                        AstRangeUpperBound::Constant(ast_constant) => RangeUpperBound::Constant(
                            parse_constraints_constant(oid, kind, ast_constant)?,
                        ),
                        AstRangeUpperBound::LtConstant(ast_constant) => {
                            RangeUpperBound::LtConstant(parse_constraints_constant(
                                oid,
                                kind,
                                &ast_constant.element.0,
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
    components
        .iter()
        .map(|component| {
            let ty = parse_type(oid, &component.element.ty)?;
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

fn parse_builtin_type(
    oid: &ModuleIdentifier,
    builtin: &AstElement<AstBuiltinType>,
) -> Result<BuiltinType> {
    Ok(match &builtin.element {
        AstBuiltinType::ObjectIdentifier(_) => BuiltinType::ObjectIdentifier,
        AstBuiltinType::BitString(bit_string) => BuiltinType::BitString(BitStringType {
            named_bits: None, // TODO
            size_constraints: match &bit_string.element.size_constraints {
                Some(sc) => Some(parse_constraints(
                    oid,
                    ConstraintsKind::Size,
                    &sc.element.0,
                )?),
                None => None,
            },
        }),
        AstBuiltinType::OctetString(octet_string) => BuiltinType::OctetString(OctetStringType {
            size_constraints: match &octet_string.element.size_constraints {
                Some(sc) => Some(parse_constraints(
                    oid,
                    ConstraintsKind::Size,
                    &sc.element.0,
                )?),
                None => None,
            },
        }),
        AstBuiltinType::Integer(integer) => BuiltinType::Integer(IntegerType {
            named_values: None, // TODO
            value_constraints: match &integer.element.value_constraints {
                Some(sc) => Some(parse_constraints(oid, ConstraintsKind::Value, sc)?),
                None => None,
            },
        }),
        AstBuiltinType::Sequence(sequence) => BuiltinType::Sequence(Structure {
            ty: TagType::Sequence,
            components: parse_structure_components(oid, &sequence.element.components)?,
        }),
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
                .unwrap()
                .resolve_symbol(&typeref.element.0),
            typeref.loc,
        )),
    })
}

fn parse_type(oid: &ModuleIdentifier, ty: &AstElement<AstType>) -> Result<TaggedType> {
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
            let kind = match tagged_type.element.kind {
                Some(ref kind) => match kind.element {
                    AstTagKind::TagKindExplicit(_) => TagKind::Explicit,
                    AstTagKind::TagKindImplicit(_) => TagKind::Implicit,
                },
                None => match context().lookup_module(oid).unwrap().tag_default {
                    // TODO: should Automatic mean Implicit?
                    TagDefault::Automatic | TagDefault::Implicit => TagKind::Implicit,
                    TagDefault::Explicit => TagKind::Explicit,
                },
            };
            let tag = Tag::new(class, tag_number, kind);
            TaggedType { tag, ty }
        }
        AstType::UntaggedType(untagged) => TaggedType {
            tag: Tag::default(),
            ty: parse_untagged_type(oid, untagged)?,
        },
    })
}

fn register_type(
    oid: &ModuleIdentifier,
    type_assignment: &AstElement<AstTypeAssignment>,
) -> Result<()> {
    let name = type_assignment.element.name.element.0.clone();
    let ty = parse_type(oid, &type_assignment.element.ty)?;

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
        let oid = module_ast_to_module_ident(&header)?;

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
) -> AstElement<valref!()> {
    valref.as_ref().map(|valref| {
        ValueReference::Reference(
            context()
                .lookup_module(oid)
                .unwrap()
                .resolve_symbol(&valref.0),
        )
    })
}

fn parse_integer_value(num: &AstElement<AstIntegerValue>) -> Result<Value> {
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
        Ok(Value::Integer(uint as i64))
    } else {
        let int = num.element.value.element.0;
        if int > i64::MAX as u64 {
            return Err(Error {
                kind: ErrorKind::Ast(format!(
                    "number -{} too small for a 64-bit signed integer",
                    int,
                )),
                loc: num.loc,
            });
        }
        Ok(Value::Integer(-(int as i64)))
    }
}

fn parse_sequence_value(
    oid: &ModuleIdentifier,
    seq_val: &AstElement<AstSequenceValue>,
    target_type: &ResolvedType,
) -> Result<Value> {
    let seq_ty_components = match &target_type.ty {
        BuiltinType::Sequence(seq_ty) => &seq_ty.components,
        ty => {
            return Err(Error {
                kind: ErrorKind::Ast(format!(
                    "SEQUENCE value cannot be assigned to {} type",
                    ty.to_string()
                )),
                loc: seq_val.loc,
            })
        }
    };
    for val_component in &seq_val.element.components {
        if seq_ty_components
            .iter()
            .find(|ty_component| ty_component.name.element == val_component.element.name.element.0)
            .is_none()
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
    for ty_component in seq_ty_components {
        let value = {
            if let Some(val_component) = seq_val.element.components.iter().find(|val_component| {
                val_component.element.name.element.0 == ty_component.name.element
            }) {
                let component_type = ty_component.component_type.resolve()?;
                parse_value(oid, &val_component.element.value, &component_type)?
            } else {
                if let Some(default_value) = &ty_component.default_value {
                    (**default_value).clone()
                } else {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "SEQUENCE value missing component '{}' of type {}",
                            ty_component.name.element,
                            ty_component.component_type.to_string(),
                        )),
                        loc: seq_val.loc,
                    });
                }
            }
        };

        components.push(SequenceValueComponent {
            name: ty_component.name.clone(),
            value,
        });
    }
    Ok(Value::Sequence(SequenceValue { components }))
}

fn parse_value(
    oid: &ModuleIdentifier,
    value: &AstElement<AstValue>,
    target_type: &ResolvedType,
) -> Result<AstElement<valref!()>> {
    Ok(AstElement::new(
        match value.element {
            AstValue::BuiltinValue(ref builtin) => ValueReference::Value(match &builtin.element {
                AstBuiltinValue::StringLiteral(ref str_lit) => match &target_type.ty {
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
                        Value::BitString(
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
                        Value::OctetString(
                            BigUint::parse_bytes(str_lit.element.data.as_bytes(), radix)
                                .expect("failed BigUint::parse_bytes")
                                .to_bytes_be(),
                        )
                    }
                    other_type => {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "{} value cannot be assigned to {}",
                                str_lit.element.kind.to_string(),
                                other_type.to_string()
                            )),
                            loc: builtin.loc,
                        })
                    }
                },
                AstBuiltinValue::ObjectIdentifierValue(object_id) => {
                    Value::ObjectIdentifier(parse_object_identifier(oid, object_id)?)
                }
                AstBuiltinValue::IntegerValue(num) => parse_integer_value(num)?,
                AstBuiltinValue::SequenceValue(seq_val) => {
                    parse_sequence_value(oid, seq_val, target_type)?
                }
                other_builtin => todo!("{:#?}", other_builtin),
            }),
            AstValue::ValueReference(ref valref) => parse_valuereference(oid, valref).element,
        },
        value.loc,
    ))
}

fn register_value(
    oid: &ModuleIdentifier,
    value_assignment: &AstElement<AstValueAssignment>,
) -> Result<()> {
    let name = value_assignment.element.name.element.0.clone();
    let ty = parse_type(oid, &value_assignment.element.ty)?;
    let resolved_ty = ty.resolve()?;
    let val = parse_value(oid, &value_assignment.element.value, &resolved_ty)?;

    context_mut().register_value(
        QualifiedIdentifier {
            module: oid.clone(),
            name,
        },
        DeclaredValue { value: val, ty: ty },
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

fn verify_value(declared_value: &DeclaredValue) -> Result<()> {
    let resolved_ty = declared_value.ty.resolve()?;
    resolved_ty
        .ty
        .ensure_satisfied_by_value(&declared_value.value)?;

    Ok(())
}

/// Fourth stage: verify all declared values.
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
