use num::BigUint;

use super::{
    context::{context, DeclaredValue},
    context_mut,
    parser::*,
};
use crate::{module::*, types::*, values::*};

fn lookup_oid_component_str(component: &str) -> Option<u64> {
    Some(match component {
        "itu-t" => 0,
        "iso" => 1,
        "joint-iso-itu-t" => 2,
        _ => return None,
    })
}

fn lookup_oid_component(val_ref: &AstElement<AstValueReference>) -> Result<u64> {
    lookup_oid_component_str(&val_ref.element.0).ok_or_else(|| Error {
        kind: ErrorKind::Ast(format!(
            "unknown named oid component '{}'",
            val_ref.element.0
        )),
        loc: val_ref.loc,
    })
}

fn symbol_list_to_string_list(symbol_list: &AstSymbolList) -> Vec<String> {
    symbol_list
        .0
        .iter()
        .map(|symbol| {
            match match &symbol.element {
                AstSymbol::ParameterizedReference(parameterized) => {
                    parameterized.element.reference.element.clone()
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
//         oid: match module_ref.id {
//             Some(ref id) => Some(ObjectIdentifier(match id.element {
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

fn module_ast_to_module_ident(module: &AstElement<AstModule>) -> Result<ModuleIdentifier> {
    name_and_oid_to_module_ident(&module.element.name, module.element.id.as_ref())
}

fn module_ref_to_module_ident(
    module_ref: &AstElement<AstGlobalModuleReference>,
) -> Result<ModuleIdentifier> {
    name_and_oid_to_module_ident(&module_ref.element.name, module_ref.element.id.as_ref())
}

fn name_and_oid_to_module_ident(
    name: &AstElement<AstTypeReference>,
    oid: Option<&AstElement<AstDefinitiveOid>>,
) -> Result<ModuleIdentifier> {
    Ok(ModuleIdentifier {
        name: name.element.0.clone(),
        oid: match oid {
            Some(oid) => Some(Oid(oid
                .element
                .0
                .iter()
                .map(|elem| {
                    Ok(match &elem.element {
                        AstDefinitiveOidComponent::Number(num) => num.element.0,
                        AstDefinitiveOidComponent::NamedNumber(named_num) => {
                            named_num.element.num.element.0
                        }
                        AstDefinitiveOidComponent::ValueReference(val_ref) => {
                            lookup_oid_component(val_ref)?
                        }
                    })
                })
                .collect::<Result<Vec<u64>>>()?)),
            None => None,
        },
    })
}

fn parse_object_identifier_component_valref(
    id: &ModuleIdentifier,
    value: &AstElement<AstValueReference>,
) -> ObjectIdentifierComponent {
    lookup_oid_component_str(&value.element.0).map_or_else(
        || ObjectIdentifierComponent::ValueReference(parse_valuereference(id, value)),
        ObjectIdentifierComponent::IntegerLiteral,
    )
}

fn parse_object_identifier(
    id: &ModuleIdentifier,
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
                        ObjectIdentifierComponent::IntegerLiteral(num.element.0)
                    }
                    AstObjectIdentifierComponent::NamedNumber(named_num) => {
                        ObjectIdentifierComponent::IntegerLiteral(named_num.element.num.element.0)
                    }
                    AstObjectIdentifierComponent::ValueReference(val_ref) => {
                        parse_object_identifier_component_valref(id, val_ref)
                    }
                    AstObjectIdentifierComponent::DefinedValue(defined_val) => {
                        parse_object_identifier_component_valref(id, &defined_val.element.value)
                    }
                })
            })
            .collect::<Result<Vec<ObjectIdentifierComponent>>>()?,
    ))
}

/// First stage: register all module headers.
pub fn register_all_modules(program: &AstElement<AstProgram>) -> Result<()> {
    for module in &program.element.0 {
        let id = module_ast_to_module_ident(module)?;
        let tag_default = match module
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
        let extensibility_implied = module.element.extension_default.is_some();
        let exports = match module
            .element
            .body
            .element
            .exports
            .as_ref()
            .map(|exports| &exports.element.0.element)
        {
            Some(exports) => match exports {
                AstExportsKind::All(_) => Exports::All,
                AstExportsKind::SymbolList(symbol_list) => {
                    Exports::SymbolList(symbol_list_to_string_list(&symbol_list.element))
                }
            },
            None => Exports::All,
        };
        let mut imports = Vec::new();
        if let Some(ref ast_imports) = module.element.body.element.imports {
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
            id,
            tag_default,
            extensibility_implied,
            exports,
            imports,
        });
    }

    Ok(())
}

fn parse_constraints(_constraints: &AstElement<AstConstraints>) -> Constraints {
    // TODO
    Vec::new()
}

fn parse_structure_components(
    id: &ModuleIdentifier,
    components: &Vec<AstElement<AstStructureComponent>>,
) -> Result<Vec<StructureComponent>> {
    components
        .iter()
        .map(|component| {
            let ty = parse_type(id, &component.element.ty)?;
            let resolved_ty = ty.resolve().map_err(|err| Error {
                kind: ErrorKind::Ast(format!(
                    "failed to resolve component '{}' type '{}': {}",
                    component.element.name.element.0,
                    ty.to_string(),
                    err,
                )),
                loc: component.element.ty.loc,
            })?;
            Ok(StructureComponent {
                name: component.element.name.element.0.clone(),
                default_value: match component
                    .element
                    .default
                    .as_ref()
                    .map(|default| Ok(Box::new(parse_value(id, default, resolved_ty)?)))
                {
                    Some(result) => Some(result?),
                    None => None,
                },
                optional: false,
                component_type: Box::new(ty),
            })
        })
        .collect::<Result<Vec<StructureComponent>>>()
}

fn parse_builtin_type(id: &ModuleIdentifier, builtin: &AstElement<AstBuiltinType>) -> Result<Type> {
    Ok(match &builtin.element {
        AstBuiltinType::ObjectIdentifier(_) => Type::ObjectIdentifier,
        AstBuiltinType::BitString(bit_string) => Type::BitString(BitStringType {
            named_bits: None, // TODO
            size_constraints: bit_string
                .element
                .size_constraints
                .as_ref()
                .map(|size_constraints| parse_constraints(&size_constraints.element.0)),
        }),
        AstBuiltinType::OctetString(octet_string) => Type::OctetString(OctetStringType {
            size_constraints: octet_string
                .element
                .size_constraints
                .as_ref()
                .map(|size_constraints| parse_constraints(&size_constraints.element.0)),
        }),
        AstBuiltinType::Integer(integer) => Type::Integer(IntegerType {
            named_values: None, // TODO
            value_constraints: integer
                .element
                .value_constraints
                .as_ref()
                .map(|value_constraints| parse_constraints(value_constraints)),
        }),
        AstBuiltinType::Sequence(sequence) => Type::Sequence(Structure {
            ty: TagType::Sequence,
            components: parse_structure_components(id, &sequence.element.components)?,
        }),
    })
}

fn parse_type(id: &ModuleIdentifier, ty: &AstElement<AstType>) -> Result<typeref!()> {
    Ok(match ty.element {
        AstType::TaggedType(ref tagged_type) => {
            let ty = parse_builtin_type(id, &tagged_type.element.ty)?;
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
            let tag = Tag::new(class, tag_number);
            let kind = match tagged_type.element.kind {
                Some(ref kind) => match kind.element {
                    AstTagKind::TagKindExplicit(_) => TagKind::Explicit,
                    AstTagKind::TagKindImplicit(_) => TagKind::Implicit,
                },
                None => match context().lookup_module(id).unwrap().tag_default {
                    // TODO: should Automatic mean Implicit?
                    TagDefault::Automatic | TagDefault::Implicit => TagKind::Implicit,
                    TagDefault::Explicit => TagKind::Explicit,
                },
            };
            TypeReference::Type(TaggedType { tag, kind, ty })
        }
        AstType::BuiltinType(ref builtin) => TypeReference::Type(TaggedType {
            tag: Tag::default(),
            kind: TagKind::Implicit,
            ty: parse_builtin_type(id, builtin)?,
        }),
        AstType::TypeReference(ref typeref) => TypeReference::Reference(
            context()
                .lookup_module(id)
                .unwrap()
                .resolve_symbol(&typeref.element.0),
        ),
    })
}

fn register_type(
    id: &ModuleIdentifier,
    type_assignment: &AstElement<AstTypeAssignment>,
) -> Result<()> {
    let name = type_assignment.element.name.element.0.clone();
    let ty = parse_type(id, &type_assignment.element.ty)?;

    context_mut().register_type(
        QualifiedIdentifier {
            module: id.clone(),
            name,
        },
        ty,
    );

    Ok(())
}

/// Second stage: register all declared types.
pub fn register_all_types(program: &AstElement<AstProgram>) -> Result<()> {
    for module in &program.element.0 {
        let id = module_ast_to_module_ident(module)?;

        for assignment in &module.element.body.element.assignments {
            if let AstAssignment::TypeAssignment(ref type_assignment) = assignment.element {
                register_type(&id, type_assignment)?;
            }
        }
    }

    Ok(())
}

fn parse_valuereference(
    id: &ModuleIdentifier,
    valref: &AstElement<AstValueReference>,
) -> valref!() {
    ValueReference::Reference(
        context()
            .lookup_module(id)
            .unwrap()
            .resolve_symbol(&valref.element.0),
    )
}

fn parse_value(
    id: &ModuleIdentifier,
    value: &AstElement<AstValue>,
    target_type: &TaggedType,
) -> Result<valref!()> {
    Ok(match value.element {
        AstValue::BuiltinValue(ref builtin) => ValueReference::Value(match &builtin.element {
            AstBuiltinValue::StringLiteral(ref str_lit) => match &target_type.ty {
                Type::BitString(_) => {
                    let radix = match str_lit.element.kind {
                        StringKind::BString => 2,
                        StringKind::HString => 16,
                        StringKind::CString => {
                            return Err(Error {
                                kind: ErrorKind::Ast(
                                    "cstring value cannot be assigned to BIT STRING".to_string(),
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
                Type::OctetString(_) => {
                    let radix = match str_lit.element.kind {
                        StringKind::BString => 2,
                        StringKind::HString => 16,
                        StringKind::CString => {
                            return Err(Error {
                                kind: ErrorKind::Ast(
                                    "cstring value cannot be assigned to OCTET STRING".to_string(),
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
                Value::ObjectIdentifier(parse_object_identifier(id, object_id)?)
            }
            AstBuiltinValue::IntegerValue(num) => {
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
                    Value::Integer(uint as i64)
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
                    Value::Integer(-(int as i64))
                }
            }
            AstBuiltinValue::SequenceValue(seq_val) => {
                let seq_ty_components = match &target_type.ty {
                    Type::Sequence(seq_ty) => &seq_ty.components,
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
                        .find(|ty_component| {
                            ty_component.name == val_component.element.name.element.0
                        })
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
                Value::Sequence(SequenceValue {
                        components: seq_ty_components
                            .iter()
                            .map(|ty_component| {
                                Ok(SequenceValueComponent {
                                    name: ty_component.name.clone(),
                                    value: {
                                        if let Some(val_component) = seq_val.element.components
                                            .iter()
                                            .find(|val_component| val_component.element.name.element.0 == ty_component.name)
                                        {
                                            let component_type = ty_component.component_type.resolve().map_err(|err| Error{
                                                kind: ErrorKind::Ast(format!("failed resolving type '{}' of component '{}': {}", ty_component.component_type.to_string(), val_component.element.name.element.0, err)),
                                                loc: val_component.loc,
                                            })?;
                                            parse_value(id, &val_component.element.value, component_type)?
                                        } else {
                                            return Err(Error {
                                                kind: ErrorKind::Ast(format!(
                                                    "SEQUENCE value missing component '{}' of type {}",
                                                    ty_component.name,
                                                    ty_component.component_type.to_string(),
                                                )),
                                                loc: seq_val.loc,
                                            });
                                        }
                                    },
                                })
                            })
                            .collect::<Result<Vec<SequenceValueComponent>>>()?,
                    })
            }
            other_builtin => todo!("{:#?}", other_builtin),
        }),
        AstValue::ValueReference(ref valref) => parse_valuereference(id, valref),
    })
}

fn register_value(
    id: &ModuleIdentifier,
    value_assignment: &AstElement<AstValueAssignment>,
) -> Result<()> {
    let name = value_assignment.element.name.element.0.clone();
    let ty = parse_type(id, &value_assignment.element.ty)?;
    let resolved_ty = ty.resolve().map_err(|err| Error {
        kind: ErrorKind::Ast(format!(
            "failed to resolve type '{}' ({})",
            ty.to_string(),
            err
        )),
        loc: value_assignment.element.ty.loc,
    })?;
    let val = parse_value(id, &value_assignment.element.value, resolved_ty)?;

    context_mut().register_value(
        QualifiedIdentifier {
            module: id.clone(),
            name,
        },
        DeclaredValue { value: val, ty },
    );

    Ok(())
}

/// Third stage: register all declared values.
pub fn register_all_values(program: &AstElement<AstProgram>) -> Result<()> {
    for module in &program.element.0 {
        let id = module_ast_to_module_ident(module)?;

        for assignment in &module.element.body.element.assignments {
            if let AstAssignment::ValueAssignment(ref value_assignment) = assignment.element {
                register_value(&id, value_assignment)?;
            }
        }
    }

    Ok(())
}
