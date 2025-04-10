use std::fmt::Write;

use num::{bigint::Sign, BigInt};

use crate::{
    compiler::{context::DeclaredValue, parser::*},
    module::*,
    types::*,
    values::*,
};

use super::{
    class,
    constraints::{self, apply_pending_constraint},
    object_id, types, AstParser,
};

lazy_static::lazy_static! {
    static ref PLUS_INFINITY_IDENT: QualifiedIdentifier = QualifiedIdentifier::new(
        ModuleIdentifier::with_name(String::from("Real")),
        String::from("plus-infinity"),
    );
    static ref MINUS_INFINITY_IDENT: QualifiedIdentifier = QualifiedIdentifier::new(
        ModuleIdentifier::with_name(String::from("Real")),
        String::from("minus-infinity"),
    );
    static ref NOT_A_NUMBER_IDENT: QualifiedIdentifier = QualifiedIdentifier::new(
        ModuleIdentifier::with_name(String::from("Real")),
        String::from("not-a-number"),
    );
}

pub fn parse_integer_value(num: &AstElement<AstIntegerValue>) -> Result<BigInt> {
    let sign = if num.element.sign.is_none() {
        Sign::Plus
    } else {
        Sign::Minus
    };

    Ok(BigInt::from_biguint(
        sign,
        num.element.value.element.0.clone(),
    ))
}

pub fn parse_decimal_value(dec: &AstElement<AstDecimalValue>) -> Result<RealLiteral> {
    let sign = if dec.element.sign.is_none() {
        Sign::Plus
    } else {
        Sign::Minus
    };

    let whole = BigInt::from_biguint(sign, dec.element.whole.element.0.clone());
    let ufraction = dec.element.fraction.element.0.clone();
    let fraction_len = ufraction.to_str_radix(10).len() as u32;
    let fraction = BigInt::from_biguint(sign, ufraction);
    let mantissa = whole * BigInt::from(10u64).pow(fraction_len) + fraction;

    Ok(RealLiteral {
        mantissa,
        exponent: BigInt::from_biguint(Sign::Minus, fraction_len.into()),
    })
}

fn parse_object_class_field<'a, 'b>(
    parser: &'a AstParser<'_>,
    component_type: &'b TaggedType,
    class_type: &AstElement<QualifiedIdentifier>,
    field: &AstElement<String>,
) -> Result<(&'a ObjectClassField, Option<&'b TableConstraint>)> {
    let class =
        class::resolve_information_object_class(parser, class_type)?.resolve(parser.context)?;
    let field = class.find_field(field)?;

    let table_constraint = component_type.constraints.as_ref().and_then(|constraints| {
        constraints
            .iter()
            .find_map(|constraint| constraint.get_table_constraint())
    });

    Ok(match table_constraint {
        Some(table_constraint) => (field, Some(table_constraint)),
        None => (field, None),
    })
}

fn find_matching_object_in_set<'a>(
    parser: &'a AstParser<'_>,
    set: &'a InformationObjectSet,
    primary_key: &AstElement<String>,
    primary_key_value: &AstElement<TypedValue>,
) -> Result<Option<&'a InformationObject>> {
    for element in set {
        let object = match element {
            ObjectSetElement::Object(object) => object.resolve(parser.context)?,
            ObjectSetElement::ObjectFieldReference(ofr) => {
                let object = resolve_information_object(parser, &ofr.object_ref)?;
                let field = object.find_field(&ofr.field)?;
                match field {
                    ObjectField::Object(object) => object.resolve(parser.context)?,
                    other => {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "expecting object field reference, found {} field reference",
                                other.get_name()
                            )),
                            loc: ofr.field.loc,
                        })
                    }
                }
            }
            ObjectSetElement::ObjectSet(set) => {
                let set = class::resolve_information_object_set(parser, set)?;
                match find_matching_object_in_set(parser, set, primary_key, primary_key_value)? {
                    Some(object) => return Ok(Some(object)),
                    None => continue,
                }
            }
        };

        let key_field = object.fields.iter().find_map(|(name, field)| {
            if name == &primary_key.element {
                Some(field)
            } else {
                None
            }
        });
        if let Some(key_field) = key_field {
            let value = match key_field {
                ObjectField::Value(value) => value,
                ObjectField::ObjectFieldReference(ofr) => {
                    let object = resolve_information_object(parser, &ofr.object_ref)?;

                    match object.find_field(&ofr.field)? {
                        ObjectField::Value(value) => value,
                        _ => {
                            return Err(Error {
                                kind: ErrorKind::Ast(format!(
                                    "field '{}' is not a value field",
                                    ofr.field.element
                                )),
                                loc: ofr.field.loc,
                            })
                        }
                    }
                }
                ObjectField::Type(_) => {
                    todo!("open types referencing open types are not supported; should they be?")
                }
                ObjectField::Object(_) => {
                    todo!("open types referencing objects are not supported; should they be?")
                }
                ObjectField::ObjectSet(_) => {
                    todo!("open types referencing object sets are not supported; should they be?")
                }
                ObjectField::ValueSet(_) => {
                    todo!("open types referencing value sets are not supported; should they be?")
                }
            };
            if primary_key_value.try_eq(parser.context, value)? {
                return Ok(Some(object));
            }
        }
    }

    Ok(None)
}

fn find_component_from_series(
    parser: &AstParser<'_>,
    structures: (&[NamedStructureComponent], &[StructureValueComponent]),
    component_series: &[AstElement<String>],
) -> Result<(TaggedType, AstElement<TypedValue>)> {
    if component_series.is_empty() {
        panic!("component_series.len() == 0");
    }

    let (structure, structure_value) = structures;

    let search_component = &component_series[0];
    for (component, component_value) in structure.iter().zip(structure_value) {
        if component.name().element == search_component.element {
            let is_search_terminal = component_series.len() == 1;
            if let UntaggedType::BuiltinType(BuiltinType::Structure(structure)) =
                &component.component_type().ty
            {
                if is_search_terminal {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "component reference cannot be to a {}",
                            structure.ty
                        )),
                        loc: search_component.loc,
                    });
                } else {
                    let structure_value =
                        match component_value.value().resolve(parser.context)?.value {
                            BuiltinValue::Structure(_, structure) => structure,
                            _ => unreachable!(),
                        };
                    return find_component_from_series(
                        parser,
                        (
                            &structure.resolve_components(parser.context)?,
                            &structure_value.components,
                        ),
                        &component_series[1..],
                    );
                }
            }
            let resolved_type = component.component_type().resolve(parser.context)?;
            match &resolved_type.ty {
                ty @ (BuiltinType::Structure(_)
                | BuiltinType::StructureOf(_)
                | BuiltinType::Choice(_)) => {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!("component reference cannot be to a {}", ty)),
                        loc: search_component.loc,
                    });
                }
                _ => {
                    if is_search_terminal {
                        return Ok((
                            component.component_type().clone(),
                            component_value.value().clone(),
                        ));
                    } else {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "component series terminated early; expecting '{}' to be an inline SEQUENCE or SET",
                                search_component.element,
                            )),
                            loc: search_component.loc,
                        });
                    }
                }
            }
        }
    }

    Err(Error {
        kind: ErrorKind::Ast(format!("no such component '{}'", search_component.element)),
        loc: search_component.loc,
    })
}

fn parse_object_class_field_reference<C: ComponentLike>(
    parser: &AstParser<'_>,
    struct_component: &C,
    value_loc: Loc,
    ocf: &ObjectClassFieldReference,
    // If the top-level type is a SEQUENCE or SET type, `root_container` represents the components of that structure.
    // Otherwise, if the root container is a CHOICE, `root_container` is None.
    root_container: Option<(&[NamedStructureComponent], &[StructureValueComponent])>,
    // If the reference-type field is within a SEQUENCE or SET type, `container` represents the components of that structure.
    // Otherwise, if the container is a CHOICE, `container` is None.
    container: Option<(&[NamedStructureComponent], &[StructureValueComponent])>,
) -> Result<(ResolvedType, ObjectClassFieldReferenceKind)> {
    let (field, table_constraint) = parse_object_class_field(
        parser,
        struct_component.component_type(),
        &ocf.class_type,
        &ocf.field,
    )?;

    match (field, ocf.kind.clone()) {
        (ObjectClassField::Value(value_field), kind @ ObjectClassFieldReferenceKind::ValueLike) => {
            let mut value_type = match &value_field.field_type {
                ObjectClassFieldValueType::TaggedType(tagged_type) => tagged_type.resolve(parser.context)?,
                ObjectClassFieldValueType::OpenTypeReference(_) => todo!("value fields referencing open type fields are not yet supported"),
                ObjectClassFieldValueType::ObjectClassFieldReference(_) => todo!("value fields referencing other object class value fields are not (yet?) supported"),
            };
            value_type.tag = struct_component.component_type().tag.clone().or(value_type.tag);
            Ok((value_type, kind))
        }
        (ObjectClassField::OpenType(_), kind @ ObjectClassFieldReferenceKind::TypeLike) => {
            match table_constraint {
                Some(table_constraint) => {
                    match &table_constraint.component_ref {
                        Some(ast_component_ref) => {
                            let set = class::resolve_information_object_set(parser, &table_constraint.set_ref)?;

                            let (component_type, component_value) = if ast_component_ref.is_relative {
                                match container {
                                    Some(container) => find_component_from_series(parser, container, &ast_component_ref.component_series)?,
                                    None => return Err(Error {
                                        kind: ErrorKind::Ast("relative component references are illegal when the containing type is not SEQUENCE or SET".to_string()),
                                        loc: ast_component_ref.component_series[0].loc
                                    })
                                }
                            } else {
                                match root_container {
                                    Some(root_container) => find_component_from_series(parser, root_container, &ast_component_ref.component_series)?,
                                    None => return Err(Error {
                                        kind: ErrorKind::Ast("absolute component references are illegal when the root type is not SEQUENCE or SET".to_string()),
                                        loc: ast_component_ref.component_series[0].loc
                                    })
                                }
                            };

                            let component_ref_type = match &component_type.ty {
                                UntaggedType::ObjectClassField(ocf) => ocf,
                                _ => {
                                    let terminal_component = ast_component_ref.component_series.last().unwrap();
                                    return Err(Error {
                                        kind: ErrorKind::Ast(format!(
                                            "referenced component '{}' is not an information object class field reference",
                                            terminal_component.element,
                                        )),
                                        loc: terminal_component.loc,
                                    })
                                }
                            };

                            match find_matching_object_in_set(parser, set, &component_ref_type.field, &component_value)? {
                                Some(object) => {
                                    let type_field = object.fields.iter().find_map(|(name, field)| if name == &ocf.field.element {
                                        Some(field)
                                    } else {
                                        None
                                    });
                                    if let Some(type_field) = type_field {
                                        match type_field {
                                            ObjectField::Type(tagged_type) => {
                                                let inner_type = tagged_type.resolve(parser.context)?;
                                                if let Some(mut outer_tag) = struct_component.component_type().tag.clone() {
                                                    match &mut outer_tag.kind {
                                                        TagKind::Explicit(inner_tag) => {
                                                            *inner_tag = inner_type.tag.clone().map(|tag| (tag.class, tag.num));
                                                            Ok((ResolvedType {
                                                                tag: Some(outer_tag),
                                                                ty: inner_type.ty,
                                                                constraints: inner_type.constraints,
                                                            }, kind))
                                                        }
                                                        TagKind::Implicit => {
                                                            Ok((ResolvedType {
                                                                tag: struct_component.component_type().tag.clone(),
                                                                ty: inner_type.ty,
                                                                constraints: inner_type.constraints,
                                                            }, kind))
                                                        }
                                                    }
                                                } else {
                                                    Ok((inner_type, kind))
                                                }
                                            }
                                            _ => unreachable!(),
                                        }
                                    } else {
                                        Err(Error {
                                            kind: ErrorKind::Ast(format!(
                                                "class in set '{}' with field '{}' equal to {} does not define the field '{}'",
                                                table_constraint.set_ref.element,
                                                component_ref_type.field.element,
                                                component_value.resolve(parser.context)?.value,
                                                struct_component.name().element,
                                            )),
                                            loc: value_loc,
                                        })
                                    }
                                }
                                None => {
                                    Err(Error {
                                        kind: ErrorKind::Ast(format!(
                                            "there is no class in set '{}' with field '{}' equal to {}, and thus the open type for component '{}' could not be determined",
                                            table_constraint.set_ref.element,
                                            component_ref_type.field.element,
                                            component_value.resolve(parser.context)?.value,
                                            struct_component.name().element,
                                        )),
                                        loc: value_loc,
                                    })
                                }
                            }
                        }
                        None => {
                            todo!("open type with table reference but no field reference")
                        }
                    }
                }
                None => {
                    let mut base_type = ResolvedType::universal(TagType::Any);
                    base_type.tag = struct_component.component_type().tag.clone();
                    Ok((base_type, kind))
                }
            }
        }
        (ObjectClassField::ValueSet(value_set), ObjectClassFieldReferenceKind::TypeLike) => {
            Ok((value_set.field_type.resolve(parser.context)?, ObjectClassFieldReferenceKind::ValueLike))
        }
        (a, b) => panic!("mismatch between field reference kind and field definition kind; this should never happen\na = {:#?}\nb={:#?}", a, b),
    }
}

fn validate_class_field_reference_value(
    value: &AstElement<AstValue>,
    kind: ObjectClassFieldReferenceKind,
) -> Result<()> {
    if kind == ObjectClassFieldReferenceKind::TypeLike {
        let is_open_type_value = match &value.element {
            AstValue::BuiltinValue(builtin) => {
                matches!(&builtin.element, AstBuiltinValue::OpenTypeValue(_))
            }
            _ => false,
        };
        if !is_open_type_value {
            return Err(Error {
                kind: ErrorKind::Ast("expecting open type value".to_string()),
                loc: value.loc,
            });
        }
    }

    Ok(())
}

fn parse_structure_value(
    parser: &AstParser<'_>,
    stage: ParseValueAssignmentStage,
    struct_val: &AstElement<AstStructureValue>,
    target_type: &ResolvedType,
    root_container: Option<(&[NamedStructureComponent], &[StructureValueComponent])>,
) -> Result<BuiltinValue> {
    let tag_type = target_type.ty.tag_type().expect("tag_type");
    let struct_ty_components = match &target_type.ty {
        BuiltinType::Structure(ty) => &ty.resolve_components(parser.context)?,
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
                let component_type = match &ty_component.component_type.ty {
                    UntaggedType::ObjectClassField(ocf) => {
                        if stage != ParseValueAssignmentStage::ClassReference {
                            return Err(Error::break_parser());
                        }

                        let (resolved_type, kind) = parse_object_class_field_reference(
                            parser,
                            ty_component,
                            val_component.loc,
                            ocf,
                            root_container.or_else(|| Some((struct_ty_components, &components))),
                            Some((struct_ty_components, &components)),
                        )?;
                        validate_class_field_reference_value(&val_component.element.value, kind)?;
                        resolved_type
                    }
                    _ => ty_component.component_type.resolve(parser.context)?,
                };
                (
                    parse_value(parser, stage, &val_component.element.value, &component_type)?,
                    false,
                )
            } else if let Some(default_value) = &ty_component.default_value {
                (
                    default_value.parse(
                        parser,
                        &ty_component.component_type.resolve(parser.context)?,
                    )?,
                    true,
                )
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
    Ok(BuiltinValue::Structure(
        tag_type,
        StructureValue { components },
    ))
}

fn parse_character_string(
    str_lit: &AstElement<AstStringLiteral>,
    tag_type: TagType,
) -> Result<BuiltinValue> {
    let cstring = match &str_lit.element.kind {
        StringKind::C => str_lit.element.data.clone(),
        _ => {
            return Err(Error {
                kind: ErrorKind::Ast(format!(
                    "{} value cannot be assigned to {}",
                    str_lit.element.kind, tag_type
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
        TagType::GraphicString | TagType::ObjectDescriptor => {
            |ch: char| ch == ' ' || !ch.is_control()
        }
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

fn parse_byte_string(str: &str, radix: u32) -> (Vec<u8>, u8) {
    if str.is_empty() {
        return (Vec::new(), 0);
    }

    let chars_per_byte = match radix {
        16 => 2,
        2 => 8,
        _ => unreachable!(),
    };
    let mut bytes = Vec::with_capacity(str.len() / chars_per_byte + 1);
    let mut char_buf = String::with_capacity(chars_per_byte);
    for i in (0..str.len()).step_by(chars_per_byte) {
        char_buf.clear();

        let segment = &str[i..usize::min(str.len(), i + chars_per_byte)];
        for char in segment.chars() {
            char_buf.write_char(char).unwrap();
        }
        let unused_bits = if char_buf.len() < chars_per_byte {
            let unused_chars = chars_per_byte - char_buf.len();
            while char_buf.len() < chars_per_byte {
                char_buf.write_char('0').unwrap();
            }
            (match radix {
                16 => unused_chars * 4,
                2 => unused_chars,
                _ => unreachable!(),
            }) as u8
        } else {
            0
        };
        bytes.push(
            u8::from_str_radix(&char_buf, radix)
                .expect("invalid bstring or hstring value (this should be caught by the parser)"),
        );
        if unused_bits > 0 {
            return (bytes, unused_bits);
        }
    }

    (bytes, 0)
}

fn create_named_bit_string(mut bit_positions: Vec<u64>) -> BitStringValue {
    if bit_positions.is_empty() {
        return BitStringValue {
            data: Vec::new(),
            unused_bits: 0,
        };
    }

    bit_positions.sort();

    let max_bit = *bit_positions.last().unwrap();
    let unused_bits = match 8 - (max_bit + 1) % 8 {
        8 => 0,
        unused_bits => unused_bits,
    };
    let mut data = vec![0u8; ((max_bit + unused_bits + 1) / 8) as usize];
    for bit_position in bit_positions {
        let byte_offset = bit_position / 8;
        if byte_offset == 0 {
            data[0] |= (1 << (7 - bit_position)) as u8;
        } else {
            let bit_offset = bit_position % byte_offset;
            data[byte_offset as usize] |= (1 << (7 - bit_offset)) as u8;
        }
    }

    BitStringValue {
        data,
        unused_bits: unused_bits as u8,
    }
}

fn parse_named_bit_string(
    parser: &AstParser<'_>,
    bit_string: &BitStringType,
    ast_bits: &AstElement<AstNamedBitStringValue>,
) -> Result<BitStringValue> {
    let named_bits = match &bit_string.named_bits {
        Some(named_bits) => named_bits,
        None => {
            return Err(Error {
                kind: ErrorKind::Ast(
                    "the governing BIT STRING type does not define any named bits".to_string(),
                ),
                loc: ast_bits.loc,
            })
        }
    };
    let named_bits = named_bits
        .iter()
        .map(|named_bit| {
            Ok((
                named_bit.name.element.clone(),
                match &named_bit.value.resolve(parser.context)?.value {
                    BuiltinValue::Integer(i) => {
                        if i.sign() == Sign::Minus {
                            return Err(Error {
                                kind: ErrorKind::Ast(
                                    "named bit value must be a positive INTEGER".to_string(),
                                ),
                                loc: named_bit.value.loc,
                            });
                        }
                        let uint: u64 = i.try_into().map_err(|_| Error {
                            kind: ErrorKind::Ast(
                                "named bit value cannot be larger than 2^64 - 1".to_string(),
                            ),
                            loc: named_bit.value.loc,
                        })?;
                        uint
                    }
                    other => {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "named bit value must be of type INTEGER, but found {}",
                                other.tag_type(parser.context)?
                            )),
                            loc: named_bit.value.loc,
                        })
                    }
                },
            ))
        })
        .collect::<Result<Vec<_>>>()?;

    let mut bit_positions = Vec::with_capacity(ast_bits.element.0.len());
    for ast_bit in &ast_bits.element.0 {
        let bit_offset = named_bits.iter().find_map(|(bit_name, bit_offset)| {
            if bit_name == &ast_bit.element.0 {
                Some(*bit_offset)
            } else {
                None
            }
        });
        match bit_offset {
            Some(bit_offset) => bit_positions.push(bit_offset),
            None => return Err(Error {
                kind: ErrorKind::Ast(
                    format!("identifier '{}' is not defined as a named bit in the governing BIT STRING type", ast_bit.element.0),
                ),
                loc: ast_bit.loc,
            })
        }
    }

    Ok(create_named_bit_string(bit_positions))
}

fn parse_braced_value<T: Parseable>(
    braced: &AstElement<AstBracedTokenStream>,
) -> Result<AstElement<T>> {
    let mut stream = TokenStream::from_tokens(braced.element.0.clone());
    let context = ParseContext::new(&mut stream);
    match T::parse(context) {
        ParseResult::Ok(value) => Ok(value),
        ParseResult::Fail(err) | ParseResult::Error(err) => Err(err),
    }
}

fn resolve_information_object<'a>(
    parser: &'a AstParser<'_>,
    ident: &AstElement<QualifiedIdentifier>,
) -> Result<&'a InformationObject> {
    parser
        .context
        .lookup_information_object(&ident.element)
        .ok_or_else(|| Error {
            kind: ErrorKind::Ast(format!(
                "undefined reference to information object '{}'",
                ident.element
            )),
            loc: ident.loc,
        })?
        .resolve(parser.context)
}

fn parse_choice_value(
    parser: &AstParser<'_>,
    stage: ParseValueAssignmentStage,
    choice: &AstElement<AstChoiceValue>,
    target_type: &ResolvedType,
    root_container: Option<(&[NamedStructureComponent], &[StructureValueComponent])>,
) -> Result<BuiltinValue> {
    let choice_type = match &target_type.ty {
        BuiltinType::Choice(ty) => ty,
        other_type => {
            return Err(Error {
                kind: ErrorKind::Ast(format!("CHOICE value cannot be assigned to {}", other_type,)),
                loc: choice.loc,
            })
        }
    };
    let alternative = 'block: {
        for alternative in choice_type.resolve_alternatives(parser.context)? {
            if alternative.name.element == choice.element.alternative.element.0 {
                break 'block alternative;
            }
        }

        return Err(Error {
            kind: ErrorKind::Ast(format!(
                "CHOICE type does not define an alternative named '{}'",
                choice.element.alternative.element.0,
            )),
            loc: choice.loc,
        });
    };

    let resolved_alternative_ty = match &alternative.alternative_type.ty {
        UntaggedType::ObjectClassField(ocf) => {
            if stage != ParseValueAssignmentStage::ClassReference {
                return Err(Error::break_parser());
            }
            let (resolved_type, kind) = parse_object_class_field_reference(
                parser,
                &alternative,
                choice.loc,
                ocf,
                root_container,
                None,
            )?;
            validate_class_field_reference_value(&choice.element.value, kind)?;
            resolved_type
        }
        _ => alternative.alternative_type.resolve(parser.context)?,
    };
    let alternative_value = parse_value(
        parser,
        stage,
        &choice.element.value,
        &resolved_alternative_ty,
    )?;

    Ok(BuiltinValue::Choice(ChoiceValue {
        alternative: choice
            .element
            .alternative
            .as_ref()
            .map(|name| name.0.clone()),
        alternative_type: resolved_alternative_ty,
        value: Box::new(alternative_value),
    }))
}

pub fn parse_value(
    parser: &AstParser<'_>,
    stage: ParseValueAssignmentStage,
    value: &AstElement<AstValue>,
    target_type: &ResolvedType,
) -> Result<AstElement<TypedValue>> {
    macro_rules! return_reference {
        ( $valref:expr ) => {
            return Ok(resolve_defined_value(parser, $valref)?
                .with_loc(value.loc)
                .map(|valref| TypedValue {
                    resolved_type: target_type.clone(),
                    value: ValueReference::Reference(valref),
                }))
        };
    }

    let mut loc = value.loc;
    let builtin = match &value.element {
        AstValue::BuiltinValue(builtin) => match &builtin.element {
            AstBuiltinValue::Null(_) => BuiltinValue::Null,
            AstBuiltinValue::BooleanValue(b) => match b.element {
                AstBooleanValue::True(_) => BuiltinValue::Boolean(true),
                AstBooleanValue::False(_) => BuiltinValue::Boolean(false),
            },
            AstBuiltinValue::StringLiteral(str_lit) => match &target_type.ty {
                BuiltinType::BitString(_) => {
                    let radix = match str_lit.element.kind {
                        StringKind::B => 2,
                        StringKind::H => 16,
                        StringKind::C => {
                            return Err(Error {
                                kind: ErrorKind::Ast(
                                    "cstring value cannot be assigned to BIT STRING".to_string(),
                                ),
                                loc: builtin.loc,
                            })
                        }
                    };

                    let (data, unused_bits) = parse_byte_string(&str_lit.element.data, radix);
                    BuiltinValue::BitString(BitStringValue { data, unused_bits })
                }
                BuiltinType::OctetString => {
                    let radix = match str_lit.element.kind {
                        StringKind::B => 2,
                        StringKind::H => 16,
                        StringKind::C => {
                            return Err(Error {
                                kind: ErrorKind::Ast(
                                    "cstring value cannot be assigned to OCTET STRING".to_string(),
                                ),
                                loc: builtin.loc,
                            })
                        }
                    };

                    let (data, _) = parse_byte_string(&str_lit.element.data, radix);
                    BuiltinValue::OctetString(data)
                }
                BuiltinType::CharacterString(tag_type) => {
                    parse_character_string(str_lit, *tag_type)?
                }
                BuiltinType::Time => {
                    BuiltinValue::Time(Time::parse(&str_lit.as_ref().map(|lit| &lit.data))?)
                }
                BuiltinType::UTCTime => BuiltinValue::UTCTime(UTCTime::parse(
                    &str_lit.as_ref().map(|lit| lit.data.as_bytes()),
                )?),
                BuiltinType::GeneralizedTime => BuiltinValue::GeneralizedTime(
                    GeneralizedTime::parse(&str_lit.as_ref().map(|lit| lit.data.as_bytes()))?,
                ),
                BuiltinType::Date => BuiltinValue::Date(Date::parse(
                    &str_lit.as_ref().map(|lit| lit.data.as_bytes()),
                )?),
                BuiltinType::TimeOfDay => BuiltinValue::TimeOfDay(TimeOfDay::parse(
                    &str_lit.as_ref().map(|lit| lit.data.as_bytes()),
                )?),
                BuiltinType::DateTime => BuiltinValue::DateTime(DateTime::parse(
                    &str_lit.as_ref().map(|lit| lit.data.as_bytes()),
                )?),
                BuiltinType::Duration => {
                    BuiltinValue::Duration(Duration::parse(&str_lit.as_ref().map(|lit| &lit.data))?)
                }
                other_type => {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "{} value cannot be assigned to {}",
                            str_lit.element.kind, other_type,
                        )),
                        loc: builtin.loc,
                    })
                }
            },
            AstBuiltinValue::BracedTokenStream(braced) => match &target_type.ty {
                BuiltinType::BitString(bit_string) => {
                    let ast_named_bits = parse_braced_value::<AstNamedBitStringValue>(braced)?;
                    loc = ast_named_bits.loc;
                    BuiltinValue::BitString(parse_named_bit_string(
                        parser,
                        bit_string,
                        &ast_named_bits,
                    )?)
                }
                BuiltinType::ObjectIdentifier => {
                    let object_id = parse_braced_value::<AstObjectIdentifierValue>(braced)?;
                    loc = object_id.loc;
                    BuiltinValue::ObjectIdentifier(object_id::parse_object_identifier(
                        parser,
                        &object_id,
                        TagType::ObjectIdentifier,
                    )?)
                }
                BuiltinType::RelativeOid => {
                    let object_id = parse_braced_value::<AstObjectIdentifierValue>(braced)?;
                    loc = object_id.loc;
                    BuiltinValue::RelativeOid(object_id::parse_object_identifier(
                        parser,
                        &object_id,
                        TagType::RelativeOid,
                    )?)
                }
                BuiltinType::Structure(_) => {
                    let struct_val = parse_braced_value::<AstStructureValue>(braced)?;
                    loc = struct_val.loc;
                    parse_structure_value(parser, stage, &struct_val, target_type, None)?
                }
                BuiltinType::StructureOf(struct_of) => {
                    let struct_of_val = parse_braced_value::<AstStructureOfValue>(braced)?;
                    loc = struct_of_val.loc;
                    let component_type = struct_of.component_type.resolve(parser.context)?;

                    let ast_elements = &struct_of_val.element.0;
                    let mut elements = Vec::with_capacity(ast_elements.len());
                    for ast_element in ast_elements {
                        elements.push(parse_value(parser, stage, ast_element, &component_type)?);
                    }
                    BuiltinValue::StructureOf(struct_of.ty, elements)
                }
                other => {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!("illegal value for {}", other)),
                        loc: braced.loc,
                    })
                }
            },
            AstBuiltinValue::IntegerValue(num) => BuiltinValue::Integer(parse_integer_value(num)?),
            AstBuiltinValue::DecimalValue(dec) => {
                BuiltinValue::RealLiteral(parse_decimal_value(dec)?)
            }
            AstBuiltinValue::ChoiceValue(choice) => {
                parse_choice_value(parser, stage, choice, target_type, None)?
            }
            AstBuiltinValue::SpecialRealValue(special) => {
                return Ok(AstElement::new(
                    TypedValue {
                        resolved_type: ResolvedType::universal(TagType::Real),
                        value: ValueReference::Reference(match &special.element {
                            AstSpecialRealValue::PlusInfinity(_) => PLUS_INFINITY_IDENT.clone(),
                            AstSpecialRealValue::MinusInfinity(_) => MINUS_INFINITY_IDENT.clone(),
                            AstSpecialRealValue::NotANumber(_) => NOT_A_NUMBER_IDENT.clone(),
                        }),
                    },
                    builtin.loc,
                ))
            }
            AstBuiltinValue::ContainingValue(containing) => {
                let constraint = target_type.constraints.as_ref().and_then(|constraints| {
                    constraints
                        .iter()
                        .find_map(|constraint| constraint.get_contents_constraint())
                });
                let contents_constraint = match constraint {
                    Some(contents_constraint) => contents_constraint,
                    _ => {
                        return Err(Error {
                            kind: ErrorKind::Ast("CONTAINING value can not be assigned to a type that does not have a contents constraint".to_string()),
                            loc: value.loc,
                        })
                    }
                };

                let container_type = match &target_type.ty {
                    BuiltinType::BitString(_) => TagType::BitString,
                    BuiltinType::OctetString => TagType::OctetString,
                    // constraint parser (compiler/ast/constraints.rs)
                    // ensures that content constraints can only be assigned to BIT STRING or OCTET STRING
                    _ => unreachable!(),
                };

                let contained_type = contents_constraint.content_type.resolve(parser.context)?;
                BuiltinValue::Containing(ContainingValue {
                    container_type,
                    value: Box::new(parse_value(
                        parser,
                        stage,
                        &containing.element.0,
                        &contained_type,
                    )?),
                })
            }
            AstBuiltinValue::OpenTypeValue(otv) => {
                let mut value_type = match &otv.element.open_type.element {
                    AstOpenTypeValueTypeReference::Type(ast_type) => {
                        let tagged_type = types::parse_type(parser, ast_type, &[])?;
                        tagged_type.resolve(parser.context)?
                    }
                    AstOpenTypeValueTypeReference::ObjectFieldReference(field_ref) => {
                        if stage != ParseValueAssignmentStage::ClassReference {
                            return Err(Error::break_parser());
                        }

                        let object_ref =
                            resolve_defined_value(parser, &field_ref.element.object_name)?;
                        let object = resolve_information_object(parser, &object_ref)?;

                        match &field_ref.element.field_ref.element {
                            AstFieldReference::TypeFieldReference(field_ref) => {
                                let field_name =
                                    field_ref.element.0.as_ref().map(|name| name.0.clone());
                                let field_type = match object.find_field(&field_name)? {
                                    ObjectField::Type(ty) => ty,
                                    _ => {
                                        return Err(Error {
                                            kind: ErrorKind::Ast(format!(
                                                "field '{}' is not an open type field",
                                                field_name.element
                                            )),
                                            loc: field_name.loc,
                                        })
                                    }
                                };
                                field_type.resolve(parser.context)?
                            }
                            AstFieldReference::ValueFieldReference(field_ref) => {
                                return Err(Error {
                                    kind: ErrorKind::Ast("expecting open type field reference, but found value field reference".to_string()),
                                    loc: field_ref.loc,
                                });
                            }
                        }
                    }
                };
                if !value_type.ty.is_assignable_to(&target_type.ty) {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "expecting {}, but found {}",
                            target_type.ty, value_type.ty
                        )),
                        loc: otv.element.open_type.loc,
                    });
                }

                value_type.tag = match &target_type.tag {
                    Some(tag)
                        if tag.class == Class::Universal && tag.num == TagType::Any as u16 =>
                    {
                        value_type.tag
                    }
                    _ => target_type.tag.clone(),
                };
                return parse_value(parser, stage, &otv.element.value, &value_type);
            }
            AstBuiltinValue::ObjectFieldReference(object_field_ref) => {
                if stage != ParseValueAssignmentStage::ClassReference {
                    return Err(Error::break_parser());
                }

                let object_name = &object_field_ref.element.object_name;
                let ident = resolve_defined_value(parser, object_name)?;
                let object = resolve_information_object(parser, &ident)?;

                let name = match &object_field_ref.element.field_ref.element {
                    AstFieldReference::ValueFieldReference(valref) => {
                        &valref.element.0.as_ref().map(|name| &name.0)
                    }
                    AstFieldReference::TypeFieldReference(typeref) => return Err(Error {
                        kind: ErrorKind::Ast(
                            "expecting value field reference, but found open type field reference"
                                .to_string(),
                        ),
                        loc: typeref.loc,
                    }),
                };

                let value = object
                    .fields
                    .iter()
                    .find_map(|(field_name, field)| {
                        if field_name == name.element {
                            Some(match field {
                                ObjectField::Value(value) => value,
                                _ => unreachable!(),
                            })
                        } else {
                            None
                        }
                    })
                    .ok_or_else(|| Error {
                        kind: ErrorKind::Ast(format!(
                            "no such field '{}' in governing information object class",
                            name.element
                        )),
                        loc: name.loc,
                    })?;
                return Ok(value.as_ref().map(|typed_value| TypedValue {
                    resolved_type: target_type.clone(),
                    value: typed_value.value.clone(),
                }));
            }
        },
        AstValue::DefinedValue(valref) => match &target_type.ty {
            BuiltinType::Enumerated(items) => 'block: {
                if valref.element.external_module.is_none() {
                    for item in items {
                        if item.name.element == valref.element.value.element.0 {
                            break 'block BuiltinValue::Enumerated(Box::new(match &item.value {
                                EnumerationItemValue::Implied(implied) => AstElement::new(
                                    TypedValue {
                                        resolved_type: ResolvedType::universal(TagType::Integer),
                                        value: ValueReference::BuiltinValue(BuiltinValue::Integer(
                                            BigInt::from(*implied),
                                        )),
                                    },
                                    value.loc,
                                ),
                                EnumerationItemValue::Specified(specified) => {
                                    return Ok(specified.clone());
                                }
                            }));
                        }
                    }
                }

                return_reference!(valref);
            }
            BuiltinType::Integer(integer)
                if valref.element.external_module.is_none() && integer.named_values.is_some() =>
            {
                for named_value in integer.named_values.as_ref().unwrap() {
                    if named_value.name.element == valref.element.value.element.0 {
                        return Ok(named_value.value.clone());
                    }
                }

                return_reference!(valref);
            }
            _ => return_reference!(valref),
        },
    };

    Ok(AstElement::new(
        TypedValue {
            resolved_type: target_type.clone(),
            value: ValueReference::BuiltinValue(builtin),
        },
        loc,
    ))
}

pub(crate) fn resolve_defined_value(
    parser: &AstParser<'_>,
    defined_value: &AstElement<AstDefinedValue>,
) -> Result<AstElement<QualifiedIdentifier>> {
    match &defined_value.element.external_module {
        Some(external_module) => {
            let header = parser
                .context
                .lookup_module(&parser.module)
                .expect("lookup_module");
            let mut imported_module = header
                .imports
                .iter()
                .find_map(|import| {
                    if import.module.element.name == external_module.element.0 {
                        Some(import.module.clone())
                    } else {
                        None
                    }
                })
                .ok_or_else(|| Error {
                    kind: ErrorKind::Ast(format!(
                        "no such imported module '{}'",
                        external_module.element.0
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
            Ok(defined_value.as_ref().map(|_| {
                QualifiedIdentifier::new(
                    imported_module.element,
                    defined_value.element.value.element.0.clone(),
                )
            }))
        }
        None => {
            Ok(parser
                .resolve_symbol(&defined_value.element.value.as_ref().map(|valref| &valref.0))?)
        }
    }
}

pub enum ParsedValueAssignment {
    Value(DeclaredValue),
    Class(InformationObjectReference),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseValueAssignmentStage {
    Normal,
    Class,
    ClassReference,
}

pub fn parse_value_assignment(
    parser: &AstParser<'_>,
    value_assignment: &AstElement<AstValueAssignment>,
    stage: ParseValueAssignmentStage,
) -> Result<Option<(QualifiedIdentifier, ParsedValueAssignment)>> {
    let name = value_assignment.element.name.element.0.clone();
    let ident = QualifiedIdentifier::new(parser.module.clone(), name);

    if let AstType::ConstrainedType(constrained) = &value_assignment.element.ty.element {
        if let AstConstrainedType::Suffixed(suffixed) = &constrained.element {
            if let AstUntaggedType::DefinedType(typeref) = &suffixed.element.ty.element {
                let class = parser.context.lookup_information_object_class(
                    &types::resolve_defined_type(parser, typeref)?.element,
                );
                if let Some(class) = class {
                    match stage {
                        ParseValueAssignmentStage::Class => {
                            return Ok(Some(match &value_assignment.element.value.element {
                                AstValue::BuiltinValue(builtin) => match &builtin.element {
                                    AstBuiltinValue::BracedTokenStream(tokens) => {
                                        let object = class::parse_object_tokens(
                                            parser,
                                            tokens,
                                            class.resolve(parser.context)?,
                                        )?;
                                        (
                                            ident,
                                            ParsedValueAssignment::Class(
                                                InformationObjectReference::Value(object),
                                            ),
                                        )
                                    }
                                    _ => {
                                        return Err(Error {
                                            kind: ErrorKind::Ast(
                                                "expecting an information object".to_string(),
                                            ),
                                            loc: builtin.loc,
                                        })
                                    }
                                },
                                AstValue::DefinedValue(valref) => (
                                    ident,
                                    ParsedValueAssignment::Class(
                                        InformationObjectReference::Reference(
                                            resolve_defined_value(parser, valref)?,
                                        ),
                                    ),
                                ),
                            }));
                        }
                        ParseValueAssignmentStage::Normal
                        | ParseValueAssignmentStage::ClassReference => {
                            return Ok(None);
                        }
                    }
                }
            }
        }
    }

    Ok(match stage {
        ParseValueAssignmentStage::Normal | ParseValueAssignmentStage::ClassReference => {
            let mut ty = types::parse_type(parser, &value_assignment.element.ty, &[])?;
            let mut resolved_ty = ty.resolve(parser.context)?;

            if let Some(typeref) =
                types::ast_type_as_parameterized_type_reference(&value_assignment.element.ty)
            {
                let (type_assignment, parameters) =
                    constraints::resolve_type_assignment_from_parameterized_type_reference(
                        parser,
                        typeref,
                        Vec::new(),
                    )?;
                let pending = constraints::parse_type_assignment_constraint_with_resolved_type(
                    parser,
                    type_assignment,
                    &resolved_ty,
                    parameters
                        .iter()
                        .map(|(name, param)| (name, param))
                        .collect(),
                )?;
                apply_pending_constraint(parser.context, &mut ty, pending)?;
                resolved_ty = ty.resolve(parser.context)?;
            }

            let val =
                match parse_value(parser, stage, &value_assignment.element.value, &resolved_ty) {
                    Ok(val) => val,
                    Err(err) if err.kind == ErrorKind::BreakParser => {
                        return Ok(None);
                    }
                    Err(err) => return Err(err),
                };

            Some((
                ident.clone(),
                ParsedValueAssignment::Value(DeclaredValue {
                    name: ident,
                    value: val,
                    ty,
                }),
            ))
        }
        ParseValueAssignmentStage::Class => None,
    })
}
