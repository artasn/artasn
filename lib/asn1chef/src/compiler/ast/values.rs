use std::fmt::Write;

use num::{bigint::Sign, BigInt};

use crate::{
    compiler::{ast::types::TypeContext, context::DeclaredValue, parser::*},
    module::*,
    types::*,
    values::*,
};

use super::{class, object_id, types, AstParser};

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

pub fn parse_valuereference(
    parser: &AstParser<'_>,
    valref: &AstElement<AstValueReference>,
) -> AstElement<QualifiedIdentifier> {
    valref.as_ref().map(|valref| {
        parser
            .context
            .lookup_module(&parser.module)
            .expect("lookup_module")
            .resolve_symbol(parser.context, &valref.0)
    })
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
    ty_component: &'b StructureComponent,
    class_type: &AstElement<String>,
    field: &AstElement<String>,
) -> Result<(&'a ObjectClassField, &'b TableConstraint)> {
    let table_constraint = ty_component
        .component_type
        .constraint
        .as_ref()
        .and_then(|constraint| {
            constraint.find(|element| matches!(element, SubtypeElement::Table(_)))
        });
    let table_constraint = match table_constraint {
        Some(table_constraint) => match table_constraint {
            SubtypeElement::Table(table) => table,
            _ => unreachable!(),
        },
        None => todo!("information object class field references without table constraints are currently unsupported"),
    };

    let class = parser
        .context
        .lookup_information_object_class(&parser.resolve_symbol(&class_type.as_ref()).element)
        .ok_or_else(|| Error {
            kind: ErrorKind::Ast(format!(
                "undefined reference to information object class '{}'",
                class_type.element,
            )),
            loc: class_type.loc,
        })?;

    Ok((
        class
            .fields
            .iter()
            .find_map(|(name, class_field)| {
                if name.element == field.element {
                    Some(class_field)
                } else {
                    None
                }
            })
            .ok_or_else(|| Error {
                kind: ErrorKind::Ast(format!(
                    "no such field '{}' in information object class type '{}'",
                    field.element, class_type.element,
                )),
                loc: field.loc,
            })?,
        table_constraint,
    ))
}

fn parse_object_class_field_reference(
    parser: &AstParser<'_>,
    struct_component: &StructureComponent,
    val_component: &AstElement<AstStructureValueComponent>,
    ocf: &ObjectClassFieldReference,
    struct_components: &[StructureComponent],
    struct_component_values: &[StructureValueComponent],
) -> Result<ResolvedType> {
    let (field, table_constraint) =
        parse_object_class_field(parser, struct_component, &ocf.class_type, &ocf.field)?;
    let set = parser
        .context
        .lookup_information_object_class_set(
            &parser
                .resolve_symbol(&table_constraint.set_name.as_ref())
                .element,
        )
        .ok_or_else(|| Error {
            kind: ErrorKind::Ast(format!(
                "undefined reference to information object class set '{}'",
                table_constraint.set_name.element,
            )),
            loc: table_constraint.set_name.loc,
        })?;

    match (field, &ocf.kind) {
        (ObjectClassField::Value(value_field), ObjectClassFieldReferenceKind::Value) => {
            let mut value_type = value_field.field_type.resolve(parser.context)?;
            value_type.tag = struct_component.component_type.tag.clone().or(value_type.tag);
            Ok(value_type)
        }
        (ObjectClassField::OpenType(_), ObjectClassFieldReferenceKind::OpenType) => {
            let field_ref = match &table_constraint.field_ref {
                Some(field_ref) => field_ref,
                None => todo!("open type references without a field reference are currently unsupported"),
            };
            if struct_component.name.element == field_ref.element {
                return Err(Error {
                    kind: ErrorKind::Ast(format!(
                        "illegal self-referential table constraint '{}'",
                        field_ref.element,
                    )),
                    loc: field_ref.loc,
                })
            }

            let component_ref = struct_components
                .iter()
                .find(|component| component.name.element == field_ref.element)
                .ok_or_else(|| Error {
                    kind: ErrorKind::Ast(format!(
                        "referenced field '{}' does not match the name of a sibling field",
                        field_ref.element,
                    )),
                    loc: field_ref.loc,
                })?;
            let component_ref_type = match &component_ref.component_type.ty {
                UntaggedType::ObjectClassField(ocf) => ocf,
                _ => return Err(Error {
                    kind: ErrorKind::Ast(format!(
                        "referenced field '{}' is not an information object class field reference",
                        field_ref.element,
                    )),
                    loc: field_ref.loc,
                })
            };
            let component_value = struct_component_values.iter().find(|component| component.name.element == field_ref.element).expect("field_ref matches struct_components but not struct_value_components");

            for object_class_ref in set {
                let object_class = object_class_ref.resolve(parser.context)?;

                let value_field = object_class.fields.iter().find_map(|(name, field)| if name == &component_ref_type.field.element {
                    Some(field)
                } else {
                    None
                });
                if let Some(class_field) = value_field {
                    match class_field {
                        ObjectClassValueField::Value(value) => {
                            if component_value.value.try_eq(parser.context, value)? {
                                let type_field = object_class.fields.iter().find_map(|(name, field)| if name == &ocf.field.element {
                                    Some(field)
                                } else {
                                    None
                                });
                                if let Some(type_field) = type_field {
                                    match type_field {
                                        ObjectClassValueField::Type(tagged_type) => {
                                            let mut outer_tag = struct_component.component_type.tag.clone().expect("open type without a tag");
                                            let inner_type = tagged_type.resolve(parser.context)?;
                                            match &mut outer_tag.kind {
                                                TagKind::Explicit(inner_tag) => {
                                                    *inner_tag = inner_type.tag.clone().map(|tag| (tag.class, tag.num));
                                                    return Ok(ResolvedType {
                                                        tag: Some(outer_tag),
                                                        ty: inner_type.ty,
                                                        constraint: inner_type.constraint,
                                                    });
                                                }
                                                TagKind::Implicit => {
                                                    return Ok(ResolvedType {
                                                        tag: struct_component.component_type.tag.clone(),
                                                        ty: inner_type.ty,
                                                        constraint: inner_type.constraint,
                                                    });
                                                }
                                            }
                                        }
                                        ObjectClassValueField::Value(_) => unreachable!(),
                                    }
                                } else {
                                    return Err(Error {
                                        kind: ErrorKind::Ast(format!(
                                            "class in set '{}' with field '{}' equal to {} does not define the field '{}'",
                                            table_constraint.set_name.element,
                                            component_ref_type.field.element,
                                            component_value.value.resolve(parser.context)?.value,
                                            struct_component.name.element,
                                        )),
                                        loc: val_component.loc,
                                    });
                                }
                            }
                        }
                        ObjectClassValueField::Type(_) => todo!("open types referencing open types is not supported; should it be?"),
                    }
                }
            }

            Err(Error {
                kind: ErrorKind::Ast(format!(
                    "there is no class in set '{}' with field '{}' equal to {}, and thus the open type for component '{}' could not be determined",
                    table_constraint.set_name.element,
                    component_ref_type.field.element,
                    component_value.value.resolve(parser.context)?.value,
                    struct_component.name.element,
                )),
                loc: val_component.loc,
            })
        }
        _ => panic!("mismatch between field reference kind and field definition kind; this should never happen"),
    }
}

fn parse_structure_value(
    parser: &AstParser<'_>,
    stage: ParseValueAssignmentStage,
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
                let component_type = match &ty_component.component_type.ty {
                    UntaggedType::ObjectClassField(ocf) => {
                        if stage != ParseValueAssignmentStage::ClassReferenceValues {
                            return Err(Error::break_parser());
                        }
                        parse_object_class_field_reference(
                            parser,
                            ty_component,
                            val_component,
                            ocf,
                            struct_ty_components,
                            &components,
                        )?
                    }
                    _ => ty_component.component_type.resolve(parser.context)?,
                };
                (
                    parse_value(parser, stage, &val_component.element.value, &component_type)?,
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

fn resolve_information_object_class_value<'a>(
    parser: &'a AstParser<'_>,
    ident: &AstElement<QualifiedIdentifier>,
) -> Result<&'a InformationObjectClassValue> {
    parser
        .context
        .lookup_information_object_class_value(&ident.element)
        .ok_or_else(|| Error {
            kind: ErrorKind::Ast(format!(
                "undefined reference to information object class value '{}'",
                ident.element
            )),
            loc: ident.loc,
        })?
        .resolve(parser.context)
}

pub fn parse_value(
    parser: &AstParser<'_>,
    stage: ParseValueAssignmentStage,
    value: &AstElement<AstValue>,
    target_type: &ResolvedType,
) -> Result<AstElement<TypedValue>> {
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
                    BuiltinValue::BitString(parse_named_bit_string(
                        parser,
                        bit_string,
                        &ast_named_bits,
                    )?)
                }
                BuiltinType::ObjectIdentifier => {
                    let object_id = parse_braced_value::<AstObjectIdentifierValue>(braced)?;
                    BuiltinValue::ObjectIdentifier(object_id::parse_object_identifier(
                        parser,
                        &object_id,
                        TagType::ObjectIdentifier,
                    )?)
                }
                BuiltinType::RelativeOid => {
                    let object_id = parse_braced_value::<AstObjectIdentifierValue>(braced)?;
                    BuiltinValue::RelativeOid(object_id::parse_object_identifier(
                        parser,
                        &object_id,
                        TagType::RelativeOid,
                    )?)
                }
                BuiltinType::Structure(_) => {
                    let struct_val = parse_braced_value::<AstStructureValue>(braced)?;
                    parse_structure_value(parser, stage, &struct_val, target_type)?
                }
                BuiltinType::StructureOf(struct_of) => {
                    let struct_of_val = parse_braced_value::<AstStructureOfValue>(braced)?;
                    let component_type = struct_of.component_type.resolve(parser.context)?;

                    let ast_elements = &struct_of_val.element.0;
                    let mut elements = Vec::with_capacity(ast_elements.len());
                    for ast_element in ast_elements {
                        elements.push(parse_value(parser, stage, ast_element, &component_type)?);
                    }
                    BuiltinValue::SequenceOf(elements)
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
                let alternative_ty = match &target_type.ty {
                    BuiltinType::Choice(ty) => 'block: {
                        for alternative in &ty.alternatives {
                            if alternative.name.element == choice.element.alternative.element.0 {
                                break 'block &alternative.alternative_type;
                            }
                        }

                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "CHOICE type does not define an alternative named '{}'",
                                choice.element.alternative.element.0,
                            )),
                            loc: builtin.loc,
                        });
                    }
                    other_type => {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "CHOICE value cannot be assigned to {}",
                                other_type,
                            )),
                            loc: builtin.loc,
                        })
                    }
                };

                let resolved_alternative_ty = alternative_ty.resolve(parser.context)?;
                let alternative_value = parse_value(
                    parser,
                    stage,
                    &choice.element.value,
                    &resolved_alternative_ty,
                )?;

                BuiltinValue::Choice(ChoiceValue {
                    alternative: choice
                        .element
                        .alternative
                        .as_ref()
                        .map(|name| name.0.clone()),
                    alternative_type: resolved_alternative_ty,
                    value: Box::new(alternative_value),
                })
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
                let constraints = target_type.constraint.as_ref().map(|constraint| {
                    constraint
                        .flatten()
                        .iter()
                        .find_map(|subtype| match &subtype.element {
                            SubtypeElement::Contents(contents) => Some(contents),
                            _ => None,
                        })
                });
                let constraints = match constraints {
                    Some(Some(constraints)) => constraints,
                    _ => {
                        return Err(Error {
                            kind: ErrorKind::Ast("CONTAINING value can not be applied to a type that does not have a contents constraint".to_string()),
                            loc: value.loc,
                        })
                    }
                };

                let container_type = match &target_type.ty {
                    BuiltinType::BitString(_) => TagType::BitString,
                    BuiltinType::OctetString => TagType::OctetString,
                    // constraint parser (compiler/ast/constraints.rs)
                    // ensures that content constraints can only be applied to BIT STRING or OCTET STRING
                    _ => unreachable!(),
                };

                let contained_type = constraints.ty.resolve(parser.context)?;
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
                let value_type = match &otv.element.open_type.element {
                    AstOpenTypeValueTypeReference::Type(ast_type) => {
                        let tagged_type =
                            types::parse_type(parser, ast_type, &[], TypeContext::Contextless)?;
                        tagged_type.resolve(parser.context)?
                    }
                    AstOpenTypeValueTypeReference::ObjectClassValueFieldReference(field_ref) => {
                        if stage != ParseValueAssignmentStage::ClassReferenceValues {
                            return Err(Error::break_parser());
                        }

                        let class_value_ref =
                            resolve_valuereference(parser, &field_ref.element.class_value_name);
                        let class_value =
                            resolve_information_object_class_value(parser, &class_value_ref)?;

                        match &field_ref.element.field_ref.element {
                            AstFieldReference::TypeFieldReference(field_ref) => {
                                let field_name = &field_ref.element.0.element.0;
                                let field_type = class_value.fields.iter().find_map(|(name, field)| {
                                    if name == field_name {
                                        Some(match field {
                                            ObjectClassValueField::Type(tagged_type) => tagged_type,
                                            ObjectClassValueField::Value(_) => unreachable!(),
                                        })
                                    } else {
                                        None
                                    }
                                }).ok_or_else(|| Error {
                                    kind: ErrorKind::Ast(format!("no such open type field '{}' in information object class value '{}'", field_name, class_value_ref.element)),
                                    loc: field_ref.loc,
                                })?;
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
                if value_type.ty != target_type.ty {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "expecting {}, but found {}",
                            target_type.ty, value_type.ty
                        )),
                        loc: otv.element.open_type.loc,
                    });
                }
                return Ok(parse_value(parser, stage, &otv.element.value, target_type)?);
            }
            AstBuiltinValue::ObjectClassValueFieldReference(class_value_field_ref) => {
                if stage != ParseValueAssignmentStage::ClassReferenceValues {
                    return Err(Error::break_parser());
                }

                let class_value_name = &class_value_field_ref.element.class_value_name;
                let ident = resolve_valuereference(parser, class_value_name);
                let class_value = resolve_information_object_class_value(parser, &ident)?;

                let name = match &class_value_field_ref.element.field_ref.element {
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

                let value = class_value
                    .fields
                    .iter()
                    .find_map(|(field_name, field)| {
                        if field_name == name.element {
                            Some(match field {
                                ObjectClassValueField::Value(value) => value,
                                ObjectClassValueField::Type(_) => unreachable!(),
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
        AstValue::ValueReference(valref) => match &target_type.ty {
            BuiltinType::Enumerated(items) => 'block: {
                for item in items {
                    if item.name.element == valref.element.0 {
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

                return Ok(parse_valuereference(parser, valref)
                    .with_loc(value.loc)
                    .map(|valref| TypedValue {
                        resolved_type: target_type.clone(),
                        value: ValueReference::Reference(valref),
                    }));
            }
            _ => {
                return Ok(parse_valuereference(parser, valref)
                    .with_loc(value.loc)
                    .map(|valref| TypedValue {
                        resolved_type: target_type.clone(),
                        value: ValueReference::Reference(valref),
                    }))
            }
        },
    };
    Ok(AstElement::new(
        TypedValue {
            resolved_type: target_type.clone(),
            value: ValueReference::BuiltinValue(builtin),
        },
        value.loc,
    ))
}

pub(crate) fn resolve_valuereference(
    parser: &AstParser<'_>,
    valref: &AstElement<AstValueReference>,
) -> AstElement<QualifiedIdentifier> {
    parser.resolve_symbol(&valref.as_ref().map(|valref| &valref.0))
}

pub enum ParsedValueAssignment {
    Value(DeclaredValue),
    Class(ObjectClassReference),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseValueAssignmentStage {
    NormalValues,
    ClassValues,
    ClassReferenceValues,
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
            if let AstUntaggedType::TypeReference(typeref) = &suffixed.element.ty.element {
                let class = parser.context.lookup_information_object_class(
                    &types::resolve_typereference(parser, typeref).element,
                );
                if let Some(class) = class {
                    match stage {
                        ParseValueAssignmentStage::ClassValues => {
                            return Ok(Some(match &value_assignment.element.value.element {
                                AstValue::BuiltinValue(builtin) => match &builtin.element {
                                    AstBuiltinValue::BracedTokenStream(tokens) => {
                                        let class_value =
                                            class::parse_information_object_class_value(
                                                parser, tokens, class,
                                            )?;
                                        (
                                            ident,
                                            ParsedValueAssignment::Class(
                                                ObjectClassReference::Value(class_value),
                                            ),
                                        )
                                    }
                                    _ => {
                                        return Err(Error {
                                            kind: ErrorKind::Ast(
                                                "expecting an information object class value"
                                                    .to_string(),
                                            ),
                                            loc: builtin.loc,
                                        })
                                    }
                                },
                                AstValue::ValueReference(valref) => (
                                    ident,
                                    ParsedValueAssignment::Class(ObjectClassReference::Reference(
                                        resolve_valuereference(parser, valref),
                                    )),
                                ),
                            }));
                        }
                        ParseValueAssignmentStage::NormalValues
                        | ParseValueAssignmentStage::ClassReferenceValues => {
                            return Ok(None);
                        }
                    }
                }
            }
        }
    }

    Ok(match stage {
        ParseValueAssignmentStage::NormalValues
        | ParseValueAssignmentStage::ClassReferenceValues => {
            let ty = types::parse_type(
                parser,
                &value_assignment.element.ty,
                &[],
                types::TypeContext::Contextless,
            )?;
            let resolved_ty = ty.resolve(parser.context)?;
            let val =
                match parse_value(parser, stage, &value_assignment.element.value, &resolved_ty) {
                    Ok(val) => val,
                    Err(err) if err.kind == ErrorKind::BreakParser => {
                        return Ok(None);
                    }
                    Err(err) => return Err(err),
                };

            Some((
                ident,
                ParsedValueAssignment::Value(DeclaredValue { value: val, ty }),
            ))
        }
        ParseValueAssignmentStage::ClassValues => None,
    })
}
