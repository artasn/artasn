use std::fmt::Write;

use num::{bigint::Sign, BigInt};

use crate::{
    compiler::{context::DeclaredValue, parser::*},
    module::*,
    types::*,
    values::*,
};

use super::{object_id, types, AstParser};

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
) -> AstElement<Value> {
    valref.as_ref().map(|valref| {
        Value::Reference(
            parser
                .context
                .lookup_module(&parser.module)
                .expect("lookup_module")
                .resolve_symbol(parser.context, &valref.0),
        )
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

fn parse_structure_value(
    parser: &AstParser<'_>,
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
                let component_type = ty_component.component_type.resolve(parser.context)?;
                (
                    parse_value(parser, &val_component.element.value, &component_type)?,
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
                match named_bit.value.resolve(parser.context)? {
                    BuiltinValue::Integer(i) => {
                        if i.sign() == Sign::Minus {
                            return Err(Error {
                                kind: ErrorKind::Ast(
                                    "named bit value must be a positive INTEGER".to_string(),
                                ),
                                loc: named_bit.value.loc,
                            });
                        }
                        let uint: u64 = match i.try_into() {
                            Ok(uint) => uint,
                            Err(_) => {
                                return Err(Error {
                                    kind: ErrorKind::Ast(
                                        "named bit value cannot be larger than 2^64 - 1"
                                            .to_string(),
                                    ),
                                    loc: named_bit.value.loc,
                                })
                            }
                        };
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

pub fn parse_value(
    parser: &AstParser<'_>,
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
                            StringKind::B => 2,
                            StringKind::H => 16,
                            StringKind::C => {
                                return Err(Error {
                                    kind: ErrorKind::Ast(
                                        "cstring value cannot be assigned to BIT STRING"
                                            .to_string(),
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
                                        "cstring value cannot be assigned to OCTET STRING"
                                            .to_string(),
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
                    BuiltinType::Duration => BuiltinValue::Duration(Duration::parse(
                        &str_lit.as_ref().map(|lit| &lit.data),
                    )?),
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
                        let ast_named_bits =
                            parse_braced_value::<AstNamedBitStringValue>(braced)?;
                        BuiltinValue::BitString(parse_named_bit_string(
                            parser,
                            bit_string,
                            &ast_named_bits,
                        )?)
                    }
                    BuiltinType::ObjectIdentifier => {
                        let object_id =
                            parse_braced_value::<AstObjectIdentifierValue>(braced)?;
                        BuiltinValue::ObjectIdentifier(object_id::parse_object_identifier(
                            parser,
                            &object_id,
                            TagType::ObjectIdentifier,
                        )?)
                    }
                    BuiltinType::RelativeOid => {
                        let object_id =
                            parse_braced_value::<AstObjectIdentifierValue>(braced)?;
                        BuiltinValue::RelativeOid(object_id::parse_object_identifier(
                            parser,
                            &object_id,
                            TagType::RelativeOid,
                        )?)
                    }
                    BuiltinType::Structure(_) => {
                        let struct_val = parse_braced_value::<AstStructureValue>(braced)?;
                        parse_structure_value(parser, &struct_val, target_type)?
                    }
                    BuiltinType::StructureOf(struct_of) => {
                        let struct_of_val =
                            parse_braced_value::<AstStructureOfValue>(braced)?;
                        let component_type = struct_of.component_type.resolve(parser.context)?;

                        let ast_elements = &struct_of_val.element.0;
                        let mut elements = Vec::with_capacity(ast_elements.len());
                        for ast_element in ast_elements {
                            elements.push(parse_value(parser, ast_element, &component_type)?);
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
                AstBuiltinValue::IntegerValue(num) => {
                    BuiltinValue::Integer(parse_integer_value(num)?)
                }
                AstBuiltinValue::DecimalValue(dec) => {
                    BuiltinValue::RealLiteral(parse_decimal_value(dec)?)
                }
                AstBuiltinValue::ChoiceValue(choice) => {
                    let alternative_ty = match &target_type.ty {
                        BuiltinType::Choice(ty) => 'block: {
                            for alternative in &ty.alternatives {
                                if alternative.name.element == choice.element.alternative.element.0
                                {
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
                    let alternative_value =
                        parse_value(parser, &choice.element.value, &resolved_alternative_ty)?;

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
                        Value::Reference(match &special.element {
                            AstSpecialRealValue::PlusInfinity(_) => PLUS_INFINITY_IDENT.clone(),
                            AstSpecialRealValue::MinusInfinity(_) => MINUS_INFINITY_IDENT.clone(),
                            AstSpecialRealValue::NotANumber(_) => NOT_A_NUMBER_IDENT.clone(),
                        }),
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
                            &containing.element.0,
                            &contained_type,
                        )?),
                    })
                }
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

                    parse_valuereference(parser, valref).element
                }
                _ => parse_valuereference(parser, valref).element,
            },
        },
        value.loc,
    ))
}

pub fn parse_value_assignment(
    parser: &AstParser<'_>,
    value_assignment: &AstElement<AstValueAssignment>,
) -> Result<(QualifiedIdentifier, DeclaredValue)> {
    let name = value_assignment.element.name.element.0.clone();
    let ty = types::parse_type(
        parser,
        &value_assignment.element.ty,
        &[],
        types::TypeContext::Contextless,
    )?;
    let resolved_ty = ty.resolve(parser.context)?;
    let val = parse_value(parser, &value_assignment.element.value, &resolved_ty)?;

    Ok((
        QualifiedIdentifier::new(parser.module.clone(), name),
        DeclaredValue { value: val, ty },
    ))
}
