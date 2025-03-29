use crate::{
    compiler::{ast::constraints, parser::*},
    module::{ModuleIdentifier, QualifiedIdentifier},
    types::*,
    values::{
        InformationObject, InformationObjectReference, InformationObjectSet, ObjectField,
        ObjectFieldReference, ObjectSetElement, ValueSet,
    },
};

use super::{
    extra::AstSyntaxTokenLiteral,
    types::{self, TypeContext},
    values::{self, ParseValueAssignmentStage},
    AstParser,
};

lazy_static::lazy_static! {
    static ref TYPE_IDENTIFIER_IDENT: QualifiedIdentifier = QualifiedIdentifier::new(
        ModuleIdentifier::with_name(String::from("InformationObjectClasses")),
        String::from("CLASS-TYPE-IDENTIFIER"),
    );
    static ref ABSTRACT_SYNTAX_IDENT: QualifiedIdentifier = QualifiedIdentifier::new(
        ModuleIdentifier::with_name(String::from("InformationObjectClasses")),
        String::from("CLASS-ABSTRACT-SYNTAX"),
    );
}

fn parse_class_value_field(
    parser: &AstParser<'_>,
    class_idents: &[QualifiedIdentifier],
    ast_field: &AstElement<AstValueField>,
) -> Result<ObjectClassField> {
    Ok(match &ast_field.element.subject.element {
        AstValueFieldSubject::Type(ast_field_type) => {
            let mut field_type =
                types::parse_type(parser, ast_field_type, &[], TypeContext::Contextless)?;

            match is_type_class(class_idents, &field_type) {
                Some(class) => ObjectClassField::Object(ObjectClassFieldObject {
                    class,
                    optional: ast_field.element.optional,
                }),
                None => match &field_type.ty {
                    UntaggedType::ObjectClassField(ocf) => {
                        if ocf.kind == ObjectClassFieldReferenceKind::TypeLike {
                            return Err(Error {
                                kind: ErrorKind::Ast(
                                    "value field type cannot be a reference to an open type field"
                                        .to_string(),
                                ),
                                loc: ocf.field.loc,
                            });
                        }

                        if ast_field.element.unique {
                            return Err(Error {
                                kind: ErrorKind::Ast(
                                    "UNIQUE is invalid for reference-type value field".to_string(),
                                ),
                                loc: ast_field.loc,
                            });
                        }

                        if let Some(default) = &ast_field.element.default {
                            return Err(Error {
                                kind: ErrorKind::Ast(
                                    "DEFAULT is invalid for reference-type value field".to_string(),
                                ),
                                loc: default.loc,
                            });
                        }

                        ObjectClassField::Value(ObjectClassFieldValue {
                            field_type: ObjectClassFieldValueType::ObjectClassFieldReference(
                                ocf.clone(),
                            ),
                            option: if ast_field.element.optional {
                                Some(ObjectClassFieldValueOption::Optional)
                            } else {
                                None
                            },
                        })
                    }
                    _ => {
                        let target_type = field_type.resolve(parser.context)?;

                        let constraint = constraints::parse_type_constraint(
                            parser,
                            ast_field_type,
                            &target_type,
                            &[],
                        )?;
                        constraints::apply_pending_constraint(&mut field_type, constraint);

                        let option = match (
                            ast_field.element.unique,
                            ast_field.element.optional,
                            &ast_field.element.default,
                        ) {
                            (true, false, None) => Some(ObjectClassFieldValueOption::Unique),
                            (false, true, None) => Some(ObjectClassFieldValueOption::Optional),
                            (false, false, Some(default)) => {
                                Some(ObjectClassFieldValueOption::Default(values::parse_value(
                                    parser,
                                    ParseValueAssignmentStage::NormalValues,
                                    default,
                                    &target_type,
                                )?))
                            }
                            (false, false, None) => None,
                            (true, true, None) => {
                                return Err(Error {
                                    kind: ErrorKind::Ast(
                                        "field cannot be both OPTIONAL and UNIQUE".to_string(),
                                    ),
                                    loc: ast_field.element.name.loc,
                                })
                            }
                            (true, false, Some(default)) => {
                                return Err(Error {
                                    kind: ErrorKind::Ast(
                                        "field cannot be both UNIQUE and have a DEFAULT"
                                            .to_string(),
                                    ),
                                    loc: default.loc,
                                })
                            }
                            (false, true, Some(default)) | (true, true, Some(default)) => {
                                return Err(Error {
                                    kind: ErrorKind::Ast(
                                        "field cannot be both OPTIONAL and have a DEFAULT"
                                            .to_string(),
                                    ),
                                    loc: default.loc,
                                })
                            }
                        };

                        ObjectClassField::Value(ObjectClassFieldValue {
                            field_type: ObjectClassFieldValueType::TaggedType(field_type),
                            option,
                        })
                    }
                },
            }
        }
        AstValueFieldSubject::OpenTypeReference(open_type_ref) => {
            let option = match (
                ast_field.element.unique,
                ast_field.element.optional,
                &ast_field.element.default,
            ) {
                (true, false, None) => Some(ObjectClassFieldValueOption::Unique),
                (false, true, None) => Some(ObjectClassFieldValueOption::Optional),
                (false, false, Some(default)) => {
                    return Err(Error { kind: ErrorKind::Ast("information object fields referencing an open type field cannot have a default value".to_string()), loc: default.loc });
                }
                (false, false, None) => None,
                _ => {
                    todo!()
                }
            };

            ObjectClassField::Value(ObjectClassFieldValue {
                field_type: ObjectClassFieldValueType::OpenTypeReference(
                    open_type_ref
                        .element
                        .0
                        .as_ref()
                        .map(|name| name.0.element.0.clone())
                        .clone(),
                ),
                option,
            })
        }
    })
}

fn parse_fields(
    parser: &AstParser<'_>,
    class_idents: &[QualifiedIdentifier],
    ast_fields: &[AstElement<AstObjectClassField>],
) -> Result<Vec<(AstElement<String>, ObjectClassField)>> {
    let mut fields = Vec::new();
    for ast_field in ast_fields {
        match &ast_field.element {
            AstObjectClassField::ValueField(ast_field) => {
                let field = parse_class_value_field(parser, class_idents, ast_field)?;
                fields.push((
                    ast_field
                        .element
                        .name
                        .as_ref()
                        .map(|name| name.0.element.0.clone()),
                    field,
                ));
            }
            AstObjectClassField::TypeField(ast_field) => {
                fields.push((
                    ast_field
                        .element
                        .name
                        .as_ref()
                        .map(|name| name.0.element.0.clone()),
                    ObjectClassField::OpenType(ObjectClassFieldType {
                        optional: ast_field.element.optional,
                    }),
                ));
            }
            AstObjectClassField::SetField(ast_field) => {
                let field_type = types::parse_type(
                    parser,
                    &ast_field.element.element_type,
                    &[],
                    TypeContext::Contextless,
                )?;

                let field = match is_type_class(class_idents, &field_type) {
                    Some(class) => ObjectClassField::ObjectSet(ObjectClassFieldObject {
                        class,
                        optional: ast_field.element.optional,
                    }),
                    None => ObjectClassField::ValueSet(ObjectClassFieldValueSet {
                        field_type,
                        option: match (ast_field.element.optional, &ast_field.element.default) {
                            (true, None) => Some(ObjectClassFieldValueSetOption::Optional),
                            (false, Some(default)) => {
                                Some(ObjectClassFieldValueSetOption::Default(default.clone()))
                            }
                            (false, None) => None,
                            (true, Some(default)) => {
                                return Err(Error {
                                    kind: ErrorKind::Ast(
                                        "field cannot be both OPTIONAL and have a DEFAULT"
                                            .to_string(),
                                    ),
                                    loc: default.loc,
                                })
                            }
                        },
                    }),
                };

                fields.push((
                    ast_field
                        .element
                        .name
                        .as_ref()
                        .map(|name| name.0.element.0.clone()),
                    field,
                ));
            }
        }
    }
    Ok(fields)
}

fn parse_syntax_item(item: &AstElement<AstSyntaxItem>) -> ObjectClassSyntaxNode {
    match &item.element {
        AstSyntaxItem::FieldReference(field_ref) => match &field_ref.element {
            AstFieldReference::TypeFieldReference(typeref) => ObjectClassSyntaxNode {
                kind: ObjectClassSyntaxNodeKind::TypeField,
                associated_data: typeref.element.0.element.0.clone(),
            },
            AstFieldReference::ValueFieldReference(valref) => ObjectClassSyntaxNode {
                kind: ObjectClassSyntaxNodeKind::ValueField,
                associated_data: valref.element.0.element.0.clone(),
            },
        },
        AstSyntaxItem::SyntaxTokenLiteral(literal) => ObjectClassSyntaxNode {
            kind: ObjectClassSyntaxNodeKind::TokenLiteral,
            associated_data: literal.element.token.clone(),
        },
    }
}

// TODO: "The Literal 'WITH' must be different from the first Literal of all immediately preceding OptionalGroups."
// for example:
// } WITH SYNTAX {
//     SYNTAX &ExtnType IDENTIFIED BY &id
//     [WITH CRITICALITY &Critical]
// 	   [WITH SUPERCLASSES &Supers]
// }
fn parse_syntax_elements(
    elements: &[AstElement<AstSyntaxListElement>],
) -> Vec<ObjectClassSyntaxNodeGroup> {
    let mut groups = Vec::new();

    for ast_element in elements {
        match &ast_element.element {
            AstSyntaxListElement::RequiredSyntaxElement(required) => {
                groups.push(ObjectClassSyntaxNodeGroup::Required(parse_syntax_item(
                    &required.element.0,
                )));
            }
            AstSyntaxListElement::OptionalSyntaxElement(optional) => {
                let optional_group = parse_syntax_elements(&optional.element.0);
                groups.push(ObjectClassSyntaxNodeGroup::Optional(optional_group));
            }
        }
    }

    groups
}

macro_rules! try_parse {
    ( $ctx:expr, $func:expr ) => {{
        let ctx = $ctx;
        let cursor = ctx.tokens.cursor();
        match $func(ctx) {
            ParseResult::Ok(val) => val,
            ParseResult::Fail(err) | ParseResult::Error(err) => {
                $ctx.tokens.set_cursor(cursor);
                return Err(err);
            }
        }
    }};
}

fn is_type_class(
    class_idents: &[QualifiedIdentifier],
    tagged_type: &TaggedType,
) -> Option<AstElement<QualifiedIdentifier>> {
    match &tagged_type.ty {
        UntaggedType::Reference(typeref) => {
            if class_idents.contains(&typeref.element) {
                Some(typeref.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

fn parse_object_field_reference(
    parser: &AstParser<'_>,
    ast_field_ref: &AstElement<AstObjectFieldReference>,
) -> Result<ObjectFieldReference> {
    let ast_field = match &ast_field_ref.element.field_ref.element {
        AstFieldReference::ValueFieldReference(field) => {
            field.element.0.as_ref().map(|field| field.0.clone())
        }
        AstFieldReference::TypeFieldReference(field) => {
            return Err(Error {
                kind: ErrorKind::Ast(
                    "expecting value field reference, found type field reference".to_string(),
                ),
                loc: field.loc,
            })
        }
    };
    Ok(ObjectFieldReference {
        object_ref: values::resolve_defined_value(parser, &ast_field_ref.element.object_name)?,
        field: ast_field,
    })
}

fn parse_next_syntax_node(
    parser: &AstParser<'_>,
    class: &InformationObjectClass,
    token_stream: &mut TokenStream,
    item: &ObjectClassSyntaxNode,
    fields: &mut Vec<(String, ObjectField)>,
) -> Result<()> {
    if item.kind == ObjectClassSyntaxNodeKind::TokenLiteral {
        let ast_literal = try_parse!(
            ParseContext::new(token_stream),
            AstSyntaxTokenLiteral::parse
        );
        if ast_literal.element.token != item.associated_data {
            return Err(Error {
                kind: ErrorKind::Ast(format!(
                    "expecting syntax literal '{}', but found '{}'",
                    item.associated_data, ast_literal.element.token
                )),
                loc: ast_literal.loc,
            });
        }

        return Ok(());
    }

    let (_, field) = class
        .fields
        .iter()
        .find(|(name, _)| name.element == item.associated_data)
        .expect("syntax refers to a value that does not exist in the CLASS body");
    match field {
        ObjectClassField::ObjectSet(field) => {
            let class =
                resolve_information_object_class(parser, &field.class)?.resolve(parser.context)?;
            let ast_set = try_parse!(ParseContext::new(token_stream), AstObjectSet::parse);
            let set = parse_object_set(parser, class, &ast_set)?;

            fields.push((item.associated_data.clone(), ObjectField::ObjectSet(set)));

            return Ok(());
        }
        ObjectClassField::ValueSet(field) => {
            let target_type = field.field_type.resolve(parser.context)?;
            let ast_set = try_parse!(ParseContext::new(token_stream), AstValueSet::parse);
            let set = parse_value_set(parser, &target_type, &ast_set)?;

            fields.push((item.associated_data.clone(), ObjectField::ValueSet(set)));

            return Ok(());
        }
        _ => match item.kind {
            ObjectClassSyntaxNodeKind::TypeField => {
                let ast_type = try_parse!(ParseContext::new(token_stream), AstType::parse);
                let ty = types::parse_type(parser, &ast_type, &[], TypeContext::Contextless)?;
                fields.push((item.associated_data.clone(), ObjectField::Type(ty)));
            }
            ObjectClassSyntaxNodeKind::ValueField => {
                let field_type = match field {
                    ObjectClassField::Value(value) => match &value.field_type {
                        ObjectClassFieldValueType::TaggedType(tagged_type) => {
                            tagged_type.resolve(parser.context)?
                        }
                        ObjectClassFieldValueType::OpenTypeReference(_) => {
                            todo!("value fields referencing open type fields are not yet supported")
                        }
                        ObjectClassFieldValueType::ObjectClassFieldReference(class_field_ref) => {
                            let ast_object_field_ref = try_parse!(
                                ParseContext::new(token_stream),
                                AstObjectFieldReference::parse
                            );

                            let object_field_ref =
                                parse_object_field_reference(parser, &ast_object_field_ref)?;
                            if class_field_ref.field.element != object_field_ref.field.element {
                                return Err(Error {
                                    kind: ErrorKind::Ast(format!(
                                        "expecting object field '{}', found '{}'",
                                        class_field_ref.field.element,
                                        object_field_ref.field.element,
                                    )),
                                    loc: object_field_ref.field.loc,
                                });
                            }

                            fields.push((
                                item.associated_data.clone(),
                                ObjectField::ObjectFieldReference(object_field_ref),
                            ));
                            return Ok(());
                        }
                    },
                    ObjectClassField::Object(object) => {
                        let object_class = resolve_information_object_class(parser, &object.class)?
                            .resolve(parser.context)?;
                        let ast_object =
                            try_parse!(ParseContext::new(token_stream), AstObject::parse);
                        let object = parse_object(parser, object_class, &ast_object)?;

                        fields.push((item.associated_data.clone(), ObjectField::Object(object)));
                        return Ok(());
                    }
                    _ => unreachable!(),
                };

                let ast_value = try_parse!(ParseContext::new(token_stream), AstValue::parse);
                let value = values::parse_value(
                    parser,
                    ParseValueAssignmentStage::ClassReferenceValues,
                    &ast_value,
                    &field_type,
                )?;
                fields.push((item.associated_data.clone(), ObjectField::Value(value)));
            }
            ObjectClassSyntaxNodeKind::TokenLiteral => unreachable!(),
        },
    }

    Ok(())
}

fn parse_token_stream_from_syntax_node_group(
    parser: &AstParser<'_>,
    class: &InformationObjectClass,
    token_stream: &mut TokenStream,
    syntax_group: &ObjectClassSyntaxNodeGroup,
    fields: &mut Vec<(String, ObjectField)>,
) -> Result<()> {
    match syntax_group {
        ObjectClassSyntaxNodeGroup::Required(item) => {
            parse_next_syntax_node(parser, class, token_stream, item, fields)?
        }
        ObjectClassSyntaxNodeGroup::Optional(groups) => {
            let cursor = token_stream.cursor();
            match parse_token_stream_from_syntax_node_group(
                parser,
                class,
                token_stream,
                &groups[0],
                fields,
            ) {
                Ok(()) => {
                    for group in groups.iter().skip(1) {
                        parse_token_stream_from_syntax_node_group(
                            parser,
                            class,
                            token_stream,
                            group,
                            fields,
                        )?
                    }
                }
                Err(_) => {
                    token_stream.set_cursor(cursor);
                }
            }
        }
    }

    Ok(())
}

pub(crate) fn parse_object_tokens(
    parser: &AstParser<'_>,
    tokens: &AstElement<AstBracedTokenStream>,
    class: &InformationObjectClass,
) -> Result<InformationObject> {
    if !class.parsed {
        panic!("parse_information_object on unparsed class");
    }

    let mut token_stream = TokenStream::from_tokens(tokens.element.0.clone());
    try_parse!(ParseContext::new(&mut token_stream), |ctx| {
        Operator::match_operator(Operator::OpenBrace, OperatorMode::Single, ctx)
    });

    let mut fields = Vec::new();
    for syntax_group in &class.syntax {
        parse_token_stream_from_syntax_node_group(
            parser,
            class,
            &mut token_stream,
            syntax_group,
            &mut fields,
        )?;
    }

    try_parse!(ParseContext::new(&mut token_stream), |ctx| {
        Operator::match_operator(Operator::CloseBrace, OperatorMode::Single, ctx)
    });

    Ok(InformationObject { fields })
}

pub(crate) fn resolve_information_object_class<'a>(
    parser: &'a AstParser<'_>,
    class_ident: &AstElement<QualifiedIdentifier>,
) -> Result<&'a InformationObjectClassReference> {
    let class = parser
        .context
        .lookup_information_object_class(&class_ident.element);
    class.ok_or_else(|| Error {
        kind: ErrorKind::Ast(format!(
            "undefined reference to information object class '{}'",
            class_ident.element,
        )),
        loc: class_ident.loc,
    })
}

pub(crate) fn resolve_information_object_set<'a>(
    parser: &'a AstParser<'_>,
    set_ident: &AstElement<QualifiedIdentifier>,
) -> Result<&'a InformationObjectSet> {
    let set = parser
        .context
        .lookup_information_object_set(&set_ident.element);
    set.ok_or_else(|| Error {
        kind: ErrorKind::Ast(format!(
            "undefined reference to information object set '{}'",
            set_ident.element,
        )),
        loc: set_ident.loc,
    })
}

fn parse_object(
    parser: &AstParser<'_>,
    class: &InformationObjectClass,
    object: &AstElement<AstObject>,
) -> Result<InformationObjectReference> {
    Ok(match &object.element {
        AstObject::BracedTokenStream(tokens) => {
            InformationObjectReference::Value(parse_object_tokens(parser, tokens, class)?)
        }
        AstObject::DefinedValue(objref) => {
            InformationObjectReference::Reference(values::resolve_defined_value(parser, objref)?)
        }
    })
}

fn parse_object_set(
    parser: &AstParser<'_>,
    class: &InformationObjectClass,
    set: &AstElement<AstObjectSet>,
) -> Result<InformationObjectSet> {
    let mut set_elements = Vec::new();

    let ast_elements: Vec<&AstElement<AstObjectSetElement>> = set
        .element
        .element_groups
        .iter()
        .filter_map(|group| match &group.element {
            AstObjectSetElementGroup::Extensible(_) => None,
            AstObjectSetElementGroup::ObjectSetElements(elements) => {
                Some(elements.element.0.iter().collect::<Vec<_>>())
            }
        })
        .flatten()
        .collect();

    for ast_element in ast_elements {
        match &ast_element.element {
            AstObjectSetElement::Object(object) => {
                set_elements.push(ObjectSetElement::Object(parse_object(
                    parser, class, object,
                )?));
            }
            AstObjectSetElement::DefinedType(defined_type) => {
                set_elements.push(ObjectSetElement::ObjectSet(types::resolve_defined_type(
                    parser,
                    defined_type,
                )?));
            }
            AstObjectSetElement::ObjectFieldReference(ofr) => {
                let field_ref = parse_object_field_reference(parser, ofr)?;
                set_elements.push(ObjectSetElement::ObjectFieldReference(field_ref));
            }
        }
    }

    Ok(set_elements)
}

fn parse_value_set(
    parser: &AstParser<'_>,
    target_type: &ResolvedType,
    set: &AstElement<AstValueSet>,
) -> Result<ValueSet> {
    let mut set_elements = Vec::new();

    let ast_elements: Vec<&AstElement<AstValue>> = set
        .element
        .element_groups
        .iter()
        .filter_map(|group| match &group.element {
            AstValueSetElementGroup::Extensible(_) => None,
            AstValueSetElementGroup::ValueSetElements(elements) => {
                Some(elements.element.0.iter().collect::<Vec<_>>())
            }
        })
        .flatten()
        .collect();

    for ast_value in ast_elements {
        set_elements.push(values::parse_value(
            parser,
            ParseValueAssignmentStage::NormalValues,
            ast_value,
            target_type,
        )?);
    }

    Ok(set_elements)
}

pub(crate) fn parse_object_set_assignment(
    parser: &AstParser<'_>,
    ast_set: &AstElement<AstObjectSetAssignment>,
) -> Result<(QualifiedIdentifier, InformationObjectSet)> {
    let ast_name = &ast_set.element.name;
    let name = ast_name.element.0.clone();
    let ident = QualifiedIdentifier::new(parser.module.clone(), name);

    let classes = match &ast_set.element.subject.element {
        AstObjectSetAssignmentSubject::EmptyExtensible(_) => Vec::new(),
        AstObjectSetAssignmentSubject::ObjectSet(set) => {
            let class_ref = ast_set.element.ty.as_ref().map(|class| &class.0);
            let class =
                resolve_information_object_class(parser, &parser.resolve_symbol(&class_ref)?)?
                    .resolve(parser.context)?;

            parse_object_set(parser, class, set)?
        }
    };

    Ok((ident, classes))
}

fn parse_information_object_class(
    parser: &AstParser<'_>,
    class_idents: &[QualifiedIdentifier],
    name: &AstElement<String>,
    ioc: &AstElement<AstInformationObjectClass>,
) -> Result<InformationObjectClass> {
    let fields = parse_fields(parser, class_idents, &ioc.element.fields)?;
    let syntax = parse_syntax_elements(&ioc.element.syntax.element.elements);

    Ok(InformationObjectClass {
        name: name.clone(),
        fields,
        syntax,
        parsed: true,
    })
}

pub enum ObjectClassAssignmentParseMode<'a> {
    NameOnly,
    Class {
        class_idents: &'a [QualifiedIdentifier],
    },
}

pub fn parse_information_object_class_assignment(
    parser: &AstParser<'_>,
    type_assignment: &AstElement<AstTypeAssignment>,
    mode: ObjectClassAssignmentParseMode,
) -> Result<Option<(QualifiedIdentifier, Option<InformationObjectClassReference>)>> {
    let ast_name = &type_assignment.element.name;
    let name = ast_name.element.0.clone();
    let ident = QualifiedIdentifier::new(parser.module.clone(), name);

    match &type_assignment.element.subject.element {
        AstTypeAssignmentSubject::Type(_) => Ok(None),
        ast_class_assignment => match mode {
            ObjectClassAssignmentParseMode::NameOnly => Ok(Some((ident, None))),
            ObjectClassAssignmentParseMode::Class { class_idents } => match ast_class_assignment {
                AstTypeAssignmentSubject::Type(_) => unreachable!(),
                AstTypeAssignmentSubject::AbstractSyntax(token) => {
                    // clone ABSTRACT-SYNTAX instead of being a reference to it
                    // in the future, this should somehow be a distinct type
                    // maybe assign a unique ID?

                    Ok(Some((
                        ident,
                        Some(InformationObjectClassReference::Reference(
                            token
                                .as_ref()
                                .map(|_| ABSTRACT_SYNTAX_IDENT.clone())
                                .clone(),
                        )),
                    )))
                }
                AstTypeAssignmentSubject::TypeIdentifier(token) => Ok(Some((
                    ident,
                    Some(InformationObjectClassReference::Reference(
                        token
                            .as_ref()
                            .map(|_| TYPE_IDENTIFIER_IDENT.clone())
                            .clone(),
                    )),
                ))),
                AstTypeAssignmentSubject::InformationObjectClass(ioc) => {
                    let class = parse_information_object_class(
                        parser,
                        class_idents,
                        &ast_name.as_ref().map(|name| name.0.clone()),
                        ioc,
                    )?;
                    Ok(Some((
                        ident,
                        Some(InformationObjectClassReference::Class(class)),
                    )))
                }
            },
        },
    }
}
