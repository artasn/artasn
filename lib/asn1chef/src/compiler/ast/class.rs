use crate::{
    compiler::{ast::constraints, parser::*},
    module::{ModuleIdentifier, QualifiedIdentifier},
    types::*,
    values::{InformationObject, InformationObjectReference, ObjectField, ObjectFieldReference},
};

use super::{
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

fn parse_fields(
    parser: &AstParser<'_>,
    class_idents: &[QualifiedIdentifier],
    ast_fields: &[AstElement<AstObjectClassField>],
) -> Result<Vec<(AstElement<String>, ObjectClassField)>> {
    let mut fields = Vec::new();
    for ast_field in ast_fields {
        match &ast_field.element {
            AstObjectClassField::ValueField(value) => {
                let field = match &value.element.subject.element {
                    AstValueFieldSubject::Type(ast_field_type) => {
                        let mut field_type = types::parse_type(
                            parser,
                            ast_field_type,
                            &[],
                            TypeContext::Contextless,
                        )?;

                        match is_type_class(class_idents, &field_type) {
                            Some(class) => ObjectClassField::Object(ObjectClassFieldObject {
                                class,
                                optional: value.element.optional,
                            }),
                            None => {
                                match &field_type.ty {
                                    UntaggedType::ObjectClassField(ocf) => {
                                        if ocf.kind == ObjectClassFieldReferenceKind::OpenType {
                                            return Err(Error {
                                            kind: ErrorKind::Ast("value field type cannot be a reference to an open type field".to_string()),
                                            loc: ocf.field.loc,
                                        });
                                        }

                                        if value.element.unique {
                                            return Err(Error {
                                            kind: ErrorKind::Ast("UNIQUE is invalid for reference-type value field".to_string()),
                                            loc: value.loc,
                                        });
                                        }

                                        if let Some(default) = &value.element.default {
                                            return Err(Error {
                                            kind: ErrorKind::Ast("DEFAULT is invalid for reference-type value field".to_string()),
                                            loc: default.loc,
                                        });
                                        }

                                        ObjectClassField::Value(ObjectClassFieldValue {
                                            field_type:
                                                ObjectClassFieldValueType::ObjectClassFieldReference(
                                                    ocf.clone(),
                                                ),
                                            option: if value.element.optional {
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
                                        constraints::apply_pending_constraint(
                                            &mut field_type,
                                            constraint,
                                        );

                                        let option = match (
                                            value.element.unique,
                                            value.element.optional,
                                            &value.element.default,
                                        ) {
                                            (true, false, None) => {
                                                Some(ObjectClassFieldValueOption::Unique)
                                            }
                                            (false, true, None) => {
                                                Some(ObjectClassFieldValueOption::Optional)
                                            }
                                            (false, false, Some(default)) => {
                                                Some(ObjectClassFieldValueOption::Default(
                                                    values::parse_value(
                                                        parser,
                                                        ParseValueAssignmentStage::NormalValues,
                                                        default,
                                                        &target_type,
                                                    )?,
                                                ))
                                            }
                                            (false, false, None) => None,
                                            _ => {
                                                todo!()
                                            }
                                        };

                                        ObjectClassField::Value(ObjectClassFieldValue {
                                            field_type: ObjectClassFieldValueType::TaggedType(
                                                field_type,
                                            ),
                                            option,
                                        })
                                    }
                                }
                            }
                        }
                    }
                    AstValueFieldSubject::OpenTypeReference(open_type_ref) => {
                        let option = match (
                            value.element.unique,
                            value.element.optional,
                            &value.element.default,
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
                };

                fields.push((
                    value
                        .element
                        .name
                        .as_ref()
                        .map(|name| name.0.element.0.clone()),
                    field,
                ));
            }
            AstObjectClassField::TypeField(open_type) => {
                fields.push((
                    open_type
                        .element
                        .name
                        .as_ref()
                        .map(|name| name.0.element.0.clone()),
                    ObjectClassField::OpenType(ObjectClassFieldType {
                        optional: open_type.element.optional,
                    }),
                ));
            }
            AstObjectClassField::ObjectSetField(object) => {
                fields.push((
                    object
                        .element
                        .name
                        .as_ref()
                        .map(|name| name.0.element.0.clone()),
                    ObjectClassField::ObjectSet(ObjectClassFieldObject {
                        class: parser
                            .resolve_symbol(&object.element.class.as_ref().map(|class| &class.0)),
                        optional: object.element.optional,
                    }),
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

fn parse_syntax(syntax: &AstElement<AstSyntaxList>) -> Vec<ObjectClassSyntaxNodeGroup> {
    let mut groups = Vec::new();

    for ast_element in &syntax.element.elements {
        match &ast_element.element {
            AstSyntaxListElement::RequiredSyntaxElement(required) => {
                groups.push(ObjectClassSyntaxNodeGroup::Required(parse_syntax_item(
                    &required.element.0,
                )));
            }
            AstSyntaxListElement::OptionalSyntaxElement(optional) => {
                let mut optional_group = Vec::new();
                for item in &optional.element.0 {
                    optional_group.push(parse_syntax_item(item));
                }
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

fn is_type_class<'a>(
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
                        ObjectClassFieldValueType::ObjectClassFieldReference(field_ref) => {
                            let ast_field_ref = try_parse!(
                                ParseContext::new(token_stream),
                                AstObjectFieldReference::parse
                            );
                            // ast_field_ref.element.

                            fields.push((
                                item.associated_data.clone(),
                                ObjectField::ObjectFieldReference(ObjectFieldReference {
                                    object_ref: values::resolve_valuereference(
                                        parser,
                                        &ast_field_ref.element.object_name,
                                    ),
                                    field: match &ast_field_ref.element.field_ref.element {
                                        AstFieldReference::ValueFieldReference(field) => {
                                            field.element.0.as_ref().map(|field| field.0.clone())
                                        }
                                        AstFieldReference::TypeFieldReference(field) => {
                                            return Err(Error {
                                                kind: ErrorKind::Ast("expecting value field reference, found type field reference".to_string()),
                                                loc: field.loc
                                            })
                                        }
                                    },
                                }),
                            ));
                            return Ok(());
                        }
                    },
                    ObjectClassField::Object(_) => {
                        let valref =
                            try_parse!(ParseContext::new(token_stream), AstValueReference::parse);

                        fields.push((
                            item.associated_data.clone(),
                            ObjectField::Object(InformationObjectReference::Reference(
                                values::resolve_valuereference(parser, &valref),
                            )),
                        ));
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

pub(crate) fn parse_information_object(
    parser: &AstParser<'_>,
    tokens: &AstElement<AstBracedTokenStream>,
    class: &InformationObjectClass,
) -> Result<InformationObject> {
    let mut token_stream = TokenStream::from_tokens(tokens.element.0.clone());
    try_parse!(ParseContext::new(&mut token_stream), |ctx| {
        Operator::match_operator(Operator::OpenBrace, ctx)
    });

    let mut fields = Vec::new();
    for syntax_group in &class.syntax {
        match syntax_group {
            ObjectClassSyntaxNodeGroup::Required(item) => {
                parse_next_syntax_node(parser, class, &mut token_stream, item, &mut fields)?
            }
            ObjectClassSyntaxNodeGroup::Optional(items) => {
                let cursor = token_stream.cursor();
                match parse_next_syntax_node(
                    parser,
                    class,
                    &mut token_stream,
                    &items[0],
                    &mut fields,
                ) {
                    Ok(()) => {
                        for item in items.iter().skip(1) {
                            parse_next_syntax_node(
                                parser,
                                class,
                                &mut token_stream,
                                item,
                                &mut fields,
                            )?
                        }
                    }
                    Err(_) => {
                        token_stream.set_cursor(cursor);
                        continue;
                    }
                }
            }
        }
    }

    try_parse!(ParseContext::new(&mut token_stream), |ctx| {
        Operator::match_operator(Operator::CloseBrace, ctx)
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

fn parse_object_set(
    parser: &AstParser<'_>,
    class: &InformationObjectClass,
    set: &AstElement<AstObjectSet>,
) -> Result<Vec<InformationObjectReference>> {
    let mut classes = Vec::new();
    for ast_element in &set.element.elements {
        match &ast_element.element {
            AstObjectSetElement::BracedTokenStream(tokens) => {
                classes.push(InformationObjectReference::Value(parse_information_object(
                    parser, tokens, class,
                )?));
            }
            AstObjectSetElement::ValueReference(valref) => {
                classes.push(InformationObjectReference::Reference(
                    values::resolve_valuereference(parser, valref),
                ));
            }
        }
    }

    Ok(classes)
}

pub(crate) fn parse_object_set_assignment(
    parser: &AstParser<'_>,
    ast_set: &AstElement<AstObjectSetAssignment>,
) -> Result<(QualifiedIdentifier, Vec<InformationObjectReference>)> {
    let ast_name = &ast_set.element.name;
    let name = ast_name.element.0.clone();
    let ident = QualifiedIdentifier::new(parser.module.clone(), name);

    let classes = match &ast_set.element.subject.element {
        AstObjectSetAssignmentSubject::EmptyExtensible(_) => Vec::new(),
        AstObjectSetAssignmentSubject::ObjectSet(set) => {
            let class_ref = ast_set.element.ty.as_ref().map(|class| &class.0);
            let class =
                resolve_information_object_class(parser, &parser.resolve_symbol(&class_ref))?
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
    let syntax = parse_syntax(&ioc.element.syntax);

    Ok(InformationObjectClass {
        name: name.clone(),
        fields,
        syntax,
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
