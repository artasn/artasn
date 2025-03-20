use crate::{
    compiler::{ast::constraints, parser::*},
    module::QualifiedIdentifier,
    types::*,
    values::{InformationObjectClassValue, ObjectClassReference, ObjectClassValueField},
};

use super::{
    types::{self, TypeContext},
    values::{self, ParseValueAssignmentStage},
    AstParser,
};

fn parse_fields(
    parser: &AstParser<'_>,
    ast_fields: &[AstElement<AstObjectClassField>],
) -> Result<Vec<(AstElement<String>, ObjectClassField)>> {
    let mut fields = Vec::new();
    for ast_field in ast_fields {
        match &ast_field.element {
            AstObjectClassField::ValueField(value) => {
                let mut field_type =
                    types::parse_type(parser, &value.element.ty, &[], TypeContext::Contextless)?;

                // TODO: should we just move this entire stage to be after we parse all types?
                let target_type = field_type.resolve(parser.context)?;
                let constraint = constraints::parse_type_constraint(
                    parser,
                    &value.element.ty,
                    &target_type,
                    &[],
                )?;
                constraints::apply_pending_constraint(&mut field_type, constraint);

                fields.push((
                    value
                        .element
                        .name
                        .as_ref()
                        .map(|name| name.0.element.0.clone()),
                    ObjectClassField::Value(ObjectClassFieldValue {
                        option: match (
                            value.element.unique,
                            value.element.optional,
                            &value.element.default,
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
                            _ => {
                                todo!()
                            }
                        },
                        field_type,
                    }),
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

fn parse_next_syntax_node(
    parser: &AstParser<'_>,
    class: &InformationObjectClass,
    token_stream: &mut TokenStream,
    item: &ObjectClassSyntaxNode,
    fields: &mut Vec<(String, ObjectClassValueField)>,
) -> Result<()> {
    match item.kind {
        ObjectClassSyntaxNodeKind::TypeField => {
            let ast_type = try_parse!(ParseContext::new(token_stream), AstType::parse);
            let ty = types::parse_type(parser, &ast_type, &[], TypeContext::Contextless)?;
            fields.push((
                item.associated_data.clone(),
                ObjectClassValueField::Type(ty),
            ));
        }
        ObjectClassSyntaxNodeKind::ValueField => {
            let (_, field) = class
                .fields
                .iter()
                .find(|(name, _)| name.element == item.associated_data)
                .expect("syntax refers to a value that does not exist in the CLASS body");
            let field_type = match field {
                ObjectClassField::Value(value) => value.field_type.resolve(parser.context)?,
                ObjectClassField::OpenType(_) => unreachable!(),
            };

            let ast_value = try_parse!(ParseContext::new(token_stream), AstValue::parse);
            let value = values::parse_value(
                parser,
                ParseValueAssignmentStage::NormalValues,
                &ast_value,
                &field_type,
            )?;
            fields.push((
                item.associated_data.clone(),
                ObjectClassValueField::Value(value),
            ));
        }
        ObjectClassSyntaxNodeKind::TokenLiteral => {
            let ast_literal = try_parse!(
                ParseContext::new(token_stream),
                AstSyntaxTokenLiteral::parse
            );
            if ast_literal.element.token != item.associated_data {
                return Err(Error {
                    kind: ErrorKind::Ast(format!(
                        "expecting '{}', but found '{}'",
                        item.associated_data, ast_literal.element.token
                    )),
                    loc: ast_literal.loc,
                });
            }
        }
    }

    Ok(())
}

pub(crate) fn parse_information_object_class_value(
    parser: &AstParser<'_>,
    tokens: &AstElement<AstBracedTokenStream>,
    class: &InformationObjectClass,
) -> Result<InformationObjectClassValue> {
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
                    Err(_) => continue,
                }
            }
        }
    }

    try_parse!(ParseContext::new(&mut token_stream), |ctx| {
        Operator::match_operator(Operator::CloseBrace, ctx)
    });

    Ok(InformationObjectClassValue { fields })
}

pub(crate) fn parse_object_class_set_assignment(
    parser: &AstParser<'_>,
    ast_set: &AstElement<AstObjectClassSetAssignment>,
) -> Result<(QualifiedIdentifier, Vec<ObjectClassReference>)> {
    let ast_name = &ast_set.element.name;
    let name = ast_name.element.0.clone();
    let ident = QualifiedIdentifier::new(parser.module.clone(), name);

    let mut classes = Vec::new();
    for ast_element in &ast_set.element.elements {
        match &ast_element.element {
            AstObjectClassSetElement::BracedTokenStream(tokens) => {
                let class = parser.context.lookup_information_object_class(
                    &types::resolve_typereference(
                        parser,
                        &ast_set
                            .element
                            .ty
                            .as_ref()
                            .map(|name| AstTypeReference(name.0.clone())),
                    )
                    .element,
                );
                let class = class.ok_or_else(|| Error {
                    kind: ErrorKind::Ast(format!(
                        "undefined reference to information object class '{}'",
                        ast_set.element.ty.element.0
                    )),
                    loc: ast_set.element.ty.loc,
                })?;
                classes.push(ObjectClassReference::Value(
                    parse_information_object_class_value(parser, tokens, class)?,
                ));
            }
            AstObjectClassSetElement::ValueReference(valref) => {
                classes.push(ObjectClassReference::Reference(
                    values::resolve_valuereference(parser, valref),
                ));
            }
        }
    }

    Ok((ident, classes))
}

pub(crate) fn parse_information_object_class(
    parser: &AstParser<'_>,
    ioc: &AstElement<AstInformationObjectClass>,
) -> Result<InformationObjectClass> {
    let fields = parse_fields(parser, &ioc.element.fields)?;
    let syntax = parse_syntax(&ioc.element.syntax);

    Ok(InformationObjectClass { fields, syntax })
}
