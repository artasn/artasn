use crate::{
    compiler::{
        oid_tree::{self, *},
        parser::*,
    },
    module::ModuleIdentifier,
    types::TagType,
    values::*,
};

use super::{values, AstParser};

fn lookup_root_oid_component_str(component: &str) -> Option<&OidTreeNode> {
    oid_tree::lookup_root_node(component)
}

fn big_uint_to_u64(big: &AstElement<AstNumber>) -> Result<u64> {
    TryInto::<u64>::try_into(&big.element.0).map_err(|_| Error {
        kind: ErrorKind::Ast("number is too large for an object identifier node".to_string()),
        loc: big.loc,
    })
}

fn named_number_to_u64(named_num: &AstElement<AstNamedNumber>) -> Result<u64> {
    match &named_num.element.num.element {
        AstIntegerValueReference::IntegerValue(int) => {
            if int.element.sign.is_some() {
                return Err(Error {
                    kind: ErrorKind::Ast("node must be an unsigned integer".to_string()),
                    loc: int.loc,
                });
            }

            Ok(big_uint_to_u64(&int.element.value)?)
        }
        AstIntegerValueReference::DefinedValue(valref) => Err(Error {
            kind: ErrorKind::Ast(format!(
                "expecting INTEGER literal, but found identifier '{}'",
                valref.element,
            )),
            loc: valref.loc,
        }),
    }
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
                    .map(|elem| {
                        Ok(match &elem.element {
                            AstDefinitiveOidComponent::Number(num) => {
                                OidTreeNodeSearch::Number(big_uint_to_u64(num)?)
                            }
                            AstDefinitiveOidComponent::NamedNumber(named_num) => {
                                OidTreeNodeSearch::Number(named_number_to_u64(named_num)?)
                            }
                            AstDefinitiveOidComponent::ValueReference(val_ref) => {
                                OidTreeNodeSearch::Name(
                                    val_ref.as_ref().map(|val_ref| val_ref.0.clone()),
                                )
                            }
                        })
                    })
                    .collect::<Result<Vec<OidTreeNodeSearch>>>()?;
                oid_tree::search(searches)?
            }),
            None => None,
        },
    })
}

fn parse_object_identifier_component_valref(
    parser: &AstParser<'_>,
    valref: &AstElement<AstDefinedValue>,
    ty: TagType,
) -> Result<ObjectIdentifierComponent> {
    if ty == TagType::ObjectIdentifier && valref.element.external_module.is_none() {
        if let Some(node) = lookup_root_oid_component_str(&valref.element.value.element.0) {
            return Ok(ObjectIdentifierComponent::IntegerLiteral {
                name: Some(valref.element.value.as_ref().map(|name| name.0.clone())),
                int: AstElement::new(node.node, valref.loc),
            });
        }
    }
    let resolved_value = values::resolve_defined_value(parser, valref)?;
    Ok(ObjectIdentifierComponent::ValueReference(resolved_value))
}

pub fn parse_object_identifier(
    parser: &AstParser<'_>,
    object_id: &AstElement<AstObjectIdentifierValue>,
    ty: TagType,
) -> Result<ObjectIdentifier> {
    Ok(ObjectIdentifier {
        ty,
        components: object_id
            .element
            .components
            .iter()
            .map(|elem| {
                Ok(match &elem.element {
                    AstObjectIdentifierComponent::Number(num) => {
                        ObjectIdentifierComponent::IntegerLiteral {
                            name: None,
                            int: AstElement::new(big_uint_to_u64(num)?, num.loc),
                        }
                    }
                    AstObjectIdentifierComponent::NamedNumber(named_num) => {
                        ObjectIdentifierComponent::IntegerLiteral {
                            name: Some(named_num.element.name.as_ref().map(|name| name.0.clone())),
                            int: AstElement::new(
                                named_number_to_u64(named_num)?,
                                named_num.element.num.loc,
                            ),
                        }
                    }
                    AstObjectIdentifierComponent::DefinedValue(val_ref) => {
                        parse_object_identifier_component_valref(parser, val_ref, ty)?
                    }
                })
            })
            .collect::<Result<Vec<ObjectIdentifierComponent>>>()?,
    })
}
