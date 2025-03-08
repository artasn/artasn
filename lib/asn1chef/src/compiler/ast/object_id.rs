use crate::{
    compiler::{
        oid_tree::{self, *},
        parser::*,
    },
    module::ModuleIdentifier,
    values::*,
};

use super::{values, AstParser};

fn lookup_root_oid_component_str(component: &str) -> Option<&OidTreeNode> {
    oid_tree::lookup_root_node(component)
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
    parser: &AstParser<'_>,
    value: &AstElement<AstValueReference>,
) -> ObjectIdentifierComponent {
    lookup_root_oid_component_str(&value.element.0).map_or_else(
        || ObjectIdentifierComponent::ValueReference(values::parse_valuereference(parser, value)),
        |lit| ObjectIdentifierComponent::IntegerLiteral(AstElement::new(lit.node, value.loc)),
    )
}

pub fn parse_object_identifier(
    parser: &AstParser<'_>,
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
                        parse_object_identifier_component_valref(parser, val_ref)
                    }
                    AstObjectIdentifierComponent::DefinedValue(defined_val) => {
                        parse_object_identifier_component_valref(parser, &defined_val.element.value)
                    }
                })
            })
            .collect::<Result<Vec<ObjectIdentifierComponent>>>()?,
    ))
}
