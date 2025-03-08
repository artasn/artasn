use crate::{
    compiler::{context::DeclaredValue, parser::*},
    types::*,
};

use super::AstParser;

// See X.680 clauses 25.6, 25.6.1, and 27.3 to see exactly what is being enforced here.
fn verify_unique_component_tags(parser: &AstParser<'_>, structure: &Structure) -> Result<()> {
    if structure.components.len() <= 1 {
        return Ok(());
    }

    match structure.ty {
        TagType::Sequence => {
            struct ComponentData<'a> {
                pub name: &'a AstElement<String>,
                pub class: Class,
                pub num: u16,
            }

            let mut consecutive_optionals: Vec<ComponentData<'_>> = Vec::new();
            for i in 0..structure.components.len() {
                let component = &structure.components[i];
                let tag = component.component_type.resolve(parser.context)?.tag;

                let illegal_component = 'block: {
                    for data in &consecutive_optionals {
                        if data.class == tag.class && data.num == tag.num {
                            break 'block Some(data);
                        }
                    }

                    None
                };
                if let Some(illegal_component) = illegal_component {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "SEQUENCE component '{}' and component '{}' must have distinct tags",
                            illegal_component.name.element, component.name.element
                        )),
                        loc: illegal_component.name.loc,
                    });
                }

                if component.optional || component.default_value.is_some() {
                    consecutive_optionals.push(ComponentData {
                        name: &component.name,
                        class: tag.class,
                        num: tag.num,
                    });
                } else {
                    consecutive_optionals.clear();
                }
            }
        }
        TagType::Set => {
            for i in 0..structure.components.len() {
                for j in 0..structure.components.len() {
                    if i == j {
                        continue;
                    }

                    let a = &structure.components[i];
                    let b = &structure.components[j];
                    let tag_a = a.component_type.resolve(parser.context)?.tag;
                    let tag_b = b.component_type.resolve(parser.context)?.tag;
                    if tag_a.class == tag_b.class && tag_a.num == tag_b.num {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "SET component '{}' and component '{}' must have distinct tags",
                                a.name.element, b.name.element
                            )),
                            loc: a.name.loc,
                        });
                    }
                }
            }
        }
        _ => unreachable!(),
    }

    Ok(())
}

pub fn verify_type(parser: &AstParser<'_>, declared_type: &TaggedType) -> Result<()> {
    let resolved_ty = declared_type.resolve(parser.context)?;
    if let BuiltinType::Structure(structure) = &resolved_ty.ty {
        verify_unique_component_tags(parser, structure)?
    }

    Ok(())
}

pub fn verify_value(parser: &AstParser<'_>, declared_value: &DeclaredValue) -> Result<()> {
    let resolved_ty = declared_value.ty.resolve(parser.context)?;
    resolved_ty
        .ty
        .ensure_satisfied_by_value(parser.context, &declared_value.value)?;

    Ok(())
}
