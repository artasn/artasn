use std::collections::HashSet;

use crate::{
    compiler::{context::DeclaredValue, parser::*, Context},
    types::*,
};

#[derive(Debug, Clone, PartialEq, Eq)]
struct TagData {
    pub class: Class,
    pub num: u16,
}

#[derive(Debug)]
struct ComponentData {
    pub name: AstElement<String>,
    pub tag_series: Vec<TagData>,
}

// See X.680 clauses 25.6, 25.6.1, and 27.3 to see exactly what is being enforced here.
fn verify_unique_component_tags(context: &Context, structure: &Structure) -> Result<()> {
    if structure.components.len() <= 1 {
        return Ok(());
    }

    match structure.ty {
        TagType::Sequence => {
            let mut consecutive_optionals: Vec<ComponentData> = Vec::new();
            for i in 0..structure.components.len() {
                let component = &structure.components[i];
                match &component.component_type.ty {
                    UntaggedType::ObjectClassField(_) => {
                        // TODO: implement this
                        continue;
                    }
                    _ => {
                        let possible_tags = component
                            .component_type
                            .resolve(context)?
                            .get_possible_tags(context)?;

                        let illegal_component = 'block: {
                            for data in &consecutive_optionals {
                                if possible_tags
                                    .iter()
                                    .find(|(tag, _)| {
                                        tag.class == data.tag_series[0].class
                                            && tag.num == data.tag_series[0].num
                                    })
                                    .is_some()
                                {
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
                            for (tag, _) in possible_tags {
                                consecutive_optionals.push(ComponentData {
                                    name: component.name.clone(),
                                    tag_series: vec![TagData {
                                        class: tag.class,
                                        num: tag.num,
                                    }],
                                });
                            }
                        } else {
                            consecutive_optionals.clear();
                        }
                    }
                }
            }
        }
        TagType::Set => {
            let all_tags = structure
                .components
                .iter()
                .map(|component| {
                    Ok((
                        component.name.clone(),
                        component
                            .component_type
                            .resolve(context)
                            .map(|ty| ty.get_possible_tags(context))?
                            .map(|tags| {
                                tags.into_iter()
                                    .map(|(tag, _)| (tag.class, tag.num))
                                    .collect::<HashSet<(Class, u16)>>()
                            })?,
                    ))
                })
                .collect::<Result<Vec<(AstElement<String>, HashSet<(Class, u16)>)>>>()?;

            for i in 0..all_tags.len() {
                for j in 0..all_tags.len() {
                    if i == j {
                        continue;
                    }

                    let (name_a, tags_a) = &all_tags[i];
                    let (name_b, tags_b) = &all_tags[j];
                    if tags_a.intersection(tags_b).count() > 0 {
                        return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "SET component '{}' and component '{}' must have distinct tags",
                                name_a.element, name_b.element
                            )),
                            loc: name_a.loc,
                        });
                    }
                }
            }
        }
        _ => unreachable!(),
    }

    Ok(())
}

fn get_choice_alternative_leaves(
    context: &Context,
    leaves: &mut Vec<ComponentData>,
    choice: &Choice,
    parent_tag_series: Vec<TagData>,
) -> Result<()> {
    for alternative in &choice.alternatives {
        let alternative_ty = alternative.alternative_type.resolve(context)?;
        match &alternative_ty.ty {
            BuiltinType::Choice(choice) => {
                let mut tag_series = parent_tag_series.clone();
                if let Some(tag) = &alternative_ty.tag {
                    tag_series.push(TagData {
                        class: tag.class,
                        num: tag.num,
                    });
                }
                get_choice_alternative_leaves(context, leaves, choice, tag_series)?
            }
            _ => {
                let mut tag_series = parent_tag_series.clone();
                let tag = alternative_ty
                    .tag
                    .expect("tag is None but type is not CHOICE");
                tag_series.push(TagData {
                    class: tag.class,
                    num: tag.num,
                });
                leaves.push(ComponentData {
                    name: alternative.name.clone(),
                    tag_series,
                })
            }
        }
    }

    Ok(())
}

fn verify_unique_alternative_tags(context: &Context, choice: &Choice) -> Result<()> {
    let mut leaves = Vec::new();
    get_choice_alternative_leaves(context, &mut leaves, choice, Vec::new())?;
    for i in 0..leaves.len() {
        for j in 0..leaves.len() {
            if i == j {
                continue;
            }

            let a = &leaves[i];
            let b = &leaves[j];
            if a.tag_series == b.tag_series {
                return Err(Error {
                    kind: ErrorKind::Ast(format!(
                        "CHOICE alternative '{}' and alternative '{}' must have distinct tags",
                        a.name.element, b.name.element
                    )),
                    loc: a.name.loc,
                });
            }
        }
    }

    Ok(())
}

pub fn verify_type(context: &Context, declared_type: &TaggedType) -> Result<()> {
    let resolved_ty = declared_type.resolve(context)?;
    match &resolved_ty.ty {
        BuiltinType::Choice(choice) => {
            verify_unique_alternative_tags(context, choice)?;
        }
        BuiltinType::Structure(structure) => verify_unique_component_tags(context, structure)?,
        _ => (),
    }

    Ok(())
}

pub fn verify_value(context: &Context, declared_value: &DeclaredValue) -> Result<()> {
    let resolved_ty = declared_value.ty.resolve(context)?;
    resolved_ty.ty.ensure_satisfied_by_value(
        context,
        &declared_value.value,
        resolved_ty.constraint.as_ref(),
    )?;

    Ok(())
}
