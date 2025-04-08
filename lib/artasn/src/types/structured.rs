use std::{cell::RefCell, fmt::Display};

use crate::{
    compiler::{
        ast::types::LazyParsedDefaultValue,
        parser::{AstElement, Result},
        Context,
    },
    module::QualifiedIdentifier,
};

use super::*;

pub trait ComponentLike: Clone {
    fn name(&self) -> &AstElement<String>;
    fn component_type(&self) -> &TaggedType;
}

#[derive(Debug, Clone)]
pub enum StructureComponent {
    Named(NamedStructureComponent),
    ComponentsOf(AstElement<Box<TaggedType>>),
}

#[derive(Debug, Clone)]
pub struct NamedStructureComponent {
    pub name: AstElement<String>,
    pub component_type: Box<TaggedType>,
    pub optional: bool,
    pub default_value: Option<LazyParsedDefaultValue>,
}

impl ComponentLike for NamedStructureComponent {
    fn name(&self) -> &AstElement<String> {
        &self.name
    }

    fn component_type(&self) -> &TaggedType {
        &self.component_type
    }
}

#[derive(Debug, Clone)]
pub struct ObjectClassFieldReference {
    pub class_type: AstElement<QualifiedIdentifier>,
    pub kind: ObjectClassFieldReferenceKind,
    pub field: AstElement<String>,
}

impl Display for ObjectClassFieldReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.class_type.element.name)?;
        f.write_str(".&")?;
        f.write_str(&self.field.element)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectClassFieldReferenceKind {
    TypeLike,
    ValueLike,
}

#[derive(Debug, Clone)]
pub struct Structure {
    /// Always either Sequence or Set
    pub ty: TagType,
    /// `true` is this structure is a candidate for `AUTOMATIC` tagging.
    /// i.e. the module has `AUTOMATIC` TagDefault, and none of the structure's top-level components have defined tags.
    /// Otherwise `false` (i.e. no `AUTOMATIC` TagDefault and/or a component has a defined tag).
    pub automatic_tagging: bool,

    pub(crate) components: Vec<StructureComponent>,
    resolved_components: RefCell<Option<Vec<NamedStructureComponent>>>,
}

impl Structure {
    pub fn new(
        ty: TagType,
        automatic_tagging: bool,
        components: Vec<StructureComponent>,
    ) -> Structure {
        Structure {
            ty,
            automatic_tagging,
            components,
            resolved_components: RefCell::new(None),
        }
    }

    pub fn resolve_components(&self, context: &Context) -> Result<Vec<NamedStructureComponent>> {
        if self.resolved_components.borrow().is_none() {
            let mut components = Vec::with_capacity(self.components.len());
            self.resolve_components_internal(context, &mut components, self.automatic_tagging, 0)?;
            *self.resolved_components.borrow_mut() = Some(components);
        }

        Ok(self.get_resolved_components())
    }

    pub(crate) fn get_resolved_components(&self) -> Vec<NamedStructureComponent> {
        self.resolved_components
            .borrow()
            .as_ref()
            .cloned()
            .expect("resolved_components is not yet initialized")
    }

    pub(crate) fn set_resolved_components(&self, components: Vec<NamedStructureComponent>) {
        *self.resolved_components.borrow_mut() = Some(components);
    }

    fn resolve_components_internal(
        &self,
        context: &Context,
        components: &mut Vec<NamedStructureComponent>,
        automatic_tagging: bool,
        mut component_offset: u16,
    ) -> Result<u16> {
        let start_offset = component_offset;
        for component in &self.components {
            match component {
                StructureComponent::Named(component) => {
                    let mut component = component.clone();
                    resolve_structure_tag(
                        context,
                        &mut component.component_type,
                        automatic_tagging,
                        component_offset,
                    )?;
                    component_offset += 1;
                    components.push(component);
                }
                StructureComponent::ComponentsOf(of) => {
                    let resolved = of.element.resolve(context)?;
                    match &resolved.ty {
                        BuiltinType::Structure(structure) if structure.ty == self.ty => {
                            let (automatic_tagging, inner_component_offset) = if !automatic_tagging
                            {
                                match &of.element.ty {
                                    UntaggedType::BuiltinType(_) => {
                                        (false, components.len() as u16)
                                    }
                                    UntaggedType::Reference(_) => (structure.automatic_tagging, 0),
                                    other => panic!("{other:#?}"),
                                }
                            } else {
                                (true, component_offset)
                            };
                            let tags_written = structure.resolve_components_internal(
                                context,
                                components,
                                automatic_tagging,
                                inner_component_offset,
                            )?;
                            if self.automatic_tagging {
                                component_offset += tags_written;
                            }
                        }
                        other => {
                            return Err(Error {
                                kind: ErrorKind::Ast(format!(
                                    "expecting {}, but found {}",
                                    self.ty, other
                                )),
                                loc: of.loc,
                            })
                        }
                    }
                }
            }
        }

        Ok(component_offset - start_offset)
    }
}

#[derive(Debug, Clone)]
pub struct StructureOf {
    // Always either Sequence or Set
    pub ty: TagType,
    pub component_type: Box<TaggedType>,
}

#[derive(Debug, Clone)]
pub struct ChoiceAlternative {
    pub name: AstElement<String>,
    pub alternative_type: Box<TaggedType>,
}

impl ComponentLike for ChoiceAlternative {
    fn name(&self) -> &AstElement<String> {
        &self.name
    }

    fn component_type(&self) -> &TaggedType {
        &self.alternative_type
    }
}

#[derive(Debug, Clone)]
pub struct Choice {
    pub alternatives: Vec<ChoiceAlternative>,
    /// See [`Structure::automatic_tagging`].
    pub automatic_tagging: bool,
}

impl Choice {
    pub fn resolve_alternatives(&self, context: &Context) -> Result<Vec<ChoiceAlternative>> {
        let mut alternatives = Vec::with_capacity(self.alternatives.len());
        for alternative in &self.alternatives {
            let mut alternative = alternative.clone();
            resolve_structure_tag(
                context,
                &mut alternative.alternative_type,
                self.automatic_tagging,
                alternatives.len() as u16,
            )?;
            alternatives.push(alternative);
        }

        Ok(alternatives)
    }
}

fn resolve_structure_tag(
    context: &Context,
    tagged_type: &mut TaggedType,
    automatic_tagging: bool,
    component_index: u16,
) -> Result<()> {
    let mut tag = if automatic_tagging {
        // Automatic is not always Implicit, there are special cases where it means Explicit
        // see below for how this is done with CHOICE and open types
        Some(Tag::new(
            Class::ContextSpecific,
            component_index,
            TagKind::Implicit,
            TagSource::TagImplied,
        ))
    } else {
        tagged_type.tag.clone().or(match &tagged_type.ty {
            UntaggedType::BuiltinType(builtin) => builtin.tag_type().map(Tag::universal),
            UntaggedType::Reference(_) | UntaggedType::ObjectClassField(_) => None,
        })
    };
    if let Some(tag) = &mut tag {
        match &tagged_type.ty {
            UntaggedType::ObjectClassField(field_ref) => {
                if field_ref.kind == ObjectClassFieldReferenceKind::TypeLike {
                    tag.kind = TagKind::Explicit(None);
                }
            }
            _ => {
                let resolved_type = tagged_type.resolve(context)?;
                if matches!(&resolved_type.ty, BuiltinType::Choice(_)) {
                    tag.kind = TagKind::Explicit(None);
                }
            }
        }
    }
    tagged_type.tag = tag;

    Ok(())
}
