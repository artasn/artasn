use int_enum::IntEnum;
use num::BigInt;
use std::fmt::Display;

mod simple;
pub use simple::*;

mod structured;
pub use structured::*;

mod constraints;
pub use constraints::*;

mod class;
pub use class::*;

use crate::{
    compiler::{
        parser::{AstElement, Error, ErrorKind, Result},
        Context,
    },
    module::QualifiedIdentifier,
    values::{BuiltinValue, TypedValue, ValueResolve},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Class {
    Universal,
    Application,
    ContextSpecific,
    Private,
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Universal => "UNIVERSAL",
            Self::Application => "APPLICATION",
            Self::ContextSpecific => "CONTEXT-SPECIFIC",
            Self::Private => "PRIVATE",
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TagKind {
    Explicit(Option<(Class, u16)>),
    Implicit,
}

impl Display for TagKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Explicit(_) => f.write_str("EXPLICIT"),
            Self::Implicit => f.write_str("IMPLICIT"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TagSource {
    // If the source type has no tag, and thus the entire tag was implied.
    // This happens in two cases:
    //   1. The UNIVERSAL tag was implied for a type.
    //   2. The CONTEXT-SPECIFIC tag was implied for a structure component type in a module with the AUTOMATIC TagDefault.
    TagImplied,
    // If the tag did not have IMPLICIT or EXPLICIT specified, and the kind was determined via the module's TagDefault.
    KindImplied,
    // If the tag had either IMPLICIT or EXPLICIT specified.
    KindSpecified,
}

impl Display for TagSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::TagImplied => "TagImplied",
            Self::KindImplied => "KindImplied",
            Self::KindSpecified => "KindSpecified",
        })
    }
}

#[derive(Debug, Clone)]
pub struct Tag {
    pub class: Class,
    pub num: u16,
    pub kind: TagKind,
    pub source: TagSource,
}

impl Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.class == Class::ContextSpecific {
            f.write_fmt(format_args!("[{}]", self.num))?;
        } else {
            f.write_fmt(format_args!("[{} {}]", self.class, self.num))?;
        }
        if self.source == TagSource::KindSpecified {
            f.write_fmt(format_args!(" {}", self.kind))?;
        }

        Ok(())
    }
}

pub struct TagContext<'a> {
    // True if the tag is the outer of an EXPLICIT definition.
    pub is_outer_explicit: bool,
    // The type that is tagged by this tag.
    pub ty: &'a BuiltinType,
}

impl Tag {
    pub const MAX_TAG: u16 = 16383;

    pub fn new(class: Class, num: u16, kind: TagKind, source: TagSource) -> Tag {
        if num > Self::MAX_TAG {
            panic!("{} > MAX_TAG", num);
        }
        Tag {
            class,
            num,
            kind,
            source,
        }
    }

    pub fn universal(tag_type: TagType) -> Tag {
        Tag::new(
            Class::Universal,
            tag_type as u16,
            TagKind::Implicit,
            TagSource::TagImplied,
        )
    }
}

#[derive(Debug, Clone)]
pub enum UntaggedType {
    BuiltinType(BuiltinType),
    Reference(AstElement<QualifiedIdentifier>),
    ObjectClassField(ObjectClassFieldReference),
}

impl Display for UntaggedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BuiltinType(builtin) => builtin.fmt(f),
            Self::Reference(typeref) => typeref.element.fmt(f),
            Self::ObjectClassField(ocf) => ocf.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedType {
    pub tag: Option<Tag>,
    pub ty: BuiltinType,
    pub constraints: Option<Vec<Constraint>>,
}

impl ResolvedType {
    pub fn universal(tag_type: TagType) -> ResolvedType {
        ResolvedType {
            tag: Some(Tag::universal(tag_type)),
            ty: BuiltinType::universal(tag_type),
            constraints: None,
        }
    }

    pub fn get_possible_tags(&self, context: &Context) -> Result<Vec<(Tag, BuiltinType)>> {
        let mut tags = Vec::with_capacity(1);
        self.extend_possible_tags(context, &mut tags)?;
        Ok(tags)
    }

    fn extend_possible_tags(
        &self,
        context: &Context,
        tags: &mut Vec<(Tag, BuiltinType)>,
    ) -> Result<()> {
        match &self.tag {
            Some(tag) => tags.push((tag.clone(), self.ty.clone())),
            None => match &self.ty {
                BuiltinType::Choice(choice) => {
                    for alternative in choice.resolve_alternatives(context)? {
                        let alternative_ty = alternative.alternative_type.resolve(context)?;
                        alternative_ty.extend_possible_tags(context, tags)?;
                    }
                }
                other => unreachable!("extend_possible_tags: tag is None but type is {}", other),
            },
        }

        Ok(())
    }
}

impl From<ResolvedType> for TaggedType {
    fn from(resolved: ResolvedType) -> Self {
        Self {
            tag: resolved.tag,
            ty: UntaggedType::BuiltinType(resolved.ty),
            constraints: resolved.constraints,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TaggedType {
    pub tag: Option<Tag>,
    pub ty: UntaggedType,
    pub constraints: Option<Vec<Constraint>>,
}

impl TaggedType {
    pub fn universal(tag_type: TagType) -> TaggedType {
        TaggedType {
            tag: Some(Tag::universal(tag_type)),
            ty: UntaggedType::BuiltinType(BuiltinType::universal(tag_type)),
            constraints: None,
        }
    }

    pub fn resolve(&self, context: &Context) -> Result<ResolvedType> {
        let mut tagged_ty = self;
        let mut tag = self.tag.as_ref();
        let mut constraint_subsets = Vec::new();
        if let Some(constraints) = &self.constraints {
            constraint_subsets.extend(constraints.iter().rev());
        }
        loop {
            match &tagged_ty.ty {
                UntaggedType::BuiltinType(ty) => {
                    match tag {
                        Some(_) => (),
                        None => match ty {
                            BuiltinType::Choice(_) => (),
                            _ => unreachable!("no tag for non-CHOICE type"),
                        },
                    }
                    return Ok(ResolvedType {
                        tag: tag.cloned(),
                        ty: ty.clone(),
                        constraints: {
                            let constraints = constraint_subsets
                                .into_iter()
                                .rev()
                                .cloned()
                                .collect::<Vec<_>>();
                            if constraints.is_empty() {
                                None
                            } else {
                                Some(constraints)
                            }
                        },
                    });
                }
                UntaggedType::Reference(name) => {
                    let decl = &context.lookup_type(&name.element).ok_or_else(|| Error {
                        kind: ErrorKind::Ast(format!(
                            "undefined reference to type '{}'",
                            name.element
                        )),
                        loc: name.loc,
                    })?;

                    tagged_ty = &decl.ty;
                    tag = tag.or(tagged_ty.tag.as_ref());
                    if let Some(constraints) = &tagged_ty.constraints {
                        constraint_subsets.extend(constraints.iter().rev());
                    }
                }
                UntaggedType::ObjectClassField(ocf) => {
                    let class_ref =
                        InformationObjectClassReference::Reference(ocf.class_type.clone());
                    let class = class_ref.resolve_reference(context)?;
                    let field = class.find_field(&ocf.field)?;
                    match field {
                        ObjectClassField::Value(value) => {
                            match &value.field_type {
                                ObjectClassFieldValueType::TaggedType(tagged_type) => {
                                    tagged_ty = tagged_type;
                                    tag = tag.or(tagged_ty.tag.as_ref());
                                    if let Some(constraints) = &tagged_ty.constraints {
                                        constraint_subsets.extend(constraints.iter().rev());
                                    }
                                }
                                _ => return Err(Error {
                                    kind: ErrorKind::Ast(format!(
                                        "expecting type, but found information object class field reference '{}'",
                                        ocf
                                    )),
                                    loc: ocf.class_type.loc,
                                })
                            }
                        }
                        _ => return Err(Error {
                            kind: ErrorKind::Ast(format!(
                                "expecting type, but found information object class field reference '{}'",
                                ocf
                            )),
                            loc: ocf.class_type.loc,
                        })
                    }
                }
            }
        }
    }
}

impl Display for TaggedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(tag) = self.tag.as_ref() {
            if tag.source != TagSource::TagImplied {
                f.write_fmt(format_args!("{} ", tag))?;
            }
        }

        self.ty.fmt(f)?;

        Ok(())
    }
}

#[repr(u16)]
#[derive(Debug, PartialEq, IntEnum, Clone, Copy)]
pub enum TagType {
    Any = 0,
    Boolean = 1,
    Integer = 2,
    BitString = 3,
    OctetString = 4,
    Null = 5,
    ObjectIdentifier = 6,
    ObjectDescriptor = 7,
    External = 8,
    Real = 9,
    Enumerated = 10,
    EmbeddedPDV = 11,
    UTF8String = 12,
    RelativeOid = 13,
    Time = 14,
    Sequence = 16,
    Set = 17,
    NumericString = 18,
    PrintableString = 19,
    TeletexString = 20,
    VideotexString = 21,
    IA5String = 22,
    UTCTime = 23,
    GeneralizedTime = 24,
    GraphicString = 25,
    VisibleString = 26,
    GeneralString = 27,
    UniversalString = 28,
    CharacterString = 29,
    BMPString = 30,
    Date = 31,
    TimeOfDay = 32,
    DateTime = 33,
    Duration = 34,
}

impl TagType {
    pub fn compare(t1: TagType, t2: TagType) -> bool {
        if t1 == Self::Any || t2 == Self::Any {
            true
        } else {
            t1 == t2
        }
    }
}

impl Display for TagType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Any => "ANY",
            Self::Boolean => "BOOLEAN",
            Self::Integer => "INTEGER",
            Self::BitString => "BIT STRING",
            Self::OctetString => "OCTET STRING",
            Self::Null => "NULL",
            Self::ObjectIdentifier => "OBJECT IDENTIFIER",
            Self::ObjectDescriptor => "ObjectDescriptor",
            Self::External => "EXTERNAL",
            Self::Real => "REAL",
            Self::Enumerated => "ENUMERATED",
            Self::EmbeddedPDV => "EMBEDDED PDV",
            Self::UTF8String => "UTF8String",
            Self::RelativeOid => "RELATIVE-OID",
            Self::Time => "TIME",
            Self::Sequence => "SEQUENCE",
            Self::Set => "SET",
            Self::NumericString => "NumericString",
            Self::PrintableString => "PrintableString",
            Self::TeletexString => "TeletexString",
            Self::VideotexString => "VideotexString",
            Self::IA5String => "IA5String",
            Self::UTCTime => "UTCTime",
            Self::GeneralizedTime => "GeneralizedTime",
            Self::GraphicString => "GraphicString",
            Self::VisibleString => "VisibleString",
            Self::GeneralString => "GeneralString",
            Self::UniversalString => "UniversalString",
            Self::CharacterString => "CHARACTER STRING",
            Self::BMPString => "BMPString",
            Self::Date => "DATE",
            Self::TimeOfDay => "TIME-OF-DAY",
            Self::DateTime => "DATE-TIME",
            Self::Duration => "DURATION",
        })
    }
}

#[derive(Debug, Clone)]
pub enum TypeForm {
    Primitive,
    Constructed,
}

#[derive(Debug, Clone)]
pub enum BuiltinType {
    Any,
    Boolean,
    Integer(IntegerType),
    BitString(BitStringType),
    OctetString,
    Null,
    ObjectIdentifier,
    Real,
    Enumerated(EnumeratedType),
    RelativeOid,
    Time,
    Structure(Structure),
    StructureOf(StructureOf),
    Choice(Choice),
    CharacterString(TagType),
    UTCTime,
    GeneralizedTime,
    Date,
    TimeOfDay,
    DateTime,
    Duration,
}

impl BuiltinType {
    pub fn universal(tag_type: TagType) -> BuiltinType {
        match tag_type {
            TagType::Any => Self::Any,
            TagType::Boolean => Self::Boolean,
            TagType::Integer => Self::Integer(IntegerType { named_values: None }),
            TagType::BitString => Self::BitString(BitStringType { named_bits: None }),
            TagType::OctetString => Self::OctetString,
            TagType::Null => Self::Null,
            TagType::ObjectIdentifier => Self::ObjectIdentifier,
            TagType::ObjectDescriptor => Self::CharacterString(TagType::ObjectDescriptor),
            TagType::Real => Self::Real,
            TagType::RelativeOid => Self::RelativeOid,
            TagType::Time => Self::Time,
            TagType::UTCTime => Self::UTCTime,
            TagType::GeneralizedTime => Self::GeneralizedTime,
            TagType::Date => Self::Date,
            TagType::TimeOfDay => Self::TimeOfDay,
            TagType::DateTime => Self::DateTime,
            TagType::Duration => Self::Duration,
            tag_type => unimplemented!("default BuiltinType for {}", tag_type),
        }
    }

    pub fn tag_type(&self) -> Option<TagType> {
        Some(match self {
            Self::Any => TagType::Any,
            Self::Boolean => TagType::Boolean,
            Self::Integer(_) => TagType::Integer,
            Self::BitString(_) => TagType::BitString,
            Self::OctetString => TagType::OctetString,
            Self::Null => TagType::Null,
            Self::ObjectIdentifier => TagType::ObjectIdentifier,
            Self::Real => TagType::Real,
            Self::Enumerated(_) => TagType::Enumerated,
            Self::RelativeOid => TagType::RelativeOid,
            Self::Time => TagType::Time,
            Self::Structure(structure) => structure.ty,
            Self::StructureOf(of) => of.ty,
            Self::Choice(_) => return None,
            Self::CharacterString(tag_type) => *tag_type,
            Self::UTCTime => TagType::UTCTime,
            Self::GeneralizedTime => TagType::GeneralizedTime,
            Self::Date => TagType::Date,
            Self::TimeOfDay => TagType::TimeOfDay,
            Self::DateTime => TagType::DateTime,
            Self::Duration => TagType::Duration,
        })
    }

    pub fn form(&self) -> TypeForm {
        match self {
            Self::Structure(_) | Self::StructureOf(_) => TypeForm::Constructed,
            _ => TypeForm::Primitive,
        }
    }

    pub fn ensure_satisfied_by_value(
        &self,
        context: &Context,
        valref: &AstElement<TypedValue>,
        constraint: Option<&ResolvedConstraint>,
    ) -> Result<()> {
        let typed_value = valref.resolve(context)?;

        let size_ok = match &constraint {
            Some(constraint) => {
                match &typed_value.value {
                    value @ BuiltinValue::BitString(_) => 'block: {
                        let total_bits = BigInt::from(value.size().expect("size() of BitString"));

                        match self {
                            Self::BitString(bs) => {
                                if bs.named_bits.is_some() {
                                    if let Some(bounds) = constraint.size_bounds()? {
                                        match bounds.lower_bound {
                                            Bound::Integer(lower_bound) => {
                                                if total_bits < lower_bound {
                                                    // BIT STRING with named bits are permitted to have values whose size are less than the lower bound
                                                    // this is because in PER, the lower bound just indicates how much padding to add to the value
                                                    break 'block Some(
                                                        IntegerInclusion::Included {
                                                            is_extension: false,
                                                        },
                                                    );
                                                }
                                            }
                                            Bound::Unbounded => (),
                                        }
                                    }
                                }
                            }
                            _ => unreachable!(),
                        };

                        constraint.includes_integer(ConstraintCheckMode::Size, &total_bits)?
                    }
                    other => match other.size() {
                        Some(size) => constraint
                            .includes_integer(ConstraintCheckMode::Size, &BigInt::from(size))?,
                        None => None,
                    },
                }
            }
            None => None,
        };
        let size_ok = size_ok.map(|inclusion| match inclusion {
            IntegerInclusion::Included { .. } => true,
            IntegerInclusion::NotIncluded => false,
        });
        let value_ok = match &constraint {
            Some(constraint) => match &typed_value.value {
                BuiltinValue::Integer(value) => constraint
                    .includes_integer(ConstraintCheckMode::Value, value)?
                    .map(|inclusion| match inclusion {
                        IntegerInclusion::Included { .. } => true,
                        IntegerInclusion::NotIncluded => false,
                    }),
                _ => constraint.includes_value(context, valref)?,
            },
            None => None,
        };

        match (size_ok, value_ok) {
            (Some(false), Some(false)) | (Some(false), None) | (None, Some(false)) => {
                return Err(Error {
                    kind: ErrorKind::Ast("value violates constraints of type".to_string()),
                    loc: valref.loc,
                });
            }
            _ => (),
        }

        if let builtin_value @ (BuiltinValue::Structure(_, _)
        | BuiltinValue::StructureOf(_, _)
        | BuiltinValue::Choice(_)) = &typed_value.value
        {
            let inner_type_constraints: Option<Vec<&InnerTypeConstraints>> =
                constraint.as_ref().map(|constraint| {
                    constraint
                        .specs
                        .iter()
                        .flat_map(|spec| {
                            spec.items.iter().filter_map(|item| match item {
                                ConstraintSpecItem::InnerType(inner_type) => Some(inner_type),
                                _ => None,
                            })
                        })
                        .collect()
                });
            if let Some(inner_type_constraints) = inner_type_constraints {
                let satisfied = 'block: {
                    for inner_type_constraint in inner_type_constraints {
                        if inner_type_constraint.is_satisfied_by_value(
                            context,
                            self,
                            builtin_value,
                        )? {
                            break 'block true;
                        }
                    }

                    false
                };
                if !satisfied {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!("{} value does not satisfy any of the inner type constraints in the governing type", typed_value.ty.ty)),
                        loc: valref.loc,
                    });
                }
            }
        }

        if let (Self::Structure(seq), BuiltinValue::Structure(_, value)) =
            (self, &typed_value.value)
        {
            for seq_component in seq.resolve_components(context)? {
                let val_component = value
                    .components
                    .iter()
                    .find(|val_component| val_component.name.element == seq_component.name.element);
                if let Some(val_component) = val_component {
                    match &seq_component.component_type.ty {
                        UntaggedType::ObjectClassField(_ocf) => {
                            // println!("need to verify OCF: {ocf:#?}");
                        }
                        _ => {
                            let seq_ty = seq_component.component_type.resolve(context)?;
                            let constraint = match &seq_component.component_type.constraints {
                                Some(constraints) => Some(Constraint::resolve_subsets(
                                    context,
                                    constraints,
                                    &seq_ty.ty,
                                )?),
                                None => None,
                            };
                            seq_ty.ty.ensure_satisfied_by_value(
                                context,
                                &val_component.value,
                                constraint.as_ref(),
                            )?;
                        }
                    }
                } else if seq_component.default_value.is_none() && !seq_component.optional {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "SEQUENCE missing required component '{}'",
                            seq_component.name.element
                        )),
                        loc: valref.loc,
                    });
                }
            }
        }

        Ok(())
    }

    pub fn is_assignable_to(&self, to: &BuiltinType) -> bool {
        match (self, to) {
            (_, BuiltinType::Any) => true,
            _ => self.tag_type() == to.tag_type(),
        }
    }
}

impl Display for BuiltinType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Choice(_) => f.write_str("CHOICE"),
            Self::StructureOf(of) => match of.ty {
                TagType::Sequence => f.write_str("SEQUENCE OF"),
                TagType::Set => f.write_str("SET OF"),
                _ => unreachable!(),
            },
            other => other.tag_type().unwrap().fmt(f),
        }
    }
}
