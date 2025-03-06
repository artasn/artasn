use std::fmt::Display;

use int_enum::IntEnum;

mod simple;
use num::BigInt;
pub use simple::*;

mod structured;
pub use structured::*;

mod constraints;
pub use constraints::*;

use crate::{
    compiler::{
        context,
        parser::{AstElement, Error, ErrorKind, Result},
    },
    encoding,
    module::QualifiedIdentifier,
    values::{BuiltinValue, Value, ValueResolve},
};

#[derive(Debug, Clone, Copy, PartialEq)]
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

    pub fn der_encode(&self, buf: &mut Vec<u8>, ctx: TagContext<'_>) {
        let class = match self.class {
            Class::Universal => 0b00,
            Class::Application => 0b01,
            Class::ContextSpecific => 0b10,
            Class::Private => 0b11,
        };
        let form = match (ctx.is_outer_explicit, ctx.ty.form()) {
            // if not the outer tag of an EXPLICIT definition and is primitive, form = 0
            (false, TypeForm::Primitive) => 0b0,
            // if either the outer tag of an EXPLICIT definition or is constructed, form = 1
            _ => 0b1,
        };
        if self.num >= 31 {
            encoding::write_vlq(self.num as u64, buf);
        }
        let msb_tag = if self.num <= 30 {
            self.num as u8
        } else {
            0b11111
        };
        buf.push(class << 6 | form << 5 | msb_tag);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TagKind {
    Explicit,
    Implicit,
}

impl Display for TagKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Explicit => f.write_str("EXPLICIT"),
            Self::Implicit => f.write_str("IMPLICIT"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UntaggedType {
    BuiltinType(BuiltinType),
    Reference(AstElement<QualifiedIdentifier>),
}

impl Display for UntaggedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BuiltinType(builtin) => builtin.fmt(f),
            Self::Reference(ident) => ident.element.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedType {
    pub tag: Tag,
    pub ty: BuiltinType,
}

#[derive(Debug, Clone)]
pub struct TaggedType {
    pub tag: Option<Tag>,
    pub ty: UntaggedType,
}

impl TaggedType {
    pub fn resolve(&self) -> Result<ResolvedType> {
        let mut tagged_ty = self;
        let mut tag = self.tag.as_ref();
        loop {
            match &tagged_ty.ty {
                UntaggedType::BuiltinType(ty) => {
                    return Ok(ResolvedType {
                        tag: tag.expect("unreachable: no tag").clone(),
                        ty: ty.clone(),
                    });
                }
                UntaggedType::Reference(ident) => {
                    tagged_ty = context().lookup_type(&ident.element).ok_or_else(|| Error {
                        kind: ErrorKind::Ast(format!(
                            "undefined reference to type '{}",
                            ident.element
                        )),
                        loc: ident.loc,
                    })?;
                    tag = tag.or(tagged_ty.tag.as_ref());
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
            Self::Any => "<ANY>",
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
    Boolean,
    Integer(IntegerType),
    BitString(BitStringType),
    OctetString(OctetStringType),
    Null,
    ObjectIdentifier,
    Real,
    Enumerated(EnumeratedType),
    Structure(Structure),
    StructureOf(StructureOf),
    Choice(Choice),
    CharacterString(TagType),
}

impl BuiltinType {
    pub fn tag_type(&self) -> Option<TagType> {
        Some(match self {
            Self::Boolean => TagType::Boolean,
            Self::Integer(_) => TagType::Integer,
            Self::BitString(_) => TagType::BitString,
            Self::OctetString(_) => TagType::OctetString,
            Self::Null => TagType::Null,
            Self::ObjectIdentifier => TagType::ObjectIdentifier,
            Self::Real => TagType::Real,
            Self::Enumerated(_) => TagType::Enumerated,
            Self::Structure(structure) => structure.ty,
            Self::StructureOf(of) => of.ty,
            Self::Choice(_) => return None,
            Self::CharacterString(tag_type) => *tag_type,
        })
    }

    /// TODO: This needs a lot more checked. See https://stackoverflow.com/a/70213161.
    pub fn form(&self) -> TypeForm {
        match self {
            Self::Structure(_) | Self::StructureOf(_) | Self::Choice(_) => TypeForm::Constructed,
            _ => TypeForm::Primitive,
        }
    }

    pub fn ensure_satisfied_by_value(&self, valref: &AstElement<Value>) -> Result<()> {
        let value = valref.resolve()?;
        let tag_type = self
            .tag_type()
            .expect("ensure_satisfied_by_value: no tag type");
        if tag_type != value.tag_type() {
            return Err(Error {
                kind: ErrorKind::Ast(format!(
                    "expecting {} but found {}",
                    tag_type,
                    value.tag_type()
                )),
                loc: valref.loc,
            });
        }

        let (constraints, item) = match (self, value) {
            (Self::BitString(bit_string), BuiltinValue::BitString(value)) => (
                bit_string.size_constraints.as_ref(),
                &BigInt::from(value.bits() as i64),
            ),
            (Self::OctetString(octet_string), BuiltinValue::OctetString(value)) => (
                octet_string.size_constraints.as_ref(),
                &BigInt::from(value.len() as i64),
            ),
            (Self::Integer(integer), BuiltinValue::Integer(value)) => {
                (integer.value_constraints.as_ref(), value)
            }
            (Self::StructureOf(seq_of), BuiltinValue::SequenceOf(value)) => (
                seq_of.size_constraints.as_ref(),
                &BigInt::from(value.len() as i64),
            ),
            _ => (None, &BigInt::ZERO),
        };
        if let Some(constraints) = constraints {
            if !constraints.includes_value(item)? {
                return Err(Error {
                    kind: ErrorKind::Ast(format!(
                        "value does not satisfy {} constraints of type",
                        constraints.kind,
                    )),
                    loc: valref.loc,
                });
            }
        }

        if let (Self::Structure(seq), BuiltinValue::Sequence(value)) = (self, value) {
            for seq_component in &seq.components {
                let val_component = value
                    .components
                    .iter()
                    .find(|val_component| val_component.name.element == seq_component.name.element);
                if let Some(val_component) = val_component {
                    let seq_ty = seq_component.component_type.resolve()?;
                    seq_ty.ty.ensure_satisfied_by_value(&val_component.value)?;
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
}

impl Display for BuiltinType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Choice(_) => f.write_str("CHOICE"),
            other => other.tag_type().unwrap().fmt(f),
        }
    }
}
