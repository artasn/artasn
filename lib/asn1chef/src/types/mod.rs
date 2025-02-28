use std::fmt::Display;

use int_enum::IntEnum;

mod simple;
use num::BigInt;
pub use simple::*;

mod structured;
pub use structured::*;

mod character_strings;
use character_strings::*;

mod constraints;
pub use constraints::*;

use crate::{
    compiler::{
        context,
        parser::{AstElement, Error, ErrorKind, Result},
    },
    encoding,
    module::QualifiedIdentifier,
    values::{valref, Value, ValueResolve},
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

#[derive(Debug, Clone)]
pub struct Tag {
    pub class: Class,
    pub num: Option<u16>,
    pub kind: TagKind,
}

pub struct TagContext<'a> {
    // True if the tag is the outer of an EXPLICIT definition.
    pub is_outer_explicit: bool,
    // The type that is tagged by this tag.
    pub ty: &'a BuiltinType,
}

impl Tag {
    pub const MAX_TAG: u16 = 16383;

    pub fn new(class: Class, tag: u16, kind: TagKind) -> Tag {
        if tag > Self::MAX_TAG {
            panic!("{} > MAX_TAG", tag);
        }
        Tag {
            class,
            num: Some(tag),
            kind,
        }
    }

    pub fn der_encode(&self, buf: &mut Vec<u8>, ctx: TagContext<'_>) {
        let (class, tag) = match (self.class, self.num) {
            // explicit class and tag
            (class, Some(tag)) => (class, tag),
            // when no tag is specified, default to the UNIVERSAL tag
            (Class::ContextSpecific, None) => (
                Class::Universal,
                ctx.ty.tag_type().expect("UNIVERSAL type to have a tag") as u16,
            ),
            _ => unreachable!(),
        };
        let class = match class {
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
        if tag >= 31 {
            encoding::write_vlq(tag as u64, buf);
        }
        let msb_tag = if tag <= 30 { tag as u8 } else { 0b11111 };
        buf.push(class << 6 | form << 5 | msb_tag);
    }

    pub fn is_default(&self) -> bool {
        self.class == Class::ContextSpecific && self.num.is_none() && self.kind == TagKind::Implicit
    }
}

impl Default for Tag {
    fn default() -> Self {
        Tag {
            class: Class::ContextSpecific,
            num: None,
            kind: TagKind::Implicit,
        }
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
    pub tag: Tag,
    pub ty: UntaggedType,
}

impl TaggedType {
    pub fn resolve(&self) -> Result<ResolvedType> {
        let mut tagged_ty = self;
        loop {
            match &tagged_ty.ty {
                UntaggedType::BuiltinType(ty) => {
                    return Ok(ResolvedType {
                        tag: self.tag.clone(),
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
                    })?
                }
            }
        }
    }
}

impl Display for TaggedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(num) = self.tag.num {
            if self.tag.class == Class::ContextSpecific {
                f.write_fmt(format_args!("[{}] ", num))?;
            } else {
                f.write_fmt(format_args!("[{} {}] ", self.tag.class, num))?;
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
    Real = 9,
    Enumerated = 10,
    Sequence = 16,
    Set = 17,
    NumericString = 18,
    PrintableString = 19,
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
            Self::Real => "REAL",
            Self::Enumerated => "ENUMERATED",
            Self::Sequence => "SEQUENCE",
            Self::Set => "SET",
            Self::NumericString => "NUMERIC STRING",
            Self::PrintableString => "PRINTABLE STRING",
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
    Sequence(Structure),
    SequenceOf(StructureOf),
    Set(Structure),
    SetOf(StructureOf),
    Choice(Choice),
    NumericString(CharacterString<NumericStringCharset>),
    PrintableString(CharacterString<PrintableStringCharset>),
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
            Self::Sequence(_) | Self::SequenceOf(_) => TagType::Sequence,
            Self::Set(_) | Self::SetOf(_) => TagType::Set,
            Self::Choice(_) => return None,
            Self::NumericString(_) => TagType::NumericString,
            Self::PrintableString(_) => TagType::PrintableString,
        })
    }

    /// TODO: This needs a lot more checked. See https://stackoverflow.com/a/70213161.
    pub fn form(&self) -> TypeForm {
        match self {
            Self::Enumerated(_)
            | Self::Sequence(_)
            | Self::SequenceOf(_)
            | Self::Set(_)
            | Self::SetOf(_)
            | Self::Choice(_) => TypeForm::Constructed,
            _ => TypeForm::Primitive,
        }
    }

    pub fn ensure_satisfied_by_value(&self, valref: &AstElement<valref!()>) -> Result<()> {
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
            (Self::BitString(bit_string), Value::BitString(value)) => {
                (bit_string.size_constraints.as_ref(), &BigInt::from(value.bits() as i64))
            }
            (Self::OctetString(octet_string), Value::OctetString(value)) => {
                (octet_string.size_constraints.as_ref(), &BigInt::from(value.len() as i64))
            }
            (Self::Integer(integer), Value::Integer(value)) => {
                (integer.value_constraints.as_ref(), value)
            }
            (Self::SequenceOf(seq_of), Value::SequenceOf(value)) => {
                (seq_of.size_constraints.as_ref(), &BigInt::from(value.len() as i64))
            }
            _ => (None, &BigInt::ZERO),
        };
        if let Some(constraints) = constraints {
            if !constraints.includes_value(item)? {
                return Err(Error {
                    kind: ErrorKind::Ast(format!(
                        "value does not satisfy {} constraints of type",
                        constraints.kind.to_string(),
                    )),
                    loc: valref.loc,
                });
            }
        }

        match (self, value) {
            (Self::Sequence(seq), Value::Sequence(value)) => {
                if seq.components.len() != value.components.len() {
                    return Err(Error {
                        kind: ErrorKind::Ast(format!(
                            "SEQUENCE type has {} components, provided value has {}",
                            seq.components.len(),
                            value.components.len()
                        )),
                        loc: valref.loc,
                    });
                }

                for i in 0..seq.components.len() {
                    let seq_component = &seq.components[i];
                    let val_component = &value.components[i];

                    let seq_ty = seq_component.component_type.resolve()?;
                    seq_ty.ty.ensure_satisfied_by_value(&val_component.value)?;
                }
            }
            _ => (),
        }

        Ok(())
    }
}

impl Display for BuiltinType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Boolean => "BOOLEAN",
            Self::Integer(_) => "INTEGER",
            Self::BitString(_) => "BIT STRING",
            Self::OctetString(_) => "OCTET STRING",
            Self::Null => "NULL",
            Self::ObjectIdentifier => "OBJECT IDENTIFIER",
            Self::Real => "REAL",
            Self::Enumerated(_) => "ENUMERATED",
            Self::Sequence(_) => "SEQUENCE",
            Self::SequenceOf(_) => "SEQUENCE OF",
            Self::Set(_) => "SET",
            Self::SetOf(_) => "SET OF",
            Self::Choice(_) => "CHOICE",
            Self::NumericString(_) => "NUMERIC STRING",
            Self::PrintableString(_) => "PRINTABLE STRING",
        })
    }
}
