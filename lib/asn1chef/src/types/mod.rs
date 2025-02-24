use std::fmt::Display;

use int_enum::IntEnum;

mod simple;
pub use simple::*;

mod structured;
pub use structured::*;

mod character_strings;
use character_strings::*;

mod constraints;
pub use constraints::*;

use crate::{
    compiler::{
        context, encode,
        parser::{AstElement, Error, ErrorKind, Result},
    },
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
}

pub struct TagContext<'a> {
    // True if the tag is the outer of an EXPLICIT definition.
    pub is_outer_explicit: bool,
    // If the tag is of a field in a SEQUENCE, this is the index (from 0) of that tag.
    pub structure_component_index: Option<u8>,
    // The type that is tagged by this tag.
    pub ty: &'a Type,
}

impl Tag {
    pub const MAX_TAG: u16 = 16383;

    pub fn new(class: Class, tag: u16) -> Tag {
        if tag > Self::MAX_TAG {
            panic!("{} > MAX_TAG", tag);
        }
        Tag {
            class,
            num: Some(tag),
        }
    }

    pub fn ber_encode(&self, buf: &mut Vec<u8>, ctx: TagContext<'_>) {
        let (class, tag) = match (self.class, self.num, ctx.structure_component_index) {
            // use the structure field index when available, overriding the tag
            // TODO: determine whether tag was explicitly defined on field, or inherited from defined type
            (_, _, Some(tag)) => (Class::ContextSpecific, tag as u16),
            // explicit class and tag
            (class, Some(tag), _) => (class, tag),
            // when an unspecific tag and not a structure field, default to the UNIVERSAL tag
            (Class::ContextSpecific, None, None) => {
                (Class::Universal, ctx.ty.tag_type().unwrap() as u16)
            }
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
            encode::write_vlq(tag as u64, buf);
        }
        let msb_tag = if tag <= 30 { tag as u8 } else { 0b11111 };
        buf.push(class << 6 | form << 5 | msb_tag);
    }
}

impl Default for Tag {
    fn default() -> Self {
        Tag {
            class: Class::ContextSpecific,
            num: None,
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
pub struct TaggedType {
    pub tag: Tag,
    pub kind: TagKind,
    pub ty: Type,
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

#[repr(u8)]
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
pub enum Type {
    Boolean,
    Integer(IntegerType),
    BitString(BitStringType),
    OctetString(OctetStringType),
    Null,
    ObjectIdentifier,
    Real,
    Enumerated(EnumeratedType),
    Sequence(Structure),
    SequenceOf(Structure),
    Set(Structure),
    SetOf(Structure),
    Choice(Choice),
    NumericString(CharacterString<NumericStringCharset>),
    PrintableString(CharacterString<PrintableStringCharset>),
}

impl Type {
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
        if self.tag_type().unwrap() != value.tag_type() {
            return Err(Error {
                kind: ErrorKind::Ast(format!(
                    "expecting {} but found {}",
                    self.tag_type().unwrap(),
                    value.tag_type()
                )),
                loc: valref.loc,
            });
        }

        let (constraints, item) = match (self, value) {
            (Self::BitString(bit_string), Value::BitString(value)) => {
                (bit_string.size_constraints.as_ref(), value.bits() as i64)
            }
            (Self::OctetString(octet_string), Value::OctetString(value)) => {
                (octet_string.size_constraints.as_ref(), value.len() as i64)
            }
            (Self::Integer(integer), Value::Integer(value)) => {
                (integer.value_constraints.as_ref(), *value)
            }
            _ => (None, 0),
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Type::Boolean => "BOOLEAN",
            Type::Integer(_) => "INTEGER",
            Type::BitString(_) => "BIT STRING",
            Type::OctetString(_) => "OCTET STRING",
            Type::Null => "NULL",
            Type::ObjectIdentifier => "OBJECT IDENTIFIER",
            Type::Real => "REAL",
            Type::Enumerated(_) => "ENUMERATED",
            Type::Sequence(_) => "SEQUENCE",
            Type::SequenceOf(_) => "SEQUENCE OF",
            Type::Set(_) => "SET",
            Type::SetOf(_) => "SET OF",
            Type::Choice(_) => "CHOICE",
            Type::NumericString(_) => "NUMERIC STRING",
            Type::PrintableString(_) => "PRINTABLE STRING",
        })
    }
}

macro_rules! typeref {
    () => {
        crate::types::TypeReference<{crate::types::TagType::Any as u8}>
    };
    ($tag:ident) => {
        crate::types::TypeReference<{crate::types::TagType::$tag as u8}>
    };
}
pub(crate) use typeref;

#[derive(Debug, Clone)]
pub enum TypeReference<const TYPE_TAG: u8> {
    Type(TaggedType),
    Reference(QualifiedIdentifier),
}

impl<const TYPE_TAG: u8> TypeReference<TYPE_TAG> {
    /// Casts `&'a self` to `&'a TypeReference<{ TagType::Any as u8 }>`.
    ///
    /// The returned reference is identical to `&self` (including pointing to the same memory),
    /// except for the modified compile-time `<const TAG_TYPE: u8>` generic parameter.
    pub fn as_any(&self) -> &TypeReference<{ TagType::Any as u8 }> {
        // since the `<TYPE_TAG>` generic parameter type is only present at compile-time,
        // we can safely transmute a &TypeReference<X> -> &TypeReference<Y>,
        // because the runtime memory representations of both these types are identical
        unsafe { std::mem::transmute(self) }
    }
}

impl<const TYPE_TAG: u8> ToString for TypeReference<TYPE_TAG> {
    fn to_string(&self) -> String {
        match self {
            Self::Type(tagged_type) => tagged_type.to_string(),
            Self::Reference(typeref) => typeref.name.clone(),
        }
    }
}

pub trait TypeResolve {
    fn resolve(&self) -> Result<&TaggedType>;
}

impl<const TYPE_TAG: u8> TypeResolve for AstElement<TypeReference<TYPE_TAG>> {
    fn resolve(&self) -> Result<&TaggedType> {
        let mut typeref = self.as_ref().map(|typeref| typeref.as_any());
        let tagged_type = loop {
            match &typeref.element {
                TypeReference::<{ TagType::Any as u8 }>::Type(ty) => break ty,
                TypeReference::<{ TagType::Any as u8 }>::Reference(ident) => {
                    typeref = context().lookup_type(ident).ok_or_else(|| Error {
                        kind: ErrorKind::Ast(format!("undefined reference to type '{ident}")),
                        loc: typeref.loc,
                    })?
                }
            }
        };
        if let Some(tag_type) = tagged_type.ty.tag_type() {
            let ref_type = TagType::try_from(TYPE_TAG).unwrap();
            if !TagType::compare(tag_type, ref_type) {
                return Err(Error {
                    kind: ErrorKind::Ast(format!("expecting {ref_type}, got {tag_type}")),
                    loc: self.loc,
                });
            }
            Ok(tagged_type)
        } else {
            Err(Error {
                kind: ErrorKind::Ast(format!("{tagged_type:?} does not have a defined type tag")),
                loc: self.loc,
            })
        }
    }
}
