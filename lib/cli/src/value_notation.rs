use artasn::{
    compiler::{
        parser::{Error, ErrorKind, Result},
        Context,
    },
    encoding::{DecodedValue, DecodedValueForm, DecodedValueKind},
    types::{BuiltinType, UntaggedType},
};
use std::fmt::Write;

struct CodeBuilder {
    buf: String,
    /// The identation prefix (in spaces) before each line is written.
    indent: String,
    indent_unit: String,
}

impl CodeBuilder {
    pub fn new(indent_size: usize, capacity: usize) -> CodeBuilder {
        CodeBuilder {
            buf: String::with_capacity(capacity),
            indent: String::with_capacity(80),
            indent_unit: " ".repeat(indent_size),
        }
    }

    pub fn into_string(self) -> String {
        self.buf
    }

    pub fn write_indented(&mut self, str: &str) {
        self.buf.write_str(&self.indent).unwrap();
        self.buf.write_str(str).unwrap();
    }

    pub fn write(&mut self, str: &str) {
        self.buf.write_str(str).unwrap();
    }

    pub fn indent(&mut self) {
        self.indent.write_str(&self.indent_unit).unwrap();
    }

    pub fn outdent(&mut self) {
        self.indent.replace_range(..self.indent_unit.len(), "");
    }
}

pub fn get_asn1_value_str(context: &Context, value: &DecodedValue) -> String {
    let mut cb = CodeBuilder::new(2, 64 * 1024);

    let metadata = value.metadata.as_ref().expect("value.metadata");
    cb.write("decoded-value ");
    cb.write(&match &metadata.type_ident {
        UntaggedType::BuiltinType(_) => panic!("expecting reference, but found builtin type"),
        UntaggedType::Reference(typeref) => typeref.element.to_string(),
        UntaggedType::ObjectClassField(_) => {
            panic!("expecting reference, but found object class field reference")
        }
    });
    cb.write(" ::= ");

    write_asn1_value_str(context, value, &mut cb);

    cb.into_string()
}

fn write_asn1_value_str(context: &Context, value: &DecodedValue, cb: &mut CodeBuilder) {
    let metadata = value.metadata.as_ref().expect("metadata");
    let value_type = resolve_untagged(context, &metadata.type_ident).expect("resolve_untagged");

    // if a CHOICE value is written with IMPLICIT tagging (primitive),
    // we need to handle that case here, instead of the AUTOMATIC and EXPLICIT (constructed)
    // tagging, which is handled in the Constructed match branch below
    if let BuiltinType::Choice(choice) = value_type {
        let alternative_name = choice
            .alternatives
            .iter()
            .find_map(|alternative| {
                let alternative_type = alternative
                    .alternative_type
                    .resolve(context)
                    .expect("alternative_type.resolve");
                let tag = alternative_type.tag.as_ref().expect("alternative_type.tag");
                let tlv_tag = &value.tag.element;
                if tlv_tag.class == tag.class && tlv_tag.num == tag.num {
                    Some(&alternative.name.element)
                } else {
                    None
                }
            })
            .expect("no matching alternative");

        cb.write(alternative_name);
        cb.write(" : ");
    }

    match &value.form {
        DecodedValueForm::Primitive(kind) => {
            write_asn1_value_str_primitive(kind, cb);
        }
        DecodedValueForm::Constructed(constructed) => match value_type {
            BuiltinType::Structure(_) => {
                cb.write("{\n");
                cb.indent();
                let mut constructed = constructed.iter().peekable();
                while let Some(element) = constructed.next() {
                    let component_name = element
                        .metadata
                        .as_ref()
                        .and_then(|metadata| metadata.component_name.as_ref());
                    match component_name {
                        Some(component_name) => {
                            cb.write_indented(component_name);
                            cb.write(" ");
                        }
                        None => cb.write_indented(""),
                    }
                    write_asn1_value_str(context, element, cb);
                    if constructed.peek().is_some() {
                        cb.write(",");
                    }
                    cb.write("\n");
                }
                cb.outdent();
                cb.write_indented("}");
            }
            BuiltinType::Choice(_) => {
                assert_eq!(constructed.len(), 1);

                let alternative = &constructed[0];
                let alternative_metadata =
                    alternative.metadata.as_ref().expect("alternative.metadata");

                cb.write(
                    alternative_metadata
                        .component_name
                        .as_ref()
                        .expect("component_name of CHOICE value"),
                );
                cb.write(" : ");
                write_asn1_value_str(context, alternative, cb);
            }
            other => panic!("{other:#?}"),
        },
    }

    // cb.write(" -- ");
    // cb.write(&match &metadata.type_ident {
    //     UntaggedType::BuiltinType(builtin) => builtin.to_string(),
    //     UntaggedType::Reference(typeref) => typeref.element.to_string(),
    //     UntaggedType::ObjectClassField(ocf) => ocf.to_string(),
    // });
    // cb.write(" --");
}

fn write_asn1_value_str_primitive(kind: &DecodedValueKind, cb: &mut CodeBuilder) {
    match kind {
        DecodedValueKind::Raw(_) => panic!("found RAW"),
        DecodedValueKind::Boolean(b) => cb.write(match *b {
            true => "TRUE",
            false => "FALSE",
        }),
        DecodedValueKind::Integer(int) => cb.write(&int.to_string()),
        DecodedValueKind::BitString(bs) => cb.write(&format!("'{}'B", bs)),
        DecodedValueKind::OctetString(os) => cb.write(&format!("'{}'H", hex::encode_upper(os))),
        DecodedValueKind::Null => cb.write("NULL"),
        DecodedValueKind::ObjectIdentifier(oid) => {
            let oid = &oid.0;
            if oid.len() < 2 {
                panic!("oid.len() < 2");
            }

            cb.write("{ ");

            cb.write(match oid[0] {
                0 => "itu-t",
                1 => "iso",
                2 => "joint-iso-itu-t",
                other => panic!("illegal oid root: {}", other),
            });

            for node in oid.iter().skip(1) {
                cb.write(" ");
                cb.write(&node.to_string());
            }

            cb.write(" }");
        }
        DecodedValueKind::Real(real) => cb.write(&real.to_string()),
        DecodedValueKind::Enumerated(enumeration) => {
            cb.write(enumeration.item.as_ref().expect("enumeration.item"))
        }
        DecodedValueKind::Time(time) => cb.write(&format!("\"{}\"", time.source)),
        DecodedValueKind::CharacterString(_, str) => cb.write(&format!("\"{}\"", str)),
        DecodedValueKind::UTCTime(utc) => cb.write(&format!("\"{}\"", utc.to_ber_string())),
        DecodedValueKind::Date(date) => cb.write(&format!(
            "\"{:04}-{:02}-{:02}\"",
            date.year, date.month, date.day
        )),
        DecodedValueKind::TimeOfDay(time_of_day) => cb.write(&format!(
            "\"{:02}:{:02}:{:02}\"",
            time_of_day.hour, time_of_day.minute, time_of_day.second
        )),
        DecodedValueKind::DateTime(date_time) => {
            let date = &date_time.date;
            let time_of_day = &date_time.time_of_day;
            cb.write(&format!(
                "\"{:04}-{:02}-{:02}",
                date.year, date.month, date.day
            ));
            cb.write(&format!(
                "T{:02}:{:02}:{:02}\"",
                time_of_day.hour, time_of_day.minute, time_of_day.second
            ));
        }
        DecodedValueKind::Duration(duration) => cb.write(&format!("\"{}\"", duration.source)),
    }
}

fn resolve_untagged<'a>(
    context: &'a Context,
    mut untagged_ty: &'a UntaggedType,
) -> Result<&'a BuiltinType> {
    loop {
        match &untagged_ty {
            UntaggedType::BuiltinType(ty) => return Ok(ty),
            UntaggedType::Reference(name) => {
                let decl = &context.lookup_type(&name.element).ok_or_else(|| Error {
                    kind: ErrorKind::Ast(format!("undefined reference to type '{}'", name.element)),
                    loc: name.loc,
                })?;

                untagged_ty = &decl.ty.ty;
            }
            UntaggedType::ObjectClassField(_) => todo!(),
        }
    }
}
