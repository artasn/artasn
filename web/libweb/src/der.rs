use asn1chef::{encoding::*, values::*};
use js_sys::{Array, BigInt, Object, Reflect};
use wasm_bindgen::{prelude::*, JsValue};

use crate::{LibWeb, QualifiedIdentifier};

fn serialize_tlv_pos(loc: TlvPos) -> JsValue {
    let obj = Object::new();
    Reflect::set(&obj, &"start".into(), &loc.start.into()).unwrap();
    Reflect::set(&obj, &"end".into(), &loc.end.into()).unwrap();
    obj.into()
}

fn serialize_tlv_tag(tag: TlvElement<TlvTag>) -> JsValue {
    let obj = Object::new();
    Reflect::set(&obj, &"pos".into(), &serialize_tlv_pos(tag.pos)).unwrap();
    Reflect::set(&obj, &"class".into(), &tag.element.class.to_string().into()).unwrap();
    Reflect::set(&obj, &"num".into(), &tag.element.num.into()).unwrap();
    obj.into()
}

fn serialize_tlv_len(len: TlvElement<u32>) -> JsValue {
    let obj = Object::new();
    Reflect::set(&obj, &"pos".into(), &serialize_tlv_pos(len.pos)).unwrap();
    Reflect::set(&obj, &"len".into(), &len.element.into()).unwrap();
    obj.into()
}

fn serialize_decoded_value_form(form: DecodedValueForm) -> JsValue {
    let obj = Object::new();
    let (form, field, value) = match form {
        DecodedValueForm::Primitive(kind) => {
            ("primitive", "kind", serialize_decoded_value_kind(kind))
        }
        DecodedValueForm::Constructed(elements) => {
            let arr = Array::new();
            for element in elements {
                arr.push(&serialize_decoded_value(element));
            }
            ("constructed", "elements", arr.into())
        }
    };
    Reflect::set(&obj, &"type".into(), &form.into()).unwrap();
    Reflect::set(&obj, &field.into(), &value).unwrap();
    obj.into()
}

fn serialize_decoded_value_kind(kind: DecodedValueKind) -> JsValue {
    let obj = Object::new();
    let (field, ty, data): (&'static str, JsValue, JsValue) = match kind {
        DecodedValueKind::Raw(data) => ("data", "RAW".into(), hex::encode_upper(data).into()),
        DecodedValueKind::Boolean(data) => ("data", "BOOLEAN".into(), data.into()),
        DecodedValueKind::Integer(data) => {
            let int_str = data.to_string().into();
            let bigint = BigInt::new(&int_str).expect("INTEGER -> bigint");
            ("data", "INTEGER".into(), bigint.into())
        }
        DecodedValueKind::BitString(data) => {
            let bit_str = data.to_string().into();
            let bigint = BigInt::new(&bit_str).expect("BIT STRING -> bigint");
            ("data", "BIT STRING".into(), bigint.into())
        }
        DecodedValueKind::OctetString(data) => (
            "data",
            "OCTET STRING".into(),
            hex::encode_upper(data).into(),
        ),
        DecodedValueKind::Null => ("data", "NULL".into(), JsValue::null()),
        DecodedValueKind::ObjectIdentifier(data) => {
            ("data", "OBJECT IDENTIFIER".into(), data.to_string().into())
        }
        DecodedValueKind::Real(data) => ("data", "REAL".into(), data.to_string().into()),
        DecodedValueKind::Enumerated(variant) => ("data", "ENUMERATED".into(), variant.into()),
        DecodedValueKind::CharacterString(tag_type, str) => {
            ("data", tag_type.to_string().into(), str.into())
        }
        DecodedValueKind::UTCTime(utc) => {
            let obj = Object::new();
            Reflect::set(&obj, &"year".into(), &utc.year.into()).unwrap();
            Reflect::set(&obj, &"month".into(), &utc.month.into()).unwrap();
            Reflect::set(&obj, &"day".into(), &utc.day.into()).unwrap();
            Reflect::set(&obj, &"hour".into(), &utc.hour.into()).unwrap();
            Reflect::set(&obj, &"minute".into(), &utc.minute.into()).unwrap();
            if let Some(second) = utc.second {
                Reflect::set(&obj, &"second".into(), &second.into()).unwrap();
            }
            let tz: JsValue = match utc.tz {
                UTCTimeZone::Z => "Z".into(),
                UTCTimeZone::Offset { sign, hour, minute } => {
                    let obj = Object::new();
                    let sign = match sign {
                        UTCTimeZoneSign::Plus => "+",
                        UTCTimeZoneSign::Minus => "-",
                    };
                    Reflect::set(&obj, &"sign".into(), &sign.into()).unwrap();
                    Reflect::set(&obj, &"hour".into(), &hour.into()).unwrap();
                    Reflect::set(&obj, &"minute".into(), &minute.into()).unwrap();
                    obj.into()
                }
            };
            Reflect::set(&obj, &"tz".into(), &tz).unwrap();
            ("data", "UTCTime".into(), obj.into())
        }
        DecodedValueKind::Date(date) => ("data", "DATE".into(), serialize_date(&date).into()),
        DecodedValueKind::TimeOfDay(time_of_day) => (
            "data",
            "TIME-OF-DAY".into(),
            serialize_time_of_day(&time_of_day).into(),
        ),
        DecodedValueKind::DateTime(date_time) => {
            let obj = Object::new();
            Reflect::set(
                &obj,
                &"date".into(),
                &serialize_date(&date_time.date).into(),
            )
            .unwrap();
            Reflect::set(
                &obj,
                &"timeOfDay".into(),
                &serialize_time_of_day(&date_time.time_of_day).into(),
            )
            .unwrap();
            ("data", "DATE-TIME".into(), obj.into())
        }
    };
    Reflect::set(&obj, &"type".into(), &ty).unwrap();
    Reflect::set(&obj, &field.into(), &data).unwrap();
    obj.into()
}

fn serialize_date(date: &Date) -> JsValue {
    let obj = Object::new();
    Reflect::set(&obj, &"year".into(), &date.year.into()).unwrap();
    Reflect::set(&obj, &"month".into(), &date.month.into()).unwrap();
    Reflect::set(&obj, &"day".into(), &date.day.into()).unwrap();
    obj.into()
}

fn serialize_time_of_day(time_of_day: &TimeOfDay) -> JsValue {
    let obj = Object::new();
    Reflect::set(&obj, &"hour".into(), &time_of_day.hour.into()).unwrap();
    Reflect::set(&obj, &"minute".into(), &time_of_day.minute.into()).unwrap();
    Reflect::set(&obj, &"second".into(), &time_of_day.second.into()).unwrap();
    obj.into()
}

fn serialize_decoded_value_metadata(metadata: &DecodedValueMetadata) -> JsValue {
    let obj = Object::new();
    if let Some(type_ident) = &metadata.type_ident {
        Reflect::set(
            &obj,
            &"typeIdent".into(),
            &serde_wasm_bindgen::to_value(&QualifiedIdentifier::new(&type_ident))
                .expect("serialize typeIdent")
                .into(),
        )
        .unwrap();
    }
    if let Some(component_name) = &metadata.component_name {
        Reflect::set(&obj, &"componentName".into(), &component_name.into()).unwrap();
    }
    obj.into()
}

fn serialize_decoded_value(value: DecodedValue) -> JsValue {
    let obj = Object::new();
    Reflect::set(&obj, &"tag".into(), &serialize_tlv_tag(value.tag)).unwrap();
    Reflect::set(&obj, &"len".into(), &serialize_tlv_len(value.len)).unwrap();
    Reflect::set(
        &obj,
        &"valuePos".into(),
        &serialize_tlv_pos(value.value_pos),
    )
    .unwrap();
    Reflect::set(
        &obj,
        &"form".into(),
        &serialize_decoded_value_form(value.form),
    )
    .unwrap();
    if let Some(metadata) = &value.metadata {
        Reflect::set(
            &obj,
            &"metadata".into(),
            &serialize_decoded_value_metadata(metadata),
        )
        .unwrap();
    }
    obj.into()
}

#[wasm_bindgen]
pub fn compiler_der_decode(libweb_ptr: *mut LibWeb, der_hex: &str, options: &JsValue) -> JsValue {
    let der = match hex::decode(der_hex) {
        Ok(der) => der,
        Err(err) => return err.to_string().into(),
    };

    let mut reader = DerReader::new(&der, 0);
    let tlvs = match reader.read_all() {
        Ok(tlvs) => tlvs,
        Err(err) => return err.to_string().into(),
    };

    let decode_mode_kind = Reflect::get(options, &"mode".into())
        .expect("missing mode property")
        .as_string()
        .expect("typeof mode !== 'string'");
    let mode = match decode_mode_kind.as_str() {
        "contextless" => DecodeMode::Contextless,
        "specificType" => {
            let ident = Reflect::get(options, &"ident".into()).expect("missing ident property");
            let ident: crate::QualifiedIdentifier =
                serde_wasm_bindgen::from_value(ident).expect("deserialize ident");
            let ident = ident.try_into().expect("convert ident");

            let libweb = unsafe { Box::from_raw(libweb_ptr) };
            let source = match libweb.context.lookup_type(&ident) {
                Some(source) => source,
                None => {
                    let _ = Box::into_raw(libweb);
                    return format!("no such type: {}", ident).into();
                }
            };
            let resolved = match source.resolve(&libweb.context) {
                Ok(resolved) => resolved,
                Err(err) => {
                    let _ = Box::into_raw(libweb);
                    return err.kind.message().into();
                }
            };
            let _ = Box::into_raw(libweb);

            DecodeMode::SpecificType {
                source_ident: Some(ident),
                component_name: None,
                resolved,
            }
        }
        other => panic!("{}", other),
    };

    let libweb = unsafe { Box::from_raw(libweb_ptr) };
    let values = match tlvs
        .into_iter()
        .map(|tlv| DecodedValue::der_decode(&libweb.context, tlv, &mode))
        .collect::<DecodeResult<Vec<DecodedValue>>>()
    {
        Ok(values) => values,
        Err(err) => {
            let _ = Box::into_raw(libweb);
            return err.to_string().into();
        }
    };
    let _ = Box::into_raw(libweb);

    let arr = Array::new();
    for value in values {
        arr.push(&serialize_decoded_value(value).into());
    }
    arr.into()
}
