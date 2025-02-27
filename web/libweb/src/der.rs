use std::io;

use asn1chef::encoding::{DecodedValue, DecodedValueKind, DerReader, TlvElement, TlvPos, TlvTag};
use js_sys::{Array, BigInt, Object, Reflect};
use wasm_bindgen::{prelude::*, JsValue};

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

fn serialize_decoded_value_kind(kind: DecodedValueKind) -> JsValue {
    let obj = Object::new();
    let (field, ty, data) = match kind {
        DecodedValueKind::Raw(data) => ("data", "RAW", hex::encode_upper(data).into()),
        DecodedValueKind::Boolean(data) => ("data", "BOOLEAN", data.into()),
        DecodedValueKind::Integer(data) => {
            let int_str = data.to_string().into();
            let bigint = BigInt::new(&int_str).expect("INTEGER -> bigint");
            ("data", "INTEGER", bigint.into())
        }
        DecodedValueKind::BitString(data) => {
            let bit_str = data.to_string().into();
            let bigint = BigInt::new(&bit_str).expect("BIT STRING -> bigint");
            ("data", "BIT STRING", bigint.into())
        }
        DecodedValueKind::OctetString(data) => {
            ("data", "OCTET STRING", hex::encode_upper(data).into())
        }
        DecodedValueKind::Null => ("data", "NULL", JsValue::null()),
        DecodedValueKind::ObjectIdentifier(data) => {
            ("data", "OBJECT IDENTIFIER", data.to_string().into())
        }
        DecodedValueKind::Real(data) => ("data", "REAL", data.to_string().into()),
        DecodedValueKind::Sequence(elements) => {
            let arr = Array::new();
            for element in elements {
                arr.push(&serialize_decoded_value(element));
            }
            ("elements", "SEQUENCE", arr.into())
        }
    };
    Reflect::set(&obj, &"type".into(), &ty.into()).unwrap();
    Reflect::set(&obj, &field.into(), &data).unwrap();
    obj.into()
}

fn serialize_decoded_value(value: DecodedValue) -> JsValue {
    let obj = Object::new();
    Reflect::set(&obj, &"tag".into(), &serialize_tlv_tag(value.tag)).unwrap();
    Reflect::set(&obj, &"len".into(), &serialize_tlv_len(value.len)).unwrap();
    Reflect::set(&obj, &"valuePos".into(), &serialize_tlv_pos(value.value_pos)).unwrap();
    Reflect::set(
        &obj,
        &"kind".into(),
        &serialize_decoded_value_kind(value.kind),
    )
    .unwrap();
    obj.into()
}

#[wasm_bindgen]
pub fn der_decode(der_hex: &str) -> JsValue {
    let der = match hex::decode(der_hex) {
        Ok(der) => der,
        Err(err) => return err.to_string().into(),
    };

    let mut reader = DerReader::new(&der, 0);
    let tlvs = match reader.read_all() {
        Ok(tlvs) => tlvs,
        Err(err) => return err.to_string().into(),
    };
    let values = match tlvs
        .into_iter()
        .map(|tlv| DecodedValue::der_decode(tlv))
        .collect::<io::Result<Vec<DecodedValue>>>()
    {
        Ok(values) => values,
        Err(err) => return err.to_string().into(),
    };

    let arr = Array::new();
    for value in values {
        arr.push(&serialize_decoded_value(value).into());
    }
    arr.into()
}
