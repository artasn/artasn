use std::panic;

use asn1chef::{
    compiler::{CompileError, Compiler},
    module,
    types::{BuiltinType, Tag, TagType, TaggedType, UntaggedType},
    values::{Oid, Value, ValueReference, ValueResolve},
};
use js_sys::{Array, Object, Reflect};
use serde::Serialize;
use wasm_bindgen::prelude::*;

mod der;

#[cfg(target_arch = "wasm32")]
use lol_alloc::{AssumeSingleThreaded, FreeListAllocator};

#[cfg(target_arch = "wasm32")]
#[global_allocator]
static ALLOCATOR: AssumeSingleThreaded<FreeListAllocator> =
    unsafe { AssumeSingleThreaded::new(FreeListAllocator::new()) };

pub struct LibWeb {
    pub compiler: Compiler,
    pub buffer: Vec<u8>,
}

#[derive(Serialize)]
pub struct JsCompileError {
    pub message: String,
    pub path: String,
    pub line: usize,
    pub col: usize,
    pub len: usize,
}

impl JsCompileError {
    pub fn new(err: CompileError) -> JsCompileError {
        let (line, col) = err.pos();
        JsCompileError {
            message: format!("{}: {}", err.phase.get_error_prefix(), err.message()),
            len: err.len(),
            path: err.path,
            line,
            col,
        }
    }
}

#[derive(Serialize)]
pub struct ModuleIdentifier {
    pub name: String,
    pub oid: Option<String>,
}

impl ModuleIdentifier {
    pub fn new(module: &module::ModuleIdentifier) -> ModuleIdentifier {
        ModuleIdentifier {
            name: module.name.clone(),
            oid: module.oid.as_ref().map(|oid| oid.to_string()),
        }
    }
}

#[derive(Serialize)]
pub struct QualifiedIdentifier {
    pub module: ModuleIdentifier,
    pub name: String,
}

impl QualifiedIdentifier {
    pub fn new(ident: &module::QualifiedIdentifier) -> QualifiedIdentifier {
        QualifiedIdentifier {
            module: ModuleIdentifier::new(&ident.module),
            name: ident.name.clone(),
        }
    }
}

pub struct TypeDefinition {
    pub ident: QualifiedIdentifier,
    pub ty: JsValue,
}

impl TypeDefinition {
    pub fn serialize(&self) -> JsValue {
        let obj = Object::new();
        Reflect::set(
            &obj,
            &"ident".into(),
            &serde_wasm_bindgen::to_value(&self.ident).expect("QualifiedIdentifier to_value"),
        )
        .unwrap();
        Reflect::set(&obj, &"ty".into(), &self.ty).unwrap();
        obj.into()
    }
}

pub struct ValueDefinition {
    pub ident: QualifiedIdentifier,
    pub ty: JsValue,
    pub value: JsValue,
}

impl ValueDefinition {
    pub fn serialize(&self) -> JsValue {
        let obj = Object::new();
        Reflect::set(
            &obj,
            &"ident".into(),
            &serde_wasm_bindgen::to_value(&self.ident).unwrap(),
        )
        .unwrap();
        Reflect::set(&obj, &"ty".into(), &self.ty).unwrap();
        Reflect::set(&obj, &"value".into(), &self.value).unwrap();
        obj.into()
    }
}

fn serialize_tagged_type(tagged_type: &TaggedType) -> JsValue {
    let (obj, mode) = match &tagged_type.ty {
        UntaggedType::Reference(reference) => {
            let obj = serde_wasm_bindgen::to_value(&QualifiedIdentifier::new(&reference.element))
                .expect("QualifiedIdentifier to_value");
            (obj, "reference")
        }
        UntaggedType::BuiltinType(builtin) => {
            let obj = serialize_builtin_type(builtin);
            (obj, "type")
        }
    };
    Reflect::set(&obj, &"mode".into(), &mode.into()).unwrap();
    Reflect::set(&obj, &"tag".into(), &serialize_type_tag(&tagged_type.tag)).unwrap();
    obj
}

fn serialize_type_tag(tag: &Tag) -> JsValue {
    let obj = Object::new();
    Reflect::set(&obj, &"class".into(), &tag.class.to_string().into()).unwrap();
    if let Some(num) = tag.num.clone() {
        Reflect::set(&obj, &"num".into(), &num.into()).unwrap();
    }
    Reflect::set(&obj, &"kind".into(), &tag.kind.to_string().into()).unwrap();
    obj.into()
}

fn serialize_builtin_type(ty: &BuiltinType) -> JsValue {
    let obj = Object::new();
    Reflect::set(&obj, &"type".into(), &ty.to_string().into()).unwrap();
    match ty {
        BuiltinType::Sequence(sequence) => {
            let components = Array::new();
            for component in &sequence.components {
                let obj = Object::new();
                Reflect::set(&obj, &"name".into(), &(&component.name.element).into()).unwrap();
                Reflect::set(
                    &obj,
                    &"componentType".into(),
                    &serialize_tagged_type(&component.component_type),
                )
                .unwrap();
                Reflect::set(&obj, &"optional".into(), &component.optional.into()).unwrap();
                if let Some(default_value) = component.default_value.as_ref() {
                    Reflect::set(
                        &obj,
                        &"defaultValue".into(),
                        &serialize_valuereference(&default_value.element),
                    )
                    .unwrap();
                }
                components.push(&obj.into());
            }
            Reflect::set(&obj, &"components".into(), &components.into()).unwrap();
        }
        BuiltinType::SequenceOf(sequence) => {
            Reflect::set(
                &obj,
                &"componentType".into(),
                &serialize_tagged_type(&sequence.component_type).into(),
            )
            .unwrap();
        }
        _ => (),
    }
    obj.into()
}

fn serialize_valuereference(valref: &ValueReference<{ TagType::Any as u8 }>) -> JsValue {
    let (obj, mode) = match valref {
        ValueReference::Reference(reference) => {
            let obj = serde_wasm_bindgen::to_value(&QualifiedIdentifier::new(reference))
                .expect("QualifiedIdentifier to_value");
            (obj, "reference")
        }
        ValueReference::Value(value) => {
            let obj = serialize_value(value);
            (obj, "value")
        }
    };
    Reflect::set(&obj, &"mode".into(), &mode.into()).unwrap();
    obj
}

fn serialize_value(value: &Value) -> JsValue {
    let obj = Object::new();
    Reflect::set(&obj, &"type".into(), &value.type_name().into()).unwrap();
    let (field, data) = match value {
        Value::Boolean(boolean) => ("value", (*boolean).into()),
        Value::Integer(int) => {
            let int_str: JsValue = int.to_string().into();
            let bigint = js_sys::BigInt::new(&int_str).expect("INTEGER -> bigint");
            ("value", bigint.into())
        }
        Value::BitString(bits) => {
            let bits_str: JsValue = bits.to_string().into();
            let bigint = js_sys::BigInt::new(&bits_str).expect("BIT STRING -> bigint");
            ("value", bigint.into())
        }
        Value::OctetString(octet_string) => ("value", hex::encode_upper(octet_string).into()),
        Value::Null => ("value", JsValue::null()),
        Value::ObjectIdentifier(oid) => {
            let oid = oid.resolve_oid().expect("resolve oid");
            ("value", oid.to_string().into())
        }
        Value::Sequence(sequence) => {
            let components = Array::new();
            for component in &sequence.components {
                let obj = Object::new();
                Reflect::set(&obj, &"name".into(), &(&component.name.element).into()).unwrap();
                Reflect::set(
                    &obj,
                    &"value".into(),
                    &serialize_valuereference(&component.value.element),
                )
                .unwrap();
                components.push(&obj.into());
            }
            ("components", components.into())
        }
        Value::SequenceOf(seq_of) => {
            let elements = Array::new();
            for ast_element in seq_of {
                elements.push(&serialize_valuereference(&ast_element.element).into());
            }
            ("elements", elements.into())
        }
        other => todo!("{:#?}", other),
    };
    Reflect::set(&obj, &field.into(), &data).unwrap();
    obj.into()
}

#[wasm_bindgen]
pub fn libweb_init() -> *mut LibWeb {
    panic::set_hook(Box::new(console_error_panic_hook::hook));

    Box::into_raw(Box::new(LibWeb {
        compiler: Compiler::new(),
        buffer: Vec::with_capacity(4 * 1024),
    }))
}

#[wasm_bindgen]
pub fn libweb_deinit(libweb_ptr: *mut LibWeb) {
    unsafe {
        std::mem::drop(Box::from_raw(libweb_ptr));
    }
}

#[wasm_bindgen]
pub fn compiler_add_source(libweb_ptr: *mut LibWeb, path: String, source: String) -> JsValue {
    let mut libweb = unsafe { Box::from_raw(libweb_ptr) };
    let result = libweb.compiler.add_source(path, source);

    let _ = Box::into_raw(libweb);
    match result {
        Ok(()) => JsValue::null(),
        Err(err) => serde_wasm_bindgen::to_value(&JsCompileError::new(err))
            .expect("JsCompileError to_value"),
    }
}

#[wasm_bindgen]
pub fn compiler_remove_source(libweb_ptr: *mut LibWeb, path: String) -> JsValue {
    let mut libweb = unsafe { Box::from_raw(libweb_ptr) };
    let result = libweb.compiler.remove_source(&path);
    let _ = Box::into_raw(libweb);
    JsValue::from_bool(result)
}

#[wasm_bindgen]
pub fn compiler_compile(libweb_ptr: *mut LibWeb) -> JsValue {
    let libweb = unsafe { Box::from_raw(libweb_ptr) };
    let errors = libweb.compiler.compile();
    let _ = Box::into_raw(libweb);
    if errors.len() == 0 {
        JsValue::null()
    } else {
        let array = Array::new();
        for err in errors {
            array.push(
                &serde_wasm_bindgen::to_value(&JsCompileError::new(err))
                    .expect("JsCompileError to_value"),
            );
        }
        array.into()
    }
}

#[wasm_bindgen]
pub fn context_list_modules() -> JsValue {
    let modules = Compiler::get_context().list_modules();
    let definitions = modules
        .into_iter()
        .map(|module| ModuleIdentifier::new(&module.oid))
        .collect::<Vec<ModuleIdentifier>>();
    serde_wasm_bindgen::to_value(&definitions).expect("Vec<ModuleIdentifier> to_value")
}

#[wasm_bindgen]
pub fn context_list_types() -> JsValue {
    let types = Array::new();
    for (ident, ty) in Compiler::get_context().list_types() {
        types.push(
            &TypeDefinition {
                ident: QualifiedIdentifier::new(&ident),
                ty: serialize_tagged_type(ty),
            }
            .serialize(),
        );
    }
    types.into()
}

#[wasm_bindgen]
pub fn context_list_values() -> JsValue {
    let types = Array::new();
    for (ident, val) in Compiler::get_context().list_values() {
        types.push(
            &ValueDefinition {
                ident: QualifiedIdentifier::new(&ident),
                ty: serialize_tagged_type(&val.ty),
                value: serialize_valuereference(&val.value.element),
            }
            .serialize(),
        );
    }
    types.into()
}

#[wasm_bindgen]
pub fn context_der_encode(
    libweb_ptr: *mut LibWeb,
    module_name: String,
    oid: Option<String>,
    value_name: String,
) -> String {
    let oid = oid.map(|oid| {
        Oid(oid
            .split(".")
            .map(|node| node.parse::<u64>().expect("parse oid node"))
            .collect())
    });
    let ident = module::QualifiedIdentifier {
        module: module::ModuleIdentifier {
            name: module_name,
            oid,
        },
        name: value_name,
    };
    if let Some(value_decl) = Compiler::get_context().lookup_value(&ident) {
        match value_decl.ty.resolve() {
            Ok(ty) => match value_decl.value.resolve() {
                Ok(value) => {
                    let mut libweb = unsafe { Box::from_raw(libweb_ptr) };
                    libweb.buffer.clear();
                    match value.der_encode(&mut libweb.buffer, &ty) {
                        Ok(()) => {
                            let mut reverse = Vec::with_capacity(libweb.buffer.len());
                            for b in libweb.buffer.iter().rev() {
                                reverse.push(*b);
                            }
                            hex::encode_upper(&reverse)
                        }
                        Err(err) => err.kind.to_string(),
                    }
                }
                Err(err) => err.kind.to_string(),
            },
            Err(err) => err.kind.to_string(),
        }
    } else {
        "no such value".to_string()
    }
}
