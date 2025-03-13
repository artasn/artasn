use std::{num::ParseIntError, panic};

use asn1chef::{
    compiler::{options::CompilerConfig, CompileError, Compiler, Context}, encoding::ber, module, types::{BuiltinType, Tag, TaggedType, UntaggedType}, values::{BuiltinValue, Oid, Value, ValueResolve}
};
use js_sys::{Array, Object, Reflect};
use serde::{Deserialize, Serialize};
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
    pub context: Context,
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

#[derive(Serialize, Deserialize)]
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

impl TryInto<module::ModuleIdentifier> for ModuleIdentifier {
    type Error = ParseIntError;

    fn try_into(self) -> Result<module::ModuleIdentifier, Self::Error> {
        Ok(module::ModuleIdentifier {
            name: self.name,
            oid: match self.oid {
                Some(oid) => Some(Oid(oid
                    .split(".")
                    .map(|node| node.parse::<u64>())
                    .collect::<Result<Vec<u64>, ParseIntError>>()?)),
                None => None,
            },
        })
    }
}

#[derive(Serialize, Deserialize)]
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

impl TryInto<module::QualifiedIdentifier> for QualifiedIdentifier {
    type Error = ParseIntError;

    fn try_into(self) -> Result<module::QualifiedIdentifier, Self::Error> {
        Ok(module::QualifiedIdentifier::new(self.module.try_into()?, self.name))
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

fn serialize_tagged_type(context: &Context, tagged_type: &TaggedType) -> JsValue {
    let (obj, mode) = match &tagged_type.ty {
        UntaggedType::Reference(reference) => {
            let obj = serde_wasm_bindgen::to_value(&QualifiedIdentifier::new(&reference.element))
                .expect("QualifiedIdentifier to_value");
            (obj, "reference")
        }
        UntaggedType::BuiltinType(builtin) => {
            let obj = serialize_builtin_type(context, builtin);
            (obj, "type")
        }
    };
    Reflect::set(&obj, &"mode".into(), &mode.into()).unwrap();
    if let Some(tag) = tagged_type.tag.as_ref() {
        Reflect::set(&obj, &"tag".into(), &serialize_type_tag(tag)).unwrap();
    }
    obj
}

fn serialize_type_tag(tag: &Tag) -> JsValue {
    let obj = Object::new();
    Reflect::set(&obj, &"class".into(), &tag.class.to_string().into()).unwrap();
    Reflect::set(&obj, &"num".into(), &tag.num.into()).unwrap();
    Reflect::set(&obj, &"kind".into(), &tag.kind.to_string().into()).unwrap();
    Reflect::set(&obj, &"source".into(), &tag.source.to_string().into()).unwrap();
    obj.into()
}

fn serialize_builtin_type(context: &Context, ty: &BuiltinType) -> JsValue {
    let obj = Object::new();
    Reflect::set(&obj, &"type".into(), &ty.to_string().into()).unwrap();
    match ty {
        BuiltinType::Structure(structure) => {
            let components = Array::new();
            for component in &structure.components {
                let obj = Object::new();
                Reflect::set(&obj, &"name".into(), &(&component.name.element).into()).unwrap();
                Reflect::set(
                    &obj,
                    &"componentType".into(),
                    &serialize_tagged_type(context, &component.component_type),
                )
                .unwrap();
                Reflect::set(&obj, &"optional".into(), &component.optional.into()).unwrap();
                if let Some(default_value) = component.default_value.as_ref() {
                    Reflect::set(
                        &obj,
                        &"defaultValue".into(),
                        &serialize_valuereference(context, &default_value.element),
                    )
                    .unwrap();
                }
                components.push(&obj.into());
            }
            Reflect::set(&obj, &"components".into(), &components.into()).unwrap();
        }
        BuiltinType::StructureOf(of) => {
            Reflect::set(
                &obj,
                &"componentType".into(),
                &serialize_tagged_type(context, &of.component_type),
            )
            .unwrap();
        }
        _ => (),
    }
    obj.into()
}

fn serialize_valuereference(context: &Context, valref: &Value) -> JsValue {
    let (obj, mode) = match valref {
        Value::Reference(reference) => {
            let obj = serde_wasm_bindgen::to_value(&QualifiedIdentifier::new(reference))
                .expect("QualifiedIdentifier to_value");
            (obj, "reference")
        }
        Value::BuiltinValue(value) => {
            let obj = serialize_value(context, value);
            (obj, "value")
        }
    };
    Reflect::set(&obj, &"mode".into(), &mode.into()).unwrap();
    obj
}

fn serialize_value(context: &Context, value: &BuiltinValue) -> JsValue {
    let obj = Object::new();
    Reflect::set(
        &obj,
        &"type".into(),
        &match value {
            BuiltinValue::SequenceOf(_) => String::from("SEQUENCE OF"),
            BuiltinValue::SetOf(_) => String::from("SET OF"),
            BuiltinValue::Choice(_) => String::from("CHOICE"),
            other => other.tag_type(context).expect("tag_type").to_string(),
        }
        .into(),
    )
    .unwrap();
    let (field, data) = match value {
        BuiltinValue::Boolean(boolean) => ("value", (*boolean).into()),
        BuiltinValue::Integer(int) => {
            let int_str: JsValue = int.to_string().into();
            let bigint = js_sys::BigInt::new(&int_str).expect("INTEGER -> bigint");
            ("value", bigint.into())
        }
        BuiltinValue::BitString(bits) => {
            let bits_str: JsValue = bits.to_string().into();
            let bigint = js_sys::BigInt::new(&bits_str).expect("BIT STRING -> bigint");
            ("value", bigint.into())
        }
        BuiltinValue::OctetString(octet_string) => {
            ("value", hex::encode_upper(octet_string).into())
        }
        BuiltinValue::Null => ("value", JsValue::null()),
        BuiltinValue::ObjectIdentifier(oid) => {
            let oid = oid.resolve_oid(context).expect("resolve oid");
            ("value", oid.to_string().into())
        }
        BuiltinValue::Sequence(sequence) => {
            let components = Array::new();
            for component in &sequence.components {
                let obj = Object::new();
                Reflect::set(&obj, &"name".into(), &(&component.name.element).into()).unwrap();
                Reflect::set(
                    &obj,
                    &"value".into(),
                    &serialize_valuereference(context, &component.value.element),
                )
                .unwrap();
                components.push(&obj.into());
            }
            ("components", components.into())
        }
        BuiltinValue::SequenceOf(seq_of) => {
            let elements = Array::new();
            for ast_element in seq_of {
                elements.push(&serialize_valuereference(context, &ast_element.element));
            }
            ("elements", elements.into())
        }
        BuiltinValue::CharacterString(_, str) => ("value", str.into()),
        other => todo!("{:#?}", other),
    };
    Reflect::set(&obj, &field.into(), &data).unwrap();
    obj.into()
}

#[wasm_bindgen]
pub fn libweb_init() -> *mut LibWeb {
    panic::set_hook(Box::new(console_error_panic_hook::hook));

    Box::into_raw(Box::new(LibWeb {
        compiler: Compiler::new(CompilerConfig::default()),
        context: Context::new(),
        buffer: Vec::with_capacity(4 * 1024),
    }))
}

#[wasm_bindgen]
pub unsafe fn libweb_deinit(libweb_ptr: *mut LibWeb) {
    std::mem::drop(Box::from_raw(libweb_ptr));
}

#[wasm_bindgen]
pub unsafe fn compiler_add_source(libweb_ptr: *mut LibWeb, path: String, source: String) -> JsValue {
    let mut libweb = Box::from_raw(libweb_ptr);
    let result = libweb.compiler.add_source(path, source);

    let _ = Box::into_raw(libweb);
    match result {
        Ok(()) => JsValue::null(),
        Err(err) => serde_wasm_bindgen::to_value(&JsCompileError::new(err))
            .expect("JsCompileError to_value"),
    }
}

#[wasm_bindgen]
pub unsafe fn compiler_remove_source(libweb_ptr: *mut LibWeb, path: String) -> JsValue {
    let mut libweb = Box::from_raw(libweb_ptr);
    let result = libweb.compiler.remove_source(&path);
    let _ = Box::into_raw(libweb);
    JsValue::from_bool(result)
}

#[wasm_bindgen]
pub unsafe fn compiler_compile(libweb_ptr: *mut LibWeb) -> JsValue {
    let mut libweb = Box::from_raw(libweb_ptr);
    libweb.context.clear();
    let errors = libweb.compiler.compile(&mut libweb.context);
    let _ = Box::into_raw(libweb);
    if errors.is_empty() {
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
pub unsafe fn compiler_list_modules(libweb_ptr: *mut LibWeb) -> JsValue {
    let libweb = Box::from_raw(libweb_ptr);
    let modules = libweb.context.list_modules();
    let definitions = modules
        .into_iter()
        .map(|module| ModuleIdentifier::new(&module.ident))
        .collect::<Vec<ModuleIdentifier>>();
    let _ = Box::into_raw(libweb);
    serde_wasm_bindgen::to_value(&definitions).expect("Vec<ModuleIdentifier> to_value")
}

#[wasm_bindgen]
pub unsafe fn compiler_list_types(libweb_ptr: *mut LibWeb) -> JsValue {
    let libweb = Box::from_raw(libweb_ptr);
    let types = Array::new();
    for (ident, ty) in libweb.context.list_types() {
        types.push(
            &TypeDefinition {
                ident: QualifiedIdentifier::new(&ident),
                ty: serialize_tagged_type(&libweb.context, ty),
            }
            .serialize(),
        );
    }
    let _ = Box::into_raw(libweb);
    types.into()
}

#[wasm_bindgen]
pub unsafe fn compiler_list_values(libweb_ptr: *mut LibWeb) -> JsValue {
    let libweb = Box::from_raw(libweb_ptr);
    let types = Array::new();
    for (ident, val) in libweb.context.list_values() {
        types.push(
            &ValueDefinition {
                ident: QualifiedIdentifier::new(&ident),
                ty: serialize_tagged_type(&libweb.context, &val.ty),
                value: serialize_valuereference(&libweb.context, &val.value.element),
            }
            .serialize(),
        );
    }
    let _ = Box::into_raw(libweb);
    types.into()
}

#[wasm_bindgen]
pub unsafe fn compiler_der_encode(
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
    let ident = module::QualifiedIdentifier::new(
        module::ModuleIdentifier {
            name: module_name,
            oid,
        },
        value_name,
    );
    let mut libweb = Box::from_raw(libweb_ptr);
    if let Some(value_decl) = libweb.context.lookup_value(&ident) {
        match value_decl.ty.resolve(&libweb.context) {
            Ok(ty) => match value_decl.value.resolve(&libweb.context) {
                Ok(value) => {
                    libweb.buffer.clear();
                    match ber::der_encode_value(&mut libweb.buffer, value, &libweb.context, &ty) {
                        Ok(()) => {
                            let mut reverse = Vec::with_capacity(libweb.buffer.len());
                            for b in libweb.buffer.iter().rev() {
                                reverse.push(*b);
                            }
                            let _ = Box::into_raw(libweb);
                            hex::encode_upper(&reverse)
                        }
                        Err(err) => {
                            let _ = Box::into_raw(libweb);
                            err.kind.to_string()
                        }
                    }
                }
                Err(err) => {
                    let _ = Box::into_raw(libweb);
                    err.kind.to_string()
                }
            },
            Err(err) => {
                let _ = Box::into_raw(libweb);
                err.kind.to_string()
            }
        }
    } else {
        let _ = Box::into_raw(libweb);
        "no such value".to_string()
    }
}
