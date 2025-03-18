use std::{collections::HashSet, num::ParseIntError, panic};

use asn1chef::{
    compiler::{options::CompilerConfig, CompileError, Compiler, Context},
    module,
    types::*,
    values::*,
};
use js_sys::{Array, Object, Reflect};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

mod codec;

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
        Ok(module::QualifiedIdentifier::new(
            self.module.try_into()?,
            self.name,
        ))
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
        UntaggedType::ObjectClassField(ocf) => {
            let obj = Object::new();
            Reflect::set(
                &obj,
                &"class".into(),
                &ocf.class_type.as_ref().element.into(),
            )
            .unwrap();
            Reflect::set(&obj, &"field".into(), &ocf.field.as_ref().element.into()).unwrap();
            Reflect::set(
                &obj,
                &"kind".into(),
                &(match &ocf.kind {
                    ObjectClassFieldReferenceKind::Value => "value",
                    ObjectClassFieldReferenceKind::OpenType => "openType",
                })
                .into(),
            )
            .unwrap();
            (obj.into(), "objectClassField")
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
                        &serialize_typed_value(context, &default_value.element),
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

fn serialize_typed_value(context: &Context, typed_value: &TypedValue) -> JsValue {
    let (obj, mode) = match &typed_value.value {
        ValueReference::Reference(reference) => {
            let obj = serde_wasm_bindgen::to_value(&QualifiedIdentifier::new(reference))
                .expect("QualifiedIdentifier to_value");
            (obj, "reference")
        }
        ValueReference::BuiltinValue(value) => {
            let obj = serialize_builtin_value(context, value, &typed_value.resolved_type);
            (obj, "value")
        }
    };
    Reflect::set(&obj, &"mode".into(), &mode.into()).unwrap();
    obj
}

fn serialize_builtin_value(
    context: &Context,
    value: &BuiltinValue,
    resolved_type: &ResolvedType,
) -> JsValue {
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
        BuiltinValue::Enumerated(enum_value) => {
            let name = 'block: {
                match &enum_value.element.value {
                    ValueReference::BuiltinValue(builtin) => {
                        let num: i64 = match builtin {
                            BuiltinValue::Integer(i) => i.try_into().unwrap(),
                            other => panic!("ENUMERATED is {:#?}", other),
                        };
                        match &resolved_type.ty {
                            BuiltinType::Enumerated(enumerated) => {
                                for item in enumerated {
                                    let item_num = match &item.value {
                                        EnumerationItemValue::Implied(implied) => *implied,
                                        EnumerationItemValue::Specified(specified) => {
                                            let specified = specified.resolve(context).unwrap();
                                            match &specified.value {
                                                BuiltinValue::Integer(i) => i.try_into().unwrap(),
                                                other => panic!(
                                                    "EnumerationItemValue::Specified is {:#?}",
                                                    other
                                                ),
                                            }
                                        }
                                    };
                                    if num == item_num {
                                        break 'block item.name.element.clone();
                                    }
                                }

                                break 'block String::from("<unknown>");
                            }
                            other => panic!("ENUMERATED value type is {}", other),
                        }
                    }
                    ValueReference::Reference(_) => panic!("ENUMERATED is reference"),
                }
            };
            ("value", name.into())
        }
        BuiltinValue::BitString(bs) => {
            let obj = Object::new();

            Reflect::set(&obj, &"data".into(), &bs.to_string().into()).unwrap();
            Reflect::set(&obj, &"unusedBits".into(), &bs.unused_bits.into()).unwrap();
            ("value", obj.into())
        }
        BuiltinValue::OctetString(octet_string) => {
            ("value", hex::encode_upper(octet_string).into())
        }
        BuiltinValue::Null => ("value", JsValue::null()),
        BuiltinValue::ObjectIdentifier(oid) => {
            let oid = oid.resolve_oid(context).expect("resolve oid");
            ("value", oid.to_string().into())
        }
        BuiltinValue::RealLiteral(lit) => ("value", lit.to_string().into()),
        BuiltinValue::Sequence(sequence) => {
            let components = Array::new();
            for component in &sequence.components {
                let obj = Object::new();
                Reflect::set(&obj, &"name".into(), &(&component.name.element).into()).unwrap();
                Reflect::set(
                    &obj,
                    &"value".into(),
                    &serialize_typed_value(context, &component.value.element),
                )
                .unwrap();
                components.push(&obj.into());
            }
            ("components", components.into())
        }
        BuiltinValue::SequenceOf(seq_of) => {
            let elements = Array::new();
            for ast_element in seq_of {
                elements.push(&serialize_typed_value(context, &ast_element.element));
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

    let mut compiler = Compiler::new(CompilerConfig::default());
    compiler.add_stdlib().unwrap();

    Box::into_raw(Box::new(LibWeb {
        compiler,
        context: Context::new(),
        buffer: Vec::with_capacity(4 * 1024),
    }))
}

#[wasm_bindgen]
pub unsafe fn libweb_deinit(libweb_ptr: *mut LibWeb) {
    std::mem::drop(Box::from_raw(libweb_ptr));
}

#[wasm_bindgen]
pub unsafe fn compiler_add_source(
    libweb_ptr: *mut LibWeb,
    path: String,
    source: String,
) -> JsValue {
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

lazy_static::lazy_static! {
    static ref IGNORE_MODULES: HashSet<module::ModuleIdentifier> = HashSet::from_iter(vec![
        module::ModuleIdentifier::with_name(String::from("CharacterString")),
        module::ModuleIdentifier::with_name(String::from("EmbeddedPDV")),
        module::ModuleIdentifier::with_name(String::from("External")),
        module::ModuleIdentifier::with_name(String::from("InformationObjectClasses")),
        module::ModuleIdentifier::with_name(String::from("Real")),
    ]);
}

#[wasm_bindgen]
pub unsafe fn compiler_list_types(libweb_ptr: *mut LibWeb) -> JsValue {
    let libweb = Box::from_raw(libweb_ptr);
    let types = Array::new();
    for (ident, ty_decl) in libweb.context.list_types() {
        if IGNORE_MODULES.contains(&ident.module) {
            continue;
        }
        types.push(
            &TypeDefinition {
                ident: QualifiedIdentifier::new(&ident),
                ty: serialize_tagged_type(&libweb.context, &ty_decl.ty),
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
        if IGNORE_MODULES.contains(&ident.module) {
            continue;
        }
        types.push(
            &ValueDefinition {
                ident: QualifiedIdentifier::new(&ident),
                ty: serialize_tagged_type(&libweb.context, &val.ty),
                value: serialize_typed_value(&libweb.context, &val.value.element),
            }
            .serialize(),
        );
    }
    let _ = Box::into_raw(libweb);
    types.into()
}
