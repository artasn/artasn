use crate::{
    module::{ModuleHeader, ModuleIdentifier, QualifiedIdentifier},
    types::{Class, TaggedType},
    values::Value,
};
use std::{collections::BTreeMap, mem::MaybeUninit};

use super::parser::AstElement;

static mut CONTEXT: MaybeUninit<Context> = MaybeUninit::uninit();

pub fn init_context() {
    unsafe {
        CONTEXT = MaybeUninit::new(Context::new());
    }
}

pub fn context<'a>() -> &'a Context {
    unsafe { CONTEXT.assume_init_ref() }
}

pub fn context_mut<'a>() -> &'a mut Context {
    unsafe { CONTEXT.assume_init_mut() }
}

#[derive(Debug)]
pub struct DeclaredValue {
    pub value: AstElement<Value>,
    pub ty: TaggedType,
}

#[derive(Debug)]
pub struct Context {
    modules: BTreeMap<ModuleIdentifier, ModuleHeader>,
    types: BTreeMap<QualifiedIdentifier, TaggedType>,
    values: BTreeMap<QualifiedIdentifier, DeclaredValue>,
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Context {
        Context {
            modules: BTreeMap::new(),
            types: BTreeMap::new(),
            values: BTreeMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.modules.clear();
        self.types.clear();
        self.values.clear();
    }

    pub fn list_modules(&self) -> Vec<&ModuleHeader> {
        self.modules.values().collect()
    }

    pub fn list_types(&self) -> Vec<(QualifiedIdentifier, &TaggedType)> {
        self.types
            .iter()
            .map(|(ident, typeref)| (ident.clone(), typeref))
            .collect()
    }

    pub fn list_values(&self) -> Vec<(QualifiedIdentifier, &DeclaredValue)> {
        self.values
            .iter()
            .map(|(ident, val)| (ident.clone(), val))
            .collect()
    }

    pub fn register_module(&mut self, module: ModuleHeader) {
        self.modules.insert(module.oid.clone(), module);
    }

    pub fn register_type(&mut self, ident: QualifiedIdentifier, typeref: TaggedType) {
        self.types.insert(ident, typeref);
    }

    pub fn register_value(&mut self, ident: QualifiedIdentifier, val: DeclaredValue) {
        self.values.insert(ident, val);
    }

    pub fn lookup_module<'a>(&'a self, ident: &ModuleIdentifier) -> Option<&'a ModuleHeader> {
        self.modules.get(ident)
    }

    pub fn lookup_type<'a>(&'a self, ident: &QualifiedIdentifier) -> Option<&'a TaggedType> {
        self.types.get(ident)
    }

    pub fn lookup_value<'a>(&'a self, ident: &QualifiedIdentifier) -> Option<&'a DeclaredValue> {
        self.values.get(ident)
    }

    pub fn lookup_type_by_tag(&self, class: Class, num: u16) -> Option<&TaggedType> {
        self.types.values().find(|ty| {
            ty.tag
                .as_ref()
                .map(|tag| tag.class == class && tag.num == num)
                .unwrap_or(false)
        })
    }
}
