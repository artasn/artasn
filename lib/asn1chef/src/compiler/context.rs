use indexmap::IndexMap;

use super::parser::AstElement;
use crate::{
    module::{ModuleHeader, ModuleIdentifier, QualifiedIdentifier},
    types::{Class, TaggedType},
    values::Value,
};

#[derive(Debug)]
pub struct DeclaredValue {
    pub value: AstElement<Value>,
    pub ty: TaggedType,
}

#[derive(Debug)]
pub struct Context {
    modules: IndexMap<ModuleIdentifier, ModuleHeader>,
    types: IndexMap<QualifiedIdentifier, TaggedType>,
    values: IndexMap<QualifiedIdentifier, DeclaredValue>,
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Context {
        Context {
            modules: IndexMap::new(),
            types: IndexMap::new(),
            values: IndexMap::new(),
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
        self.modules.insert(module.ident.clone(), module);
    }

    pub fn register_type(&mut self, ident: QualifiedIdentifier, typeref: TaggedType) {
        self.types.insert(ident, typeref);
    }

    pub fn register_value(&mut self, ident: QualifiedIdentifier, val: DeclaredValue) {
        self.values.insert(ident, val);
    }

    pub fn lookup_module_by_name<'a>(&'a self, name: &str) -> Option<&'a ModuleHeader> {
        self.modules
            .values()
            .find(|value| value.ident.name.as_str() == name)
    }

    pub fn lookup_module<'a>(&'a self, ident: &ModuleIdentifier) -> Option<&'a ModuleHeader> {
        self.modules.get(ident)
    }

    pub fn lookup_type<'a>(&'a self, ident: &QualifiedIdentifier) -> Option<&'a TaggedType> {
        self.types.get(ident)
    }

    pub fn lookup_type_mut<'a>(
        &'a mut self,
        ident: &QualifiedIdentifier,
    ) -> Option<&'a mut TaggedType> {
        self.types.get_mut(ident)
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
