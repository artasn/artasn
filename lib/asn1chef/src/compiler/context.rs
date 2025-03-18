use indexmap::IndexMap;

use super::parser::{AstElement, AstTypeAssignment};
use crate::{
    module::{ModuleHeader, ModuleIdentifier, QualifiedIdentifier},
    types::{Class, InformationObjectClass, TaggedType},
    values::{ObjectClassReference, TypedValue},
};

#[derive(Debug)]
pub struct DeclaredValue {
    pub value: AstElement<TypedValue>,
    pub ty: TaggedType,
}

#[derive(Debug)]
pub struct DeclaredType {
    pub ty: TaggedType,
}

#[derive(Debug)]
pub struct Context {
    modules: IndexMap<ModuleIdentifier, ModuleHeader>,
    parameterized_types: IndexMap<QualifiedIdentifier, AstElement<AstTypeAssignment>>,
    types: IndexMap<QualifiedIdentifier, DeclaredType>,
    classes: IndexMap<QualifiedIdentifier, InformationObjectClass>,
    class_values: IndexMap<QualifiedIdentifier, ObjectClassReference>,
    class_sets: IndexMap<QualifiedIdentifier, Vec<ObjectClassReference>>,
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
            parameterized_types: IndexMap::new(),
            types: IndexMap::new(),
            classes: IndexMap::new(),
            class_values: IndexMap::new(),
            class_sets: IndexMap::new(),
            values: IndexMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.modules.clear();
        self.parameterized_types.clear();
        self.types.clear();
        self.classes.clear();
        self.class_values.clear();
        self.class_sets.clear();
        self.values.clear();
    }

    pub fn clear_parameterized_types(&mut self) {
        self.parameterized_types.clear();
    }

    pub fn list_modules(&self) -> Vec<&ModuleHeader> {
        self.modules.values().collect()
    }

    pub fn list_types(&self) -> Vec<(QualifiedIdentifier, &DeclaredType)> {
        self.types
            .iter()
            .map(|(ident, decl)| (ident.clone(), decl))
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

    pub fn register_type(&mut self, ident: QualifiedIdentifier, decl: DeclaredType) {
        self.types.insert(ident, decl);
    }

    pub fn register_information_object_class(
        &mut self,
        ident: QualifiedIdentifier,
        decl: InformationObjectClass,
    ) {
        self.classes.insert(ident, decl);
    }

    pub fn register_information_object_class_value(
        &mut self,
        ident: QualifiedIdentifier,
        decl: ObjectClassReference,
    ) {
        self.class_values.insert(ident, decl);
    }

    pub fn register_information_object_class_set(
        &mut self,
        ident: QualifiedIdentifier,
        decl: Vec<ObjectClassReference>,
    ) {
        self.class_sets.insert(ident, decl);
    }

    pub fn register_parameterized_type(
        &mut self,
        ident: QualifiedIdentifier,
        decl: AstElement<AstTypeAssignment>,
    ) {
        self.parameterized_types.insert(ident, decl);
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

    pub fn lookup_parameterized_type<'a>(
        &'a self,
        ident: &QualifiedIdentifier,
    ) -> Option<&'a AstElement<AstTypeAssignment>> {
        self.parameterized_types.get(ident)
    }

    pub fn lookup_type<'a>(&'a self, ident: &QualifiedIdentifier) -> Option<&'a DeclaredType> {
        self.types.get(ident)
    }

    pub fn lookup_information_object_class<'a>(
        &'a self,
        ident: &QualifiedIdentifier,
    ) -> Option<&'a InformationObjectClass> {
        self.classes.get(ident)
    }

    pub fn lookup_information_object_class_value<'a>(
        &'a self,
        ident: &QualifiedIdentifier,
    ) -> Option<&'a ObjectClassReference> {
        self.class_values.get(ident)
    }

    pub fn lookup_information_object_class_set<'a>(
        &'a self,
        ident: &QualifiedIdentifier,
    ) -> Option<&'a Vec<ObjectClassReference>> {
        self.class_sets.get(ident)
    }

    pub fn lookup_type_mut<'a>(
        &'a mut self,
        ident: &QualifiedIdentifier,
    ) -> Option<&'a mut DeclaredType> {
        self.types.get_mut(ident)
    }

    pub fn lookup_value<'a>(&'a self, ident: &QualifiedIdentifier) -> Option<&'a DeclaredValue> {
        self.values.get(ident)
    }

    pub fn lookup_type_by_tag(&self, class: Class, num: u16) -> Option<&DeclaredType> {
        self.types.values().find(|decl| {
            decl.ty
                .tag
                .as_ref()
                .map(|tag| tag.class == class && tag.num == num)
                .unwrap_or(false)
        })
    }
}
