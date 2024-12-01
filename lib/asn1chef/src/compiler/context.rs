use crate::{
    module::{ModuleHeader, ModuleIdentifier, QualifiedIdentifier},
    types::typeref,
    values::valref,
};
use std::{collections::HashMap, mem::MaybeUninit};

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
    pub value: valref!(),
    pub ty: typeref!(),
}

#[derive(Debug)]
pub struct Context {
    modules: HashMap<ModuleIdentifier, ModuleHeader>,
    types: HashMap<QualifiedIdentifier, typeref!()>,
    values: HashMap<QualifiedIdentifier, DeclaredValue>,
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Context {
        Context {
            modules: HashMap::new(),
            types: HashMap::new(),
            values: HashMap::new(),
        }
    }

    pub fn list_modules(&self) -> Vec<&ModuleHeader> {
        self.modules.values().collect()
    }

    pub fn list_types(&self) -> Vec<(QualifiedIdentifier, &typeref!())> {
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
        self.modules.insert(module.id.clone(), module);
    }

    pub fn register_type(&mut self, ident: QualifiedIdentifier, typeref: typeref!()) {
        self.types.insert(ident, typeref);
    }

    pub fn register_value(&mut self, ident: QualifiedIdentifier, val: DeclaredValue) {
        self.values.insert(ident, val);
    }

    pub fn lookup_module<'a>(&'a self, ident: &ModuleIdentifier) -> Option<&'a ModuleHeader> {
        self.modules.get(ident)
    }

    pub fn lookup_type<'a>(&'a self, ident: &QualifiedIdentifier) -> Option<&'a typeref!()> {
        self.types.get(ident)
    }

    pub fn lookup_value<'a>(&'a self, ident: &QualifiedIdentifier) -> Option<&'a DeclaredValue> {
        self.values.get(ident)
    }

    // pub fn construct_modules(&self) -> anyhow::Result<Vec<Module>> {
    //     let mut modules = Vec::new();
    //     for header in &self.modules {
    //         let types = self
    //             .types
    //             .iter()
    //             .filter(|(key, _)| ModuleIdentifier::compare(&header.id, &key.module))
    //             .map(|(_, typeref)| typeref.resolve().cloned())
    //             .collect::<anyhow::Result<Vec<TaggedType>>>()?;
    //         let values = self
    //             .values
    //             .iter()
    //             .filter(|(key, _)| ModuleIdentifier::compare(&header.id, &key.module))
    //             .map(|(_, valref)| valref.resolve().cloned())
    //             .collect::<anyhow::Result<Vec<Value>>>()?;

    //         modules.push(Module {
    //             header: header.clone(),
    //             types,
    //             values,
    //         });
    //     }
    //     Ok(modules)
    // }
}
