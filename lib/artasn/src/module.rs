use std::{
    fmt::{Display, Write},
    marker::PhantomData,
};

use crate::{
    compiler::{parser::AstElement, Context},
    types::TaggedType,
    values::{BuiltinValue, Oid},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleIdentifier {
    pub name: String,
    pub oid: Option<Oid>,
}

impl ModuleIdentifier {
    pub fn with_name(name: String) -> ModuleIdentifier {
        ModuleIdentifier { name, oid: None }
    }

    pub fn with_id(name: String, oid: Oid) -> ModuleIdentifier {
        ModuleIdentifier {
            name,
            oid: Some(oid),
        }
    }

    /// Serialize the ModuleIdentifier to a format for use with [`crate::compiler::parser::ErrorKind::Foreign`].
    pub fn to_foreign_string(&self) -> String {
        let mut str = String::new();
        write!(&mut str, "{}", self.name).unwrap();
        if let Some(oid) = &self.oid {
            write!(&mut str, "/{}", oid).unwrap();
        }
        str
    }

    /// Deserialize the ModuleIdentifier from a [`crate::compiler::parser::ErrorKind::Foreign`] instance.
    pub fn from_foreign_string(str: &str) -> ModuleIdentifier {
        if let Some(index) = str.find('/') {
            let name = &str[..index];
            let oid = &str[index + 1..];
            let oid = Oid::parse_string(oid).expect("malformed oid in foreign string");
            ModuleIdentifier::with_id(name.to_string(), oid)
        } else {
            ModuleIdentifier::with_name(str.to_string())
        }
    }
}

impl ModuleIdentifier {
    pub fn compare(module_id: &ModuleIdentifier, ident_id: &ModuleIdentifier) -> bool {
        match (&module_id.oid, &ident_id.oid) {
            (Some(module_id), Some(ident_id)) => {
                // when matching two OIDs, the names don't have to match
                module_id == ident_id
            }
            (None, Some(_)) => {
                // if the module is declared without an OID,
                // but the resolving identifier has an OID specified,
                // then the resolution fails
                // see error A1130E in asn1studio for details
                false
            }
            (Some(_), None) => {
                // if the module is declared with an OID,
                // but the resolving identifier has no OID specified (i.e. by name only),
                // the resolution depends on the names matching
                module_id.name == ident_id.name
            }
            (None, None) => {
                // if the module and resolving identifier both have no OID,
                // the resolution depends on the names matching
                module_id.name == ident_id.name
            }
        }
    }
}

impl Display for ModuleIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)?;
        if let Some(id) = &self.oid {
            f.write_fmt(format_args!(" ({})", id))?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct QualifiedIdentifier {
    pub module: ModuleIdentifier,
    pub name: String,
}

impl QualifiedIdentifier {
    pub fn new(module: ModuleIdentifier, name: String) -> QualifiedIdentifier {
        QualifiedIdentifier { module, name }
    }
}

impl Display for QualifiedIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} FROM {}", self.name, self.module))
    }
}

pub struct QualifiedObject<T> {
    object_type: PhantomData<T>,
    pub ident: QualifiedIdentifier,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TagDefault {
    Explicit,
    Implicit,
    Automatic,
}

#[derive(Debug, Clone)]
pub enum Exports {
    All,
    SymbolList(Vec<String>),
}

#[derive(Debug)]
pub struct Module {
    pub header: ModuleHeader,
    pub types: Vec<TaggedType>,
    pub values: Vec<BuiltinValue>,
}

#[derive(Debug, Clone)]
pub struct ModuleHeader {
    pub ident: ModuleIdentifier,
    pub tag_default: TagDefault,
    pub extensibility_implied: bool,
    pub exports: Exports,
    pub imports: Vec<ImportsFromModule>,
    pub declarations: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ImportsFromModule {
    pub imports: Vec<AstElement<String>>,
    pub module: AstElement<ModuleIdentifier>,
}

pub enum ImportError {
    SymbolNotFound,
    ModuleNotFound(AstElement<ModuleIdentifier>),
    AmbiguousSymbol(Vec<QualifiedIdentifier>),
}

impl ModuleHeader {
    pub fn resolve_symbol(
        &self,
        context: &Context,
        symbol: &String,
    ) -> Result<QualifiedIdentifier, ImportError> {
        if self.has_export(symbol) {
            return Ok(QualifiedIdentifier::new(
                self.ident.clone(),
                symbol.to_string(),
            ));
        }
        self.resolve_import(context, symbol)
    }

    pub fn resolve_import(
        &self,
        context: &Context,
        symbol: &String,
    ) -> Result<QualifiedIdentifier, ImportError> {
        let mut resolutions = Vec::with_capacity(1);
        for imports_from_module in &self.imports {
            for import in &imports_from_module.imports {
                if &import.element == symbol {
                    let module = match &imports_from_module.module.element.oid {
                        Some(oid) => context.lookup_module_by_oid(oid),
                        None => {
                            context.lookup_module_by_name(&imports_from_module.module.element.name)
                        }
                    };
                    match module {
                        Some(module) => {
                            if module.declarations.contains(&import.element) {
                                return Ok(QualifiedIdentifier::new(
                                    module.ident.clone(),
                                    import.element.clone(),
                                ));
                            } else {
                                // try importing a proxied symbol
                                // for example:
                                //
                                // ModuleA:
                                //   IMPORTS Type from ModuleB
                                // ModuleB:
                                //   IMPORTS Type from ModuleC
                                // ModuleC:
                                //   Type ::= ...
                                resolutions.push(module.resolve_import(context, symbol)?);
                            }
                        }
                        None => {
                            return Err(ImportError::ModuleNotFound(
                                imports_from_module.module.clone(),
                            ))
                        }
                    }
                }
            }
        }

        if resolutions.is_empty() {
            Err(ImportError::SymbolNotFound)
        } else if resolutions.len() == 1 {
            Ok(resolutions.into_iter().next().unwrap())
        } else {
            Err(ImportError::AmbiguousSymbol(resolutions))
        }
    }

    /// Returns true if this module exports the provided symbol.
    pub fn has_export(&self, symbol: &String) -> bool {
        match &self.exports {
            Exports::All => self.declarations.contains(symbol),
            Exports::SymbolList(symbol_list) => symbol_list.contains(symbol),
        }
    }
}
