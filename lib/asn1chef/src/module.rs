use std::{
    fmt::{Display, Write},
    marker::PhantomData,
};

use crate::{
    compiler::Context,
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
    pub imports: Vec<QualifiedIdentifier>,
}

impl ModuleHeader {
    pub fn resolve_symbol(&self, context: &Context, symbol: &str) -> QualifiedIdentifier {
        self.resolve_import(context, symbol)
            .unwrap_or_else(|| QualifiedIdentifier::new(self.ident.clone(), symbol.to_string()))
    }

    pub fn resolve_import(&self, context: &Context, symbol: &str) -> Option<QualifiedIdentifier> {
        for import in &self.imports {
            if import.name == symbol {
                return Some(match import.module.oid {
                    Some(_) => import.clone(),
                    None => match context.lookup_module_by_name(&import.module.name) {
                        Some(module) => {
                            QualifiedIdentifier::new(module.ident.clone(), import.name.clone())
                        }
                        None => return None,
                    },
                });
            }
        }

        None
    }

    /// Returns true if this module exports the provided symbol.
    /// Note that this does not check that the symbol is actually defined;
    /// it only checks that the symbol is exported, either explicitly or implicitly.
    ///
    /// Symbols are exported explicitly via `EXPORTS symbol1, symbol2;`
    /// Symbols are exported implicitly via `EXPORTS ALL;` or via the lack of an `EXPORTS` definition.
    pub fn has_export(&self, symbol: String) -> bool {
        match &self.exports {
            Exports::All => true,
            Exports::SymbolList(symbol_list) => symbol_list.contains(&symbol),
        }
    }
}
