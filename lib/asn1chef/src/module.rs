use std::{fmt::Display, marker::PhantomData};

use crate::{
    types::TaggedType,
    values::{Oid, BuiltinValue},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleIdentifier {
    pub name: String,
    pub oid: Option<Oid>,
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
        f.write_str(&self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct QualifiedIdentifier {
    pub module: ModuleIdentifier,
    pub name: String,
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
    pub oid: ModuleIdentifier,
    pub tag_default: TagDefault,
    pub extensibility_implied: bool,
    pub exports: Exports,
    pub imports: Vec<QualifiedIdentifier>,
}

impl ModuleHeader {
    pub fn resolve_symbol(&self, symbol: &str) -> QualifiedIdentifier {
        self.resolve_import(symbol)
            .unwrap_or_else(|| QualifiedIdentifier {
                module: self.oid.clone(),
                name: symbol.to_string(),
            })
    }

    pub fn resolve_import(&self, symbol: &str) -> Option<QualifiedIdentifier> {
        for import in &self.imports {
            if import.name == symbol {
                return Some(import.clone());
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
