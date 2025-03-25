pub(crate) mod class;
pub(crate) mod constraints;
pub(crate) mod object_id;
pub(crate) mod types;
pub(crate) mod util;
pub(crate) mod values;
pub(crate) mod verify;

use class::ObjectClassAssignmentParseMode;
use types::TypeAssignmentParseMode;
use values::{ParseValueAssignmentStage, ParsedValueAssignment};

use super::options::CompilerConfig;
use super::{parser::*, Compiler, Context};
use crate::compiler::options::EmptyExportBehavior;
use crate::module::ModuleIdentifier;
use crate::module::*;
use crate::types::{InformationObjectClass, InformationObjectClassReference};

#[derive(Debug)]
pub struct AstParser<'a> {
    pub context: &'a Context,
    pub config: &'a CompilerConfig,
    pub ast_module: &'a AstElement<AstModule>,
    pub module: ModuleIdentifier,
    pub(crate) compiler: &'a Compiler,
}

impl AstParser<'_> {
    pub fn resolve_symbol(
        &self,
        symbol: &AstElement<&String>,
    ) -> Result<AstElement<QualifiedIdentifier>> {
        Ok(AstElement::new(
            self.context
                .lookup_module(&self.module)
                .expect("lookup_module")
                .resolve_symbol(self.context, symbol.element)
                .map_err(|err| Error {
                    kind: ErrorKind::Ast(format!(
                        "failed to resolve symbol '{}' ({})",
                        symbol.element,
                        match err {
                            ImportError::ModuleNotFound(module) =>
                                format!("module '{}' not found", module.element),
                            ImportError::SymbolNotFound => "symbol not found".to_string(),
                            ImportError::AmbiguousSymbol(symbols) => format!(
                                "ambigous symbol defined in modules {}",
                                symbols
                                    .into_iter()
                                    .map(|symbol| symbol.module.to_string())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        }
                    )),
                    loc: symbol.loc,
                })?,
            symbol.loc,
        ))
    }

    pub fn run_with_context<T, F: Fn(&Self) -> Result<T>>(
        &self,
        target_module: &ModuleIdentifier,
        f: F,
    ) -> Result<T> {
        let (_, ast_target_module) = self
            .compiler
            .find_source_by_ident(target_module)
            .expect("find_source_by_ident failed");

        let context_parser = AstParser {
            context: self.context,
            config: self.config,
            ast_module: ast_target_module,
            module: target_module.clone(),
            compiler: self.compiler,
        };
        f(&context_parser).map_err(|err| err.into_foreign(target_module.to_foreign_string()))
    }
}

pub struct AstVerifier<'a> {
    pub context: &'a Context,
    pub config: &'a CompilerConfig,
    pub module: ModuleIdentifier,
}

fn run_parser<T, F: Fn(AstParser<'_>) -> Vec<Result<T>>>(
    context: &Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
    f: F,
) -> std::result::Result<Vec<T>, Vec<Error>> {
    let mut results = Vec::new();
    let mut errors = Vec::new();

    for ast_module in &program.element.0 {
        let module = match module_ast_to_module_ident(&ast_module.element.header) {
            Ok(module) => module,
            Err(err) => {
                errors.push(err);
                continue;
            }
        };

        let parser = AstParser {
            context,
            config: &compiler.config,
            ast_module,
            module,
            compiler,
        };

        for result in f(parser) {
            match result {
                Ok(result) => results.push(result),
                Err(err) => errors.push(err),
            }
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(results)
    }
}

fn run_verifier<F: Fn(AstVerifier<'_>) -> Vec<Error>>(
    context: &Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
    f: F,
) -> Vec<Error> {
    let mut errors = Vec::new();

    for ast_module in &program.element.0 {
        let module = match module_ast_to_module_ident(&ast_module.element.header) {
            Ok(module) => module,
            Err(err) => {
                errors.push(err);
                continue;
            }
        };

        let verifier = AstVerifier {
            context,
            config: &compiler.config,
            module,
        };

        errors.extend(f(verifier));
    }

    errors
}

pub(crate) fn module_ast_to_module_ident(
    header: &AstElement<AstModuleHeader>,
) -> Result<ModuleIdentifier> {
    object_id::name_and_oid_to_module_ident(
        &header.element.name,
        header.element.oid.as_ref().map(|oid| match &oid.element {
            AstModuleIdentifier::DefinitiveOid(oid) => oid,
            AstModuleIdentifier::DefinitiveOidWithIri(oid_with_iri) => &oid_with_iri.element.oid,
        }),
    )
}

fn module_ref_to_module_ident(
    module_ref: &AstElement<AstGlobalModuleReference>,
) -> Result<AstElement<ModuleIdentifier>> {
    let oid = match &module_ref.element.oid {
        Some(oid) => match &oid.element {
            AstAssignedIdentifier::DefinitiveOid(oid) => Some(oid),
            // TODO: implement import by valuereference for oid
            AstAssignedIdentifier::ValueIdentifier(_) => None,
        },
        None => None,
    };
    Ok(AstElement::new(
        object_id::name_and_oid_to_module_ident(&module_ref.element.name, oid)?,
        module_ref.loc,
    ))
}

fn symbol_list_to_string_list(symbol_list: &AstSymbolList) -> Vec<AstElement<String>> {
    symbol_list
        .0
        .iter()
        .map(|symbol| {
            match match &symbol.element {
                AstSymbol::ParameterizedReference(parameterized) => {
                    &parameterized.element.0.element
                }
                AstSymbol::Reference(reference) => &reference.element,
            } {
                AstReference::TypeReference(type_ref) => {
                    type_ref.as_ref().map(|name| name.0.clone())
                }
                AstReference::ValueReference(val_ref) => {
                    val_ref.as_ref().map(|name| name.0.clone())
                }
            }
        })
        .collect()
}

/// Stage 1: register all module headers.
pub fn register_all_modules(
    context: &mut Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, compiler, program, |parser| {
        let header = &parser.ast_module.element.header;

        let tag_default = match header
            .element
            .tag_default
            .as_ref()
            .map(|e| &e.element.0.element)
        {
            Some(tag_default) => match tag_default {
                AstTagDefaultKind::TagDefaultExplicit(_) => TagDefault::Explicit,
                AstTagDefaultKind::TagDefaultAutomatic(_) => TagDefault::Automatic,
                AstTagDefaultKind::TagDefaultImplicit(_) => TagDefault::Implicit,
            },
            None => TagDefault::Explicit,
        };
        let extensibility_implied = header.element.extensibility_implied.is_some();
        let exports = match header
            .element
            .exports
            .as_ref()
            .map(|exports| &exports.element.0)
        {
            // matches `EXPORTS ALL;` or `EXPORTS foo, bar, baz, ...`;
            Some(Some(exports)) => match &exports.element {
                AstExportsKind::All(_) => Exports::All,
                AstExportsKind::SymbolList(symbol_list) => Exports::SymbolList(
                    symbol_list_to_string_list(&symbol_list.element)
                        .into_iter()
                        .map(|symbol| symbol.element)
                        .collect(),
                ),
            },
            // matches `EXPORTS;`
            Some(None) => match parser.config.empty_export_behavior {
                EmptyExportBehavior::ExportAll => Exports::All,
                EmptyExportBehavior::ExportNone => Exports::SymbolList(Vec::new()),
            },
            // matches no EXPORTS statement at all
            None => Exports::All,
        };
        let mut imports_from_module = Vec::new();
        if let Some(ref ast_imports) = header.element.imports {
            for symbols_from_module in &ast_imports.element.0 {
                let imports =
                    symbol_list_to_string_list(&symbols_from_module.element.symbols.element);
                let module = match module_ref_to_module_ident(&symbols_from_module.element.module) {
                    Ok(module) => module,
                    Err(err) => return vec![Err(err)],
                };
                imports_from_module.push(ImportsFromModule { imports, module });
            }
        }

        let mut declarations = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            match &assignment.element {
                AstAssignment::TypeAssignment(assignment) => {
                    declarations.push(assignment.element.name.element.0.clone())
                }
                AstAssignment::ValueAssignment(assignment) => {
                    declarations.push(assignment.element.name.element.0.clone())
                }
                AstAssignment::ObjectSetAssignment(assignment) => {
                    declarations.push(assignment.element.name.element.0.clone())
                }
            }
        }

        vec![Ok(ModuleHeader {
            ident: parser.module.clone(),
            tag_default,
            extensibility_implied,
            exports,
            imports: imports_from_module,
            declarations,
        })]
    }) {
        Ok(modules) => {
            for module in modules {
                context.register_module(module);
            }
            Vec::new()
        }
        Err(errors) => errors,
    }
}

/// Stage 2: verify resolution of all imports.
pub fn resolve_all_imports(
    context: &mut Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, compiler, program, |parser| {
        let module = parser
            .context
            .lookup_module(&parser.module)
            .expect("lookup_module");

        let mut results: Vec<Result<()>> = Vec::new();
        for imports_from_module in &module.imports {
            for import in &imports_from_module.imports {
                match parser.resolve_symbol(&import.as_ref()) {
                    Ok(_) => (),
                    Err(err) => results.push(Err(err)),
                }
            }
        }

        results
    }) {
        Ok(_) => Vec::new(),
        Err(errors) => errors,
    }
}

/// Stage 3: register the names, but not the fields or syntax, of all declared information object classes.
///
/// This happens for two reasons:
///
/// 1. The class parser (register_all_information_object_classes) needs to know whether
///    a given typereference refers to a class or a type.
///    This is necessary in order to create the parser for the WITH SYNTAX definitions;
///    it must be known ahead of time whether to parse class(reference) tokens or type(reference) tokens.
///    In order to achieve this, we first find the qualified identifiers for every class in a first pass,
///    and then we pass those qualified identifiers to the second pass.
///
/// 2. The type parser (register_all_types) needs to know whether
///    a given typereference refers to a class of a type.
///    This is necessary in order to know whether a parameter passed to a parameterized type definition
///    is that of a class type or that of a normal type.
pub fn register_all_information_object_class_names(
    context: &mut Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, compiler, program, |parser| {
        let mut results = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            if let AstAssignment::TypeAssignment(type_assignment) = &assignment.element {
                match class::parse_information_object_class_assignment(
                    &parser,
                    type_assignment,
                    ObjectClassAssignmentParseMode::NameOnly,
                ) {
                    Ok(Some((ident, _))) => results.push(Ok(ident)),
                    Err(err) => results.push(Err(err)),
                    _ => (),
                }
            }
        }
        results
    }) {
        Ok(idents) => {
            for ident in idents {
                let class = InformationObjectClass {
                    name: AstElement::new(ident.name.clone(), Loc::default()),
                    fields: Vec::new(),
                    syntax: Vec::new(),
                    parsed: false,
                };
                context.register_information_object_class(
                    ident,
                    InformationObjectClassReference::Class(class),
                );
            }
            Vec::new()
        }
        Err(errors) => errors,
    }
}

// Stage 4: register all parameterized types.
// This stores their ASTs in the Context so that they can be parsed with the substituted types later.
pub fn register_all_parameterized_types(
    context: &mut Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, compiler, program, |parser| {
        let mut results = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            if let AstAssignment::TypeAssignment(ref type_assignment) = assignment.element {
                match types::parse_parameterized_type_assignment(&parser, type_assignment) {
                    Ok(Some(decl)) => results.push(Ok(decl)),
                    Err(err) => results.push(Err(err)),
                    _ => (),
                }
            }
        }
        results
    }) {
        Ok(types) => {
            for (ident, decl) in types {
                context.register_parameterized_type(ident, decl);
            }
            Vec::new()
        }
        Err(errors) => errors,
    }
}

/// Stage 5: register all declared types.
pub fn register_all_types(
    context: &mut Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, compiler, program, |parser| {
        let mut results = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            if let AstAssignment::TypeAssignment(type_assignment) = &assignment.element {
                match types::parse_type_assignment(
                    &parser,
                    type_assignment,
                    &TypeAssignmentParseMode::Normal,
                ) {
                    Ok(Some(decl)) => results.push(Ok(decl)),
                    Err(err) => results.push(Err(err)),
                    _ => (),
                }
            }
        }
        results
    }) {
        Ok(types) => {
            for (ident, decl) in types {
                context.register_type(ident, decl);
            }
            Vec::new()
        }
        Err(errors) => errors,
    }
}

/// Stage 6: register all declared information object classes.
pub fn register_all_information_object_classes(
    context: &mut Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    let class_idents = context
        .list_class_idents()
        .into_iter()
        .cloned()
        .collect::<Vec<_>>();

    // second pass
    match run_parser(context, compiler, program, |parser| {
        let mut results = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            if let AstAssignment::TypeAssignment(type_assignment) = &assignment.element {
                match class::parse_information_object_class_assignment(
                    &parser,
                    type_assignment,
                    ObjectClassAssignmentParseMode::Class {
                        class_idents: &class_idents,
                    },
                ) {
                    Ok(Some(decl)) => results.push(Ok(decl)),
                    Err(err) => results.push(Err(err)),
                    _ => (),
                }
            }
        }
        results
    }) {
        Ok(types) => {
            for (ident, decl) in types {
                context.register_information_object_class(
                    ident,
                    decl.expect("parse mode is Class, but parser returned name only"),
                );
            }
            Vec::new()
        }
        Err(errors) => errors,
    }
}

// Stage 7: register all declared information object sets.
pub fn register_all_information_object_sets(
    context: &mut Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, compiler, program, |parser| {
        let mut results = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            if let AstAssignment::ObjectSetAssignment(object_set_assignment) = &assignment.element {
                results.push(class::parse_object_set_assignment(
                    &parser,
                    object_set_assignment,
                ));
            }
        }
        results
    }) {
        Ok(types) => {
            for (ident, decl) in types {
                context.register_information_object_set(ident, decl);
            }
            Vec::new()
        }
        Err(errors) => errors,
    }
}

// Stage 8: register all declared information objects.
pub fn register_all_information_objects(
    context: &mut Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, compiler, program, |parser| {
        let mut results = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            if let AstAssignment::ValueAssignment(ref value_assignment) = assignment.element {
                match values::parse_value_assignment(
                    &parser,
                    value_assignment,
                    ParseValueAssignmentStage::ClassValues,
                ) {
                    Ok(Some(result)) => results.push(Ok(result)),
                    Ok(None) => (),
                    Err(err) => results.push(Err(err)),
                }
            }
        }
        results
    }) {
        Ok(values) => {
            for (ident, parsed) in values {
                match parsed {
                    ParsedValueAssignment::Value(_) => unreachable!(),
                    ParsedValueAssignment::Class(class) => {
                        context.register_information_object(ident, class)
                    }
                };
            }
            Vec::new()
        }
        Err(errors) => errors,
    }
}

/// Stage 9: register the constraints for all types.
pub fn register_all_constraints(
    context: &mut Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, compiler, program, |parser| {
        let mut results = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            if let AstAssignment::TypeAssignment(ref type_assignment) = assignment.element {
                match constraints::parse_type_assignment_constraint(&parser, type_assignment) {
                    Ok(Some(decl)) => results.push(Ok(decl)),
                    Err(err) => results.push(Err(err)),
                    _ => (),
                }
            }
        }
        results
    }) {
        Ok(pending_constraints) => {
            for (ident, pending) in pending_constraints {
                let decl = context.lookup_type_mut(&ident).expect("lookup_type");
                constraints::apply_pending_constraint(&mut decl.ty, pending);
            }
            Vec::new()
        }
        Err(errors) => errors,
    }
}

/// Stage 10: register all declared values that do not contain references
/// to information object class fields.
pub fn register_all_normal_values(
    context: &mut Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, compiler, program, |parser| {
        let mut results = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            if let AstAssignment::ValueAssignment(ref value_assignment) = assignment.element {
                match values::parse_value_assignment(
                    &parser,
                    value_assignment,
                    ParseValueAssignmentStage::NormalValues,
                ) {
                    Ok(Some(result)) => results.push(Ok(result)),
                    Ok(None) => (),
                    Err(err) => results.push(Err(err)),
                }
            }
        }
        results
    }) {
        Ok(values) => {
            for (ident, parsed) in values {
                match parsed {
                    ParsedValueAssignment::Value(value) => context.register_value(ident, value),
                    ParsedValueAssignment::Class(_) => unreachable!(),
                };
            }
            Vec::new()
        }
        Err(errors) => errors,
    }
}

/// Stage 11: register all values that contain references to information object class fields.
pub fn register_all_class_reference_values(
    context: &mut Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, compiler, program, |parser| {
        let mut results = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            if let AstAssignment::ValueAssignment(ref value_assignment) = assignment.element {
                match values::parse_value_assignment(
                    &parser,
                    value_assignment,
                    ParseValueAssignmentStage::ClassReferenceValues,
                ) {
                    Ok(Some(result)) => results.push(Ok(result)),
                    Ok(None) => (),
                    Err(err) => results.push(Err(err)),
                }
            }
        }
        results
    }) {
        Ok(values) => {
            for (ident, parsed) in values {
                // don't double-register parsed values
                if context.lookup_value(&ident).is_none() {
                    match parsed {
                        ParsedValueAssignment::Value(value) => context.register_value(ident, value),
                        ParsedValueAssignment::Class(_) => unreachable!(),
                    };
                }
            }
            Vec::new()
        }
        Err(errors) => errors,
    }
}

/// Stage 12: verify all types.
pub fn verify_all_types(
    context: &mut Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    // free up some memory by clearing the ASTs for parameterized types
    context.clear_parameterized_types();

    run_verifier(context, compiler, program, |verifier| {
        let mut errors: Vec<Error> = Vec::new();
        for (ident, declared_type) in verifier.context.list_types() {
            if ident.module != verifier.module {
                continue;
            }

            if let Err(err) = verify::verify_type(verifier.context, &declared_type.ty) {
                errors.push(err);
            }
        }
        errors
    })
}

/// Stage 13: verify all declared values.
pub fn verify_all_values(
    context: &Context,
    compiler: &Compiler,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    run_verifier(context, compiler, program, |verifier| {
        let mut errors: Vec<Error> = Vec::new();
        for (ident, declared_value) in verifier.context.list_values() {
            if ident.module != verifier.module {
                continue;
            }

            if let Err(err) = verify::verify_value(verifier.context, declared_value) {
                errors.push(err);
            }
        }
        errors
    })
}
