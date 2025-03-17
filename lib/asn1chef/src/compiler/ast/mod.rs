mod object_id;
pub(crate) mod types;
pub(crate) mod constraints;
mod values;
mod verify;

use types::TypeAssignmentParseMode;

use super::options::CompilerConfig;
use super::{parser::*, Context};
use crate::compiler::options::EmptyExportBehavior;
use crate::module::ModuleIdentifier;
use crate::module::*;

pub struct AstParser<'a> {
    pub context: &'a Context,
    pub config: &'a CompilerConfig,
    pub ast_module: &'a AstElement<AstModule>,
    pub module: ModuleIdentifier,
}

pub struct AstVerifier<'a> {
    pub context: &'a Context,
    pub config: &'a CompilerConfig,
    pub module: ModuleIdentifier,
}

fn run_parser<T, F: Fn(AstParser<'_>) -> Vec<Result<T>>>(
    context: &Context,
    config: &CompilerConfig,
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
            config,
            ast_module,
            module,
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
    config: &CompilerConfig,
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
            config,
            module,
        };

        errors.extend(f(verifier));
    }

    errors
}

fn module_ast_to_module_ident(header: &AstElement<AstModuleHeader>) -> Result<ModuleIdentifier> {
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
) -> Result<ModuleIdentifier> {
    let oid = match &module_ref.element.oid {
        Some(oid) => match &oid.element {
            AstAssignedIdentifier::DefinitiveOid(oid) => Some(oid),
            // TODO: implement import by valuereference for oid
            AstAssignedIdentifier::ValueIdentifier(_) => None,
        },
        None => None,
    };
    object_id::name_and_oid_to_module_ident(&module_ref.element.name, oid)
}

fn symbol_list_to_string_list(symbol_list: &AstSymbolList) -> Vec<String> {
    symbol_list
        .0
        .iter()
        .map(|symbol| {
            match match &symbol.element {
                AstSymbol::ParameterizedReference(parameterized) => {
                    parameterized.element.0.element.clone()
                }
                AstSymbol::Reference(reference) => reference.element.clone(),
            } {
                AstReference::TypeReference(type_ref) => type_ref.element.0,
                AstReference::ValueReference(val_ref) => val_ref.element.0,
            }
        })
        .collect()
}

/// Stage 1: register all module headers.
pub fn register_all_modules(
    context: &mut Context,
    config: &CompilerConfig,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, config, program, |parser| {
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
        let extensibility_implied = header.element.extension_default.is_some();
        let exports = match header
            .element
            .exports
            .as_ref()
            .map(|exports| &exports.element.0)
        {
            // matches `EXPORTS ALL;` or `EXPORTS foo, bar, baz, ...`;
            Some(Some(exports)) => match &exports.element {
                AstExportsKind::All(_) => Exports::All,
                AstExportsKind::SymbolList(symbol_list) => {
                    Exports::SymbolList(symbol_list_to_string_list(&symbol_list.element))
                }
            },
            // matches `EXPORTS;`
            Some(None) => match parser.config.empty_export_behavior {
                EmptyExportBehavior::ExportAll => Exports::All,
                EmptyExportBehavior::ExportNone => Exports::SymbolList(Vec::new()),
            },
            // matches no EXPORTS statement at all
            None => Exports::All,
        };
        let mut imports = Vec::new();
        if let Some(ref ast_imports) = header.element.imports {
            for symbols_from_module in &ast_imports.element.0 {
                let symbols =
                    symbol_list_to_string_list(&symbols_from_module.element.symbols.element);
                let module = match module_ref_to_module_ident(&symbols_from_module.element.module) {
                    Ok(module) => module,
                    Err(err) => return vec![Err(err)],
                };
                for name in symbols {
                    imports.push(QualifiedIdentifier::new(module.clone(), name));
                }
            }
        }

        vec![Ok(ModuleHeader {
            ident: parser.module.clone(),
            tag_default,
            extensibility_implied,
            exports,
            imports,
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

// Stage 2: register all parameterized types.
// This stores their ASTs in the Context so that they can be parsed with the substituted types later.
pub fn register_all_parameterized_types(
    context: &mut Context,
    config: &CompilerConfig,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, config, program, |parser| {
        let mut results = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            if let AstAssignment::TypeAssignment(ref type_assignment) = assignment.element {
                match types::parse_parameterized_type_assignment(&parser, type_assignment) {
                    Ok(Some(decl)) => results.push(Ok(decl)),
                    Err(decl) => results.push(Err(decl)),
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

/// Stage 3: register all declared types.
pub fn register_all_types(
    context: &mut Context,
    config: &CompilerConfig,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, config, program, |parser| {
        let mut results = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            if let AstAssignment::TypeAssignment(ref type_assignment) = assignment.element {
                match types::parse_type_assignment(
                    &parser,
                    type_assignment,
                    &TypeAssignmentParseMode::Normal,
                ) {
                    Ok(Some(decl)) => results.push(Ok(decl)),
                    Err(decl) => results.push(Err(decl)),
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

/// Stage 4: register the constraints for all types.
pub fn register_all_constraints(
    context: &mut Context,
    config: &CompilerConfig,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, config, program, |parser| {
        let mut results = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            if let AstAssignment::TypeAssignment(ref type_assignment) = assignment.element {
                match constraints::parse_type_assignment_constraint(
                    &parser,
                    type_assignment,
                ) {
                    Ok(Some(decl)) => results.push(Ok(decl)),
                    Err(decl) => results.push(Err(decl)),
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

/// Stage 5: register all declared values.
pub fn register_all_values(
    context: &mut Context,
    config: &CompilerConfig,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    match run_parser(context, config, program, |parser| {
        let mut results = Vec::new();
        for assignment in &parser.ast_module.element.body.element.0 {
            if let AstAssignment::ValueAssignment(ref value_assignment) = assignment.element {
                results.push(values::parse_value_assignment(&parser, value_assignment));
            }
        }
        results
    }) {
        Ok(values) => {
            for (ident, val) in values {
                context.register_value(ident, val);
            }
            Vec::new()
        }
        Err(errors) => errors,
    }
}

/// Stage 6: verify all types.
pub fn verify_all_types(
    context: &mut Context,
    config: &CompilerConfig,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    // free up some memory by clearing the ASTs for parameterized types
    context.clear_parameterized_types();

    run_verifier(context, config, program, |verifier| {
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

/// Stage 7: verify all declared values.
pub fn verify_all_values(
    context: &Context,
    config: &CompilerConfig,
    program: &AstElement<AstProgram>,
) -> Vec<Error> {
    run_verifier(context, config, program, |verifier| {
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
