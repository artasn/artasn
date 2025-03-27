use std::{
    ffi::{c_char, CStr, CString},
    mem, ptr,
};

use artasn::compiler::ast::name_and_oid_to_module_ident;
use artasn::compiler::parser::*;

#[repr(C)]
pub struct GetModuleDependenciesResult {
    pub dependencies: *const Dependencies,
    pub error: *const c_char,
}

#[repr(C)]
pub struct Dependencies {
    pub module: Module,
    pub dependencies: *const Module,
    pub dependencies_len: u32,
}

#[repr(C)]
pub struct Module {
    pub name: *const c_char,
    pub oid: *const c_char,
}

#[no_mangle]
pub extern "C" fn get_module_dependencies(
    module_source_cstr: *const c_char,
) -> GetModuleDependenciesResult {
    let module_source = unsafe { CStr::from_ptr(module_source_cstr).to_str().unwrap() };

    // let mut tokens = TokenStream::new(module_source);
    // std::fs::write(
    //     "/tmp/mod.asn",
    //     tokens
    //         .as_tokens()
    //         .unwrap()
    //         .iter()
    //         .map(|token| token.to_string())
    //         .collect::<Vec<String>>()
    //         .join(" "),
    // )
    // .unwrap();
    let mut tokens = TokenStream::new(module_source);

    let context = ParseContext::new(&mut tokens);
    SOI::parse(ParseContext::new(context.tokens));
    match AstModuleHeader::parse(context) {
        ParseResult::Ok(header) => {
            let module = match get_module(&header.element.name, header.element.oid.as_ref().map(|oid| match &oid.element {
                AstModuleIdentifier::DefinitiveOid(oid) => oid,
                AstModuleIdentifier::DefinitiveOidWithIri(oid_with_iri) => &oid_with_iri.element.oid,
            })) {
                Ok(module) => module,
                Err(err) => return get_error_message(err, &module_source),
            };
            let mut dependencies = Vec::new();
            let deps_len;
            if let Some(imports) = &header.element.imports {
                deps_len = imports.element.0.len() as u32;
                for import in &imports.element.0 {
                    let module = &import.element.module.element;
                    let oid = match &module.oid {
                        Some(oid) => match &oid.element {
                            AstAssignedIdentifier::DefinitiveOid(oid) => Some(oid),
                            // TODO: implement import by valuereference for oid
                            AstAssignedIdentifier::ValueIdentifier(_) => None,
                        },
                        None => None,
                    };
                    dependencies.push(match get_module(&module.name, oid) {
                        Ok(module) => module,
                        Err(err) => return get_error_message(err, &module_source),
                    });
                }
            } else {
                deps_len = 0;
            }
            let deps_slice_box = dependencies.into_boxed_slice();
            let result = GetModuleDependenciesResult {
                dependencies: Box::into_raw(Box::new(Dependencies {
                    module,
                    dependencies: deps_slice_box.as_ptr(),
                    // dependencies_len: deps_slice_box.len() as u32,
                    dependencies_len: deps_len,
                })),
                error: ptr::null(),
            };
            mem::forget(deps_slice_box);
            result
        }
        ParseResult::Error(err) | ParseResult::Fail(err) => get_error_message(err, module_source),
    }
}

fn get_error_message(err: Error, module_source: &str) -> GetModuleDependenciesResult {
    GetModuleDependenciesResult {
        dependencies: ptr::null(),
        error: CString::new(err.get_message(module_source))
            .unwrap()
            .into_raw(),
    }
}

fn get_module(
    name: &AstElement<AstTypeReference>,
    oid: Option<&AstElement<AstDefinitiveOid>>,
) -> Result<Module> {
    let module_ident = name_and_oid_to_module_ident(name, oid)?;
    Ok(Module {
        oid: module_ident
            .oid
            .map(|oid| CString::new(oid.to_string()).unwrap().into_raw())
            .unwrap_or(ptr::null_mut()),
        name: CString::new(module_ident.name).unwrap().into_raw(),
    })
}
