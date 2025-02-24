use core::panic;
use quote::ToTokens;
use std::{
    fs::{self, File},
    io::{self, Write},
};
mod code_builder;
mod parser;
mod syntax;
mod tokenizer;

pub fn generate_parser(syntax_path: &str, syntax_defs: &str, output_path: &str) {
    match fs::remove_file(output_path) {
        Ok(_) => (),
        Err(err) => match err.kind() {
            io::ErrorKind::NotFound => (), // ignore if the file didn't exist
            _ => panic!("error deleting output file: {}", err),
        },
    }

    let rust_code = match syntax::parse_syntax(syntax_path, syntax_defs) {
        Ok(rust_code) => rust_code,
        Err(err) => panic!("{}", err),
    };

    // we include "common.rs" ()
    let gen_files: &[&str] = &[include_str!("./parser.rs"), include_str!("./tokenizer.rs")];
    let gen_files = gen_files
        .iter()
        .map(|gen_file| syn::parse_file(gen_file).unwrap());

    let mut output_file = File::create(output_path).expect("failed to open output file");
    let mut composite_module = syn::File {
        shebang: None,
        attrs: Vec::new(),
        items: Vec::new(),
    };
    // usages required for generated code, but stripped in strip_module
    composite_module
        .items
        .push(syn::Item::Use(syn::parse_quote!(
            use std::fmt::{Display, Write};
        )));
    for mut gen_file in gen_files {
        // all module-level attributes are combined
        composite_module.attrs.append(&mut gen_file.attrs);

        // in the source code, these modules refer to each other wth `use::` declarations
        // since we're combining them into module, we need to remove all instances of `use::`
        // also remove `mod` definitions, so that parsegen tests aren't included
        composite_module.items.extend(
            gen_file
                .items
                .into_iter()
                .filter(|item| !matches!(item, syn::Item::Use(_) | syn::Item::Mod(_))),
        );
    }
    output_file
        .write_all(composite_module.to_token_stream().to_string().as_bytes())
        .unwrap();
    output_file.write_all(rust_code.as_bytes()).unwrap();
}
