use core::panic;
use quote::ToTokens;
use std::{
    collections::HashMap,
    fs::{self, File},
    io::{self, Write},
};
use syn::*;
mod code_builder;
mod parser;
mod syntax;
mod tokenizer;

fn parse_str_enum(item_enum: syn::ItemEnum) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for variant in item_enum.variants {
        let (_, discriminant) = variant.discriminant.unwrap();
        match discriminant {
            Expr::Lit(lit) => match lit.lit {
                Lit::Str(str) => {
                    map.insert(str.value(), variant.ident.to_string());
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    map
}

pub fn generate_parser(syntax_path: &str, syntax_defs: &str, output_path: &str, extra: syn::File) {
    match fs::remove_file(output_path) {
        Ok(_) => (),
        Err(err) => match err.kind() {
            io::ErrorKind::NotFound => (), // ignore if the file didn't exist
            _ => panic!("error deleting output file: {}", err),
        },
    }

    let (keywords, operators) = {
        let mut keywords = None;
        let mut operators = None;
        for item in &extra.items {
            if let Item::Macro(item_macro) = item {
                if *item_macro
                    .mac
                    .path
                    .get_ident()
                    .as_ref()
                    .unwrap()
                    == "enum_str"
                {
                    let macro_body_tokens = &item_macro.mac.tokens;
                    let item_enum: ItemEnum = parse2(macro_body_tokens.clone()).unwrap();
                    let enum_name = item_enum.ident.to_string();
                    let str_enum = parse_str_enum(item_enum);
                    if enum_name == "Keyword" {
                        keywords = Some(str_enum);
                    } else if enum_name == "Operator" {
                        operators = Some(str_enum);
                    } else {
                        panic!();
                    }
                } else {
                    panic!();
                }
            }
        }
        (keywords.unwrap(), operators.unwrap())
    };

    let rust_code = match syntax::parse_syntax(syntax_path, syntax_defs, &keywords, &operators) {
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
    composite_module.items.push(Item::Use(syn::parse_quote!(
        use std::fmt::{Display, Write};
    )));
    for mut gen_file in gen_files {
        // all module-level attributes are combined
        composite_module.attrs.append(&mut gen_file.attrs);

        // in the source code, these modules refer to each other wth `use::` declarations
        // since we're combining them into module, we need to remove all instances of `use::`
        // also remove `mod` definitions, so that parsegen tests aren't included
        let mut filtered_items = gen_file
            .items
            .into_iter()
            .filter(|item| {
                !matches!(item, Item::Use(_) | Item::Mod(_))
                    && match item {
                        Item::Macro(item_macro) => {
                            match item_macro
                                .mac
                                .path
                                .get_ident()
                                .as_ref()
                                .map(|ident| ident.to_string())
                            {
                                Some(ident) => ident != "enum_str",
                                None => true,
                            }
                        }
                        _ => true,
                    }
            })
            .collect::<Vec<_>>();

        for i in 0..filtered_items.len() {
            let item = &filtered_items[i];
            if let Item::Const(item_const) = item {
                if item_const.ident == "EXTRA_PLACEHOLDER" {
                    filtered_items.remove(i);
                    for extra_item in extra.items.iter().rev().cloned() {
                        filtered_items.insert(i, extra_item);
                    }
                }
            }
        }

        composite_module.items.extend(filtered_items);
    }
    output_file
        .write_all(composite_module.to_token_stream().to_string().as_bytes())
        .unwrap();
    output_file.write_all(rust_code.as_bytes()).unwrap();
}
