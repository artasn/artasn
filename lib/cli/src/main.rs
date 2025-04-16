use std::{fmt::Display, fs, time::Instant};

use artasn::{
    compiler::{options::CompilerConfig, Compiler, Context},
    encoding::{DecodeMode, EncodeMode, TransferSyntax},
    module::QualifiedIdentifier,
    values::ValueResolve,
};
use clap::{Parser, ValueEnum};
use value_notation::get_asn1_value_str;

mod value_notation;

#[derive(Debug, Parser)]
#[command(name = "artasn")]
#[command(version = env!("CARGO_PKG_VERSION"))]
#[command(about = "CLI for ArtASN", long_about = None)]
struct Cli {
    /// Path to the config JSON file
    #[arg(long, short = 'c')]
    config: Option<String>,
    /// Prevents information like time elapsed from being printed.
    #[arg(long, short = 's')]
    silent: bool,
    /// Encode an ASN.1 value definition in the format "ModuleName.valueName"
    #[arg(long, group = "group_encode")]
    encode: Option<String>,
    /// Decode a hex-encoded ASN.1 value into ASN.1 value notation.
    #[arg(long, conflicts_with = "encode", group = "group_decode")]
    decode: Option<String>,
    /// When using '--decode', optionally provide the ASN.1 type of the value in the format "ModuleName.TypeName".
    /// If this argument is not included, the decoder will try each ASN.1 type defined across all provided modules until one of them matches the value.
    #[arg(long, conflicts_with = "encode", requires = "group_decode")]
    decode_type: Option<String>,
    /// The transfer syntax used for encoding or decoding  values.
    #[clap(long, short = 't', default_value_t = TransferSyntaxName::DER, requires = "group_encode", requires = "group_decode")]
    transfer_syntax: TransferSyntaxName,
    /// ASN.1 module files to compile
    #[arg(required = true, num_args = 1..)]
    files: Vec<String>,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum TransferSyntaxName {
    #[value(name = "BER")]
    BER,
    #[value(name = "CER")]
    CER,
    #[value(name = "DER")]
    DER,
    #[value(name = "PER")]
    PER,
    #[value(name = "UPER")]
    UPER,
    #[value(name = "CPER")]
    CPER,
    #[value(name = "CUPER")]
    CUPER,
    #[value(name = "XER")]
    XER,
    #[value(name = "CXER")]
    CXER,
    #[value(name = "E-XER")]
    EXER,
    #[value(name = "OER")]
    OER,
    #[value(name = "COER")]
    COER,
}

impl Display for TransferSyntaxName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::BER => "BER",
            Self::CER => "CER",
            Self::DER => "DER",
            Self::PER => "PER",
            Self::UPER => "UPER",
            Self::CPER => "CPER",
            Self::CUPER => "CUPER",
            Self::XER => "XER",
            Self::CXER => "CXER",
            Self::EXER => "E-XER",
            Self::OER => "OER",
            Self::COER => "COER",
        })
    }
}

fn exit_with_error(args: std::fmt::Arguments) -> ! {
    eprintln!("{}", args);
    std::process::exit(1);
}

fn elapsed_to_string(start: &Instant) -> String {
    let elapsed = start.elapsed();
    let ms = elapsed.as_millis();
    let us = elapsed.as_micros();
    if ms == 0 {
        format!("{}us", us)
    } else {
        let s = elapsed.as_secs();
        if s == 0 {
            format!("{}.{:03}ms", ms, us % 1000)
        } else {
            format!("{}.{:03}s", s, ms % 1000)
        }
    }
}

fn main() {
    let args = Cli::parse();

    let config = match &args.config {
        Some(path) => match fs::read_to_string(path) {
            Ok(json) => match CompilerConfig::from_json(&json) {
                Ok(config) => config,
                Err(err) => exit_with_error(format_args!("failed to parse config JSON: {}", err)),
            },
            Err(err) => exit_with_error(format_args!(
                "failed to read config JSON file at '{}': {}",
                path, err
            )),
        },
        None => CompilerConfig::default(),
    };

    let mut compiler = Compiler::new(config);
    match compiler.add_stdlib() {
        Ok(()) => (),
        Err(err) => exit_with_error(format_args!("failed to parse stdlib: {}", err)),
    }

    let start = Instant::now();
    let mut parse_errors = Vec::new();
    for file_path in &args.files {
        let asn_source = match fs::read_to_string(file_path) {
            Ok(asn_source) => asn_source,
            Err(err) => exit_with_error(format_args!(
                "failed to read ASN.1 module file at '{}': {}",
                file_path, err
            )),
        };
        match compiler.add_source(file_path.clone(), asn_source) {
            Ok(_) => (),
            Err(err) => parse_errors.push(err),
        };
    }

    if !parse_errors.is_empty() {
        for parse_error in parse_errors {
            eprintln!("{parse_error}");
        }
        std::process::exit(1);
    }

    let mut context = Context::new();
    let errors = compiler.compile(&mut context);
    if !errors.is_empty() {
        for error in errors {
            eprintln!("{error}");
        }
        std::process::exit(1);
    }

    if !args.silent {
        println!("compiled in {}", elapsed_to_string(&start));
    }
    let start = Instant::now();

    if let Some(value) = args.encode {
        let transfer_syntax = args.transfer_syntax;
        let ts = TransferSyntax::get_by_name(transfer_syntax.to_string().as_str())
            .expect("invalid transfer syntax (this should be prevented by clap)");
        let encoder = match ts.get_codec().encoder {
            Some(encoder) => encoder,
            None => exit_with_error(format_args!(
                "encoding with the {} transfer syntax is not yet implemented",
                transfer_syntax
            )),
        };

        let value_ident = parse_qualified_identifier(&context, IdentKind::Value, &value);
        let declared_value = match context.lookup_value(&value_ident) {
            Some(value) => value,
            None => exit_with_error(format_args!(
                "value '{}' could not be found in module '{}'",
                value_ident.name, value_ident.module
            )),
        };

        let value = match declared_value.value.resolve(&context) {
            Ok(value) => value,
            Err(err) => exit_with_error(format_args!(
                "failed to resolve value: {}",
                err.kind.message()
            )),
        };

        let mut buf = Vec::with_capacity(64 * 1024);
        match encoder(ts, EncodeMode::Normal, &mut buf, &context, &value) {
            Ok(()) => (),
            Err(err) => exit_with_error(format_args!(
                "failed to encode value: {}",
                err.kind.message()
            )),
        }
        let hex = hex::encode_upper(buf);
        if !args.silent {
            println!("encoded in {}\n", elapsed_to_string(&start));
        }
        println!("{}", hex);
    } else if let Some(hex) = args.decode {
        let binary = match hex::decode(hex) {
            Ok(binary) => binary,
            Err(err) => exit_with_error(format_args!("failed to decode hex: {}", err)),
        };

        let transfer_syntax = args.transfer_syntax;
        let ts = TransferSyntax::get_by_name(transfer_syntax.to_string().as_str())
            .expect("invalid transfer syntax (this should be prevented by clap)");
        let decoder = match ts.get_codec().decoder {
            Some(decoder) => decoder,
            None => exit_with_error(format_args!(
                "decoding with the {} transfer syntax is not yet implemented",
                transfer_syntax
            )),
        };

        let mode = match args.decode_type.as_ref() {
            Some(decode_type) => {
                let type_ident = parse_qualified_identifier(&context, IdentKind::Type, decode_type);
                let declared_type = match context.lookup_type(&type_ident) {
                    Some(value) => value,
                    None => exit_with_error(format_args!(
                        "type '{}' could not be found in module '{}'",
                        type_ident.name, type_ident.module
                    )),
                };

                let resolved = match declared_type.ty.resolve(&context) {
                    Ok(value) => value,
                    Err(err) => exit_with_error(format_args!(
                        "failed to resolve type: {}",
                        err.kind.message()
                    )),
                };

                DecodeMode::SpecificType {
                    source_ident: Some(type_ident),
                    component_name: None,
                    resolved,
                }
            }
            None => todo!("--decode without --decode-type"),
        };

        let decoded_values = match decoder(ts, &mode, &binary, &context) {
            Ok(decoded_values) => decoded_values,
            Err(err) => exit_with_error(format_args!("failed to decode value: {}", err)),
        };
        for decoded_value in decoded_values {
            let str = get_asn1_value_str(&context, &decoded_value);
            println!("{str}");
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum IdentKind {
    Type,
    Value,
}

fn parse_qualified_identifier(
    context: &Context,
    kind: IdentKind,
    str_name: &str,
) -> QualifiedIdentifier {
    let dot_count = str_name.chars().filter(|ch| *ch == '.').count();
    if dot_count == 0 {
        match kind {
            IdentKind::Type => {
                exit_with_error(format_args!(
                    "type '{}' is missing module name; use the format 'ModuleName.{}'",
                    str_name, str_name
                ));
            }
            IdentKind::Value => {
                exit_with_error(format_args!(
                    "value '{}' is missing module name; use the format 'ModuleName.{}'",
                    str_name, str_name
                ));
            }
        }
    } else if dot_count > 1 {
        match kind {
            IdentKind::Type => exit_with_error(format_args!(
                "type '{}' is malformed; use the format 'ModuleName.TypeName'",
                str_name
            )),
            IdentKind::Value => exit_with_error(format_args!(
                "value '{}' is malformed; use the format 'ModuleName.valueName'",
                str_name
            )),
        }
    }

    let split = str_name.split(".").collect::<Vec<&str>>();
    let module_name = split[0].trim();
    let value_name = split[1].trim();

    if module_name.is_empty() {
        exit_with_error(format_args!("module name cannot be empty"));
    }
    if value_name.is_empty() {
        exit_with_error(format_args!("value name cannot be empty"));
    }

    let module = match context.lookup_module_by_name(module_name) {
        Some(module) => module.ident.clone(),
        None => exit_with_error(format_args!("module '{}' could not be found", module_name)),
    };

    QualifiedIdentifier::new(module, value_name.to_string())
}
