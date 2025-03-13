use std::{fmt::Display, fs, time::Instant};

use asn1chef::{
    compiler::{options::CompilerConfig, Compiler, Context},
    encoding::TransferSyntax,
    module::QualifiedIdentifier,
    values::ValueResolve,
};
use clap::{Parser, Subcommand, ValueEnum};

#[derive(Debug, Parser)]
#[command(name = "asn1chef")]
#[command(version = env!("CARGO_PKG_VERSION"))]
#[command(about = "CLI for ASN.1Chef", long_about = None)]
struct Cli {
    /// Path to the config JSON file
    #[arg(long, short = 'c')]
    config: Option<String>,
    /// ASN.1 module files to compile
    #[arg(long, short = 'f', required = true)]
    files: Vec<String>,
    /// Prevents information like time elapsed from being printed.
    #[arg(long, short = 's')]
    silent: bool,
    #[clap(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Only print compiler errors and warnings
    Validate,
    /// Encode an ASN.1 value definition
    Encode {
        /// The name of the value, in the format "ModuleName.valueName"
        #[clap(long, short = 'v')]
        value: String,

        /// The transfer syntax to encode the value into
        #[clap(long, short = 't', default_value_t = TransferSyntaxName::DER)]
        transfer_syntax: TransferSyntaxName,
    },
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
    #[value(name = "E-CER")]
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
    if ms == 0 {
        format!("{}us", elapsed.as_micros())
    } else {
        let s = elapsed.as_secs();
        if s == 0 {
            format!("{}ms", ms)
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

    match args.command {
        Command::Validate => (),
        Command::Encode {
            value,
            transfer_syntax,
        } => {
            let ts = TransferSyntax::get_by_name(transfer_syntax.to_string().as_str())
                .expect("invalid transfer syntax (this should be prevented by clap)");
            let encoder = match ts.get_codec().encoder {
                Some(encoder) => encoder,
                None => exit_with_error(format_args!(
                    "encoding with the {} transfer syntax is not yet implemented",
                    transfer_syntax
                )),
            };

            let dot_count = value.chars().filter(|ch| *ch == '.').count();
            if dot_count == 0 {
                exit_with_error(format_args!(
                    "value '{}' is missing module name; use the format 'ModuleName.{}'",
                    value, value
                ));
            } else if dot_count > 1 {
                exit_with_error(format_args!(
                    "value '{}' is malformed; use the format 'ModuleName.valueName'",
                    value
                ))
            }

            let split = value.split(".").collect::<Vec<&str>>();
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
                None => {
                    exit_with_error(format_args!("module '{}' could not be found", module_name))
                }
            };
            let declared_value = match context
                .lookup_value(&QualifiedIdentifier::new(module, value_name.to_string()))
            {
                Some(value) => value,
                None => exit_with_error(format_args!(
                    "value '{}' could not be found in module '{}'",
                    value_name, module_name
                )),
            };

            let resolved_type = match declared_value.ty.resolve(&context) {
                Ok(resolved_type) => resolved_type,
                Err(err) => exit_with_error(format_args!(
                    "failed to resolve value's type: {}",
                    err.kind.message()
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
            match encoder(ts, &mut buf, &context, value, &resolved_type) {
                Ok(()) => (),
                Err(err) => exit_with_error(format_args!(
                    "failed to encode value: {}",
                    err.kind.message()
                )),
            }
            let hex = hex::encode_upper(buf.into_iter().rev().collect::<Vec<u8>>());
            if !args.silent {
                println!("encoded in {}\n", elapsed_to_string(&start));
            }
            println!("{}", hex);
        }
    }
}
