use std::{fmt::Display, fs, time::Instant};

use asn1chef::{
    compiler::{options::CompilerConfig, Compiler, Context},
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
        #[clap(long, short = 't', default_value_t = TransferSyntax::Der)]
        transfer_syntax: TransferSyntax,
    },
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum TransferSyntax {
    #[value(name = "der")]
    Der,
}

impl Display for TransferSyntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Der => "der",
        })
    }
}

fn exit_with_error(args: std::fmt::Arguments) -> ! {
    eprintln!("{}", args);
    std::process::exit(1);
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

    let elapsed = {
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
    };
    println!("finished in {}", elapsed);

    match args.command {
        Command::Validate => (),
        Command::Encode { value, .. } => {
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
            let declared_value = match context.lookup_value(&QualifiedIdentifier {
                module,
                name: value_name.to_string(),
            }) {
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
            match value.der_encode(&mut buf, &context, &resolved_type) {
                Ok(()) => (),
                Err(err) => exit_with_error(format_args!(
                    "failed to encode value: {}",
                    err.kind.message()
                )),
            }
            let hex = hex::encode_upper(buf.into_iter().rev().collect::<Vec<u8>>());
            println!("{}", hex);
        }
    }
}
