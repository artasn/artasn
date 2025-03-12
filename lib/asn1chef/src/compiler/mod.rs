#![allow(static_mut_refs)]

pub mod ast;
mod context;
pub use context::Context;

#[path = "asn1.gen.rs"]
pub mod parser;

use options::CompilerConfig;
use parser::*;
use std::fmt::Display;

pub mod oid_tree;

pub mod options;

#[cfg(test)]
pub mod test;

struct SourceFile {
    path: String,
    code: String,
    program: AstElement<AstProgram>,
}

pub type CompileResult<T> = std::result::Result<T, CompileError>;

#[derive(Debug)]
pub enum CompilePhase {
    Parse,
    Walk,
}

impl CompilePhase {
    pub fn get_error_prefix(&self) -> &'static str {
        match self {
            Self::Parse => "parse error",
            Self::Walk => "compile error",
        }
    }
}

#[derive(Debug)]
pub struct CompileError {
    pub phase: CompilePhase,
    pub error: Error,
    pub path: String,
    source: String,
}

impl CompileError {
    pub fn pos(&self) -> (usize, usize) {
        self.error.pos(&self.source)
    }

    pub fn len(&self) -> usize {
        self.error.loc.len
    }

    pub fn message(&self) -> String {
        self.error.kind.message()
    }
}

impl std::error::Error for CompileError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn description(&self) -> &str {
        "description() is deprecated; use Display"
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (line, col) = self.error.pos(&self.source);
        f.write_fmt(format_args!(
            "{}: {} at {}:{}:{}",
            self.phase.get_error_prefix(),
            self.message(),
            self.path,
            line,
            col
        ))
    }
}

pub struct Compiler {
    sources: Vec<SourceFile>,
    pub config: CompilerConfig,
}

impl Compiler {
    pub fn new(config: CompilerConfig) -> Compiler {
        Compiler {
            sources: Vec::new(),
            config,
        }
    }

    pub fn add_stdlib(&mut self) -> CompileResult<()> {
        self.add_source(
            String::from("EmbeddedPDV.asn"),
            include_str!("../../stdlib/EmbeddedPDV.asn").to_string(),
        )?;
        self.add_source(
            String::from("CharacterString.asn"),
            include_str!("../../stdlib/CharacterString.asn").to_string(),
        )?;

        Ok(())
    }

    pub fn add_source(&mut self, path: String, source: String) -> CompileResult<()> {
        let mut token_stream =
            TokenStream::new(&source, self.config.permit_lowercase_string_indicator);
        let parser = parser::AstProgram::parse(ParseContext::new(&mut token_stream));
        match parser {
            ParseResult::Ok(program) => {
                if token_stream.cursor() < source.len() {
                    Err(CompileError {
                        phase: CompilePhase::Parse,
                        error: Error {
                            loc: Loc::at(token_stream.cursor()),
                            kind: ErrorKind::ExpectingEoi,
                        },
                        path,
                        source,
                    })
                } else {
                    if let Some(existing_source) =
                        self.sources.iter_mut().find(|source| source.path == path)
                    {
                        existing_source.code = source;
                        existing_source.program = program;
                    } else {
                        self.sources.push(SourceFile {
                            path,
                            code: source,
                            program,
                        });
                    }

                    Ok(())
                }
            }
            ParseResult::Fail(error) | ParseResult::Error(error) => Err(CompileError {
                phase: CompilePhase::Parse,
                error,
                path,
                source,
            }),
        }
    }

    pub fn remove_source(&mut self, path: &str) -> bool {
        if let Some((index, _)) = self
            .sources
            .iter()
            .enumerate()
            .find(|(_, source)| source.path == path)
        {
            self.sources.remove(index);
            true
        } else {
            false
        }
    }

    pub fn compile(&self, context: &mut Context) -> Vec<CompileError> {
        macro_rules! stage {
            ( $stage:ident ) => {{
                let mut errors = Vec::new();
                for source in &self.sources {
                    let source_errors = ast::$stage(context, &self.config, &source.program)
                        .into_iter()
                        .map(|error| CompileError {
                            phase: CompilePhase::Walk,
                            path: source.path.clone(),
                            source: source.code.clone(),
                            error,
                        });
                    errors.extend(source_errors);
                }
                if errors.len() > 0 {
                    return errors;
                }
            }};
        }

        stage!(register_all_modules);
        stage!(register_all_types);
        stage!(register_all_constraints);
        stage!(register_all_values);
        stage!(verify_all_types);
        stage!(verify_all_values);

        Vec::new()
    }
}
