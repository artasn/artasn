#![allow(static_mut_refs)]

pub mod ast;
mod context;
pub use context::{context, context_mut, Context};

#[path = "asn1.gen.rs"]
pub mod parser;

use parser::*;
use std::fmt::Display;

pub mod encode;

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
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Compiler {
        context::init_context();

        Compiler {
            sources: Vec::new(),
        }
    }

    pub fn add_source(&mut self, path: String, source: String) -> CompileResult<()> {
        let mut token_stream = TokenStream::new(&source);
        let parser = parser::AstProgram::parse(ParseContext::new(&mut token_stream));
        match parser {
            ParseResult::Ok(program) => {
                if token_stream.cursor() < source.len() {
                    Err(CompileError {
                        phase: CompilePhase::Parse,
                        error: Error {
                            loc: Loc::at(token_stream.cursor()),
                            kind: ErrorKind::ExpectingEOI,
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

    pub fn compile(&self) -> Vec<CompileError> {
        macro_rules! map_err {
            ( $source: expr ) => {
                |error| CompileError {
                    phase: CompilePhase::Walk,
                    path: $source.path.clone(),
                    source: $source.code.clone(),
                    error,
                }
            };
        }

        macro_rules! stage {
            ( $stage:ident ) => {{
                let mut errors = Vec::new();
                for source in &self.sources {
                    match ast::$stage(&source.program)
                        .map(|errors| {
                            errors
                                .into_iter()
                                .map(map_err!(source))
                                .collect::<Vec<CompileError>>()
                        })
                        .map_err(map_err!(source))
                    {
                        Ok(source_errors) => errors.extend(source_errors),
                        Err(err) => errors.push(err),
                    }
                }
                if errors.len() > 0 {
                    return errors;
                }
            }};
        }

        stage!(register_all_modules);
        stage!(register_all_types);
        stage!(register_all_values);
        stage!(verify_all_values);

        Vec::new()
    }

    pub fn get_context<'a>() -> &'a Context {
        context()
    }
}
