#![allow(static_mut_refs)]

mod ast;
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
        offset_to_line_col(&self.source, self.error.loc.offset)
    }

    pub fn len(&self) -> usize {
        self.error.loc.len
    }

    pub fn message(&self) -> String {
        get_parse_error_message(&self.error)
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
        let (line, col) = self.pos();
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

    pub fn compile(&self) -> CompileResult<()> {
        macro_rules! stage {
            ( $stage:ident ) => {{
                for source in &self.sources {
                    ast::$stage(&source.program).map_err(|error| CompileError {
                        phase: CompilePhase::Walk,
                        path: source.path.clone(),
                        source: source.code.clone(),
                        error,
                    })?;
                }
            }};
        }

        stage!(register_all_modules);
        stage!(register_all_types);
        stage!(register_all_values);

        Ok(())
    }

    pub fn get_context<'a>() -> &'a Context {
        context()
    }
}

pub fn offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
    let line = 1 + source[..offset].chars().filter(|c| *c == '\n').count();
    let col = offset - source[..offset].rfind('\n').unwrap_or(1);
    (line, col)
}

fn get_parse_error_message(error: &parser::Error) -> String {
    match &error.kind {
        ErrorKind::UnexpectedToken(token) => {
            format!("unexpected token '{}'", token)
        }
        ErrorKind::ExpectingEOI => String::from("expecting end of input"),
        ErrorKind::ExpectingOther {
            expecting,
            received,
        } => {
            let expecting_str = if expecting.len() == 1 {
                expecting[0].to_string()
            } else {
                let mut option_strs = expecting
                    .iter()
                    .map(|option| option.to_string())
                    .collect::<Vec<String>>();
                let last_idx = option_strs.len() - 1;
                let last = &option_strs[last_idx];
                option_strs[last_idx] = "or ".to_owned() + last;
                option_strs.join(", ")
            };
            if let Some(received_data) = &received.data {
                format!(
                    "expecting {}, received {}",
                    expecting_str,
                    received_data.to_string(&received.kind)
                )
            } else {
                format!(
                    "expecting {}, received {}",
                    expecting_str,
                    received.kind.to_string()
                )
            }
        }
        ErrorKind::IllegalStringCharacter {
            string_kind,
            character,
        } => format!(
            "illegal {} character '{}'",
            string_kind.to_string(),
            character
        ),
        ErrorKind::InvalidStringIndicator(indicator) => format!(
            "illegal string indicator suffix '{}' (expecting 'B', 'H', or a cstring)",
            indicator
        ),
        ErrorKind::ExpectingKeyword {
            expecting,
            received,
        } => format!(
            "expecting keyword {}, found keyword {}",
            expecting.name(),
            received.name()
        ),
        ErrorKind::ExpectingOperator {
            expecting,
            received,
        } => format!(
            "expecting operator '{}', found operator '{}'",
            expecting.name(),
            received.name()
        ),
        ErrorKind::MalformedIdentifier { ident } => {
            format!("malformed identifier '{}'", ident)
        }
        ErrorKind::MalformedNumber { number } => format!("malformed integer '{}'", number),
        ErrorKind::NumberTooLarge { number } => format!("number {} is too large", number),
        ErrorKind::UnterminatedString => "unterminated string".to_string(),
        ErrorKind::VariantUnmatched { variant } => {
            format!("unmatched variant '{}'", variant)
        }
        ErrorKind::Ast(message) => message.clone(),
    }
}
