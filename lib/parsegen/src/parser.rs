#![allow(dead_code)]
#![allow(unused_macros)]

use super::tokenizer::*;

pub struct TokenStream {
    tokenizer: Tokenizer,
}

impl TokenStream {
    pub fn new(source: &str) -> TokenStream {
        TokenStream {
            tokenizer: Tokenizer::new(source),
        }
    }

    pub fn try_parse(&mut self, kind: TokenKind) -> Result<Token> {
        let token = self.tokenizer.tokenize_next()?;
        if token.kind == kind {
            Ok(token)
        } else {
            Err(Error {
                kind: ErrorKind::ExpectingOther {
                    expecting: vec![kind],
                    found: token.clone(),
                },
                loc: token.loc,
            })
        }
    }

    pub fn set_cursor(&mut self, cursor: usize) {
        self.tokenizer.set_cursor(cursor);
    }

    pub fn cursor(&self) -> usize {
        self.tokenizer.cursor()
    }

    pub fn as_tokens(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        loop {
            let token = self.tokenizer.tokenize_next()?;
            if token.kind == TokenKind::EOI {
                tokens.push(token);
                return Ok(tokens);
            }
            tokens.push(token);
        }
    }
}

#[derive(Debug, Clone)]
pub struct SOI;

impl Parseable for SOI {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::SOI)
            .map(|token| {
                ParseResult::Ok(AstElement {
                    element: SOI,
                    loc: token.loc,
                })
            })
            .unwrap_or_else(ParseResult::Fail)
    }
}

#[derive(Debug, Clone)]
pub struct EOI;

impl Parseable for EOI {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::EOI)
            .map(|token| {
                ParseResult::Ok(AstElement {
                    element: EOI,
                    loc: token.loc,
                })
            })
            .unwrap_or_else(ParseResult::Fail)
    }
}

#[derive(Debug, Clone)]
pub struct AstTypeReference(pub String);

impl Parseable for AstTypeReference {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::TypeReference)
            .map(|token| {
                ParseResult::Ok(AstElement {
                    element: AstTypeReference(match token.data.unwrap() {
                        TokenData::Named(name) => name,
                        _ => unreachable!(),
                    }),
                    loc: token.loc,
                })
            })
            .unwrap_or_else(ParseResult::Fail)
    }
}

#[derive(Debug, Clone)]
pub struct AstEncodingReference(pub String);

impl Parseable for AstEncodingReference {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::TypeReference)
            .map(|token| {
                let name = match token.data.as_ref().unwrap() {
                    TokenData::Named(name) => name,
                    _ => unreachable!(),
                };
                for ch in name.chars() {
                    if !ch.is_ascii_uppercase() {
                        return ParseResult::Fail(Error {
                            loc: token.loc,
                            kind: ErrorKind::ExpectingOther {
                                expecting: vec![TokenKind::EncodingReference],
                                found: token,
                            },
                        });
                    }
                }
                ParseResult::Ok(AstElement {
                    element: AstEncodingReference(match token.data.unwrap() {
                        TokenData::Named(name) => name,
                        _ => unreachable!(),
                    }),
                    loc: token.loc,
                })
            })
            .unwrap_or_else(ParseResult::Fail)
    }
}

#[derive(Debug, Clone)]
pub struct AstValueReference(pub String);

impl Parseable for AstValueReference {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::ValueReference)
            .map(|token| {
                ParseResult::Ok(AstElement {
                    element: AstValueReference(match token.data.unwrap() {
                        TokenData::Named(name) => name,
                        _ => unreachable!(),
                    }),
                    loc: token.loc,
                })
            })
            .unwrap_or_else(ParseResult::Fail)
    }
}

#[derive(Debug, Clone)]
pub struct AstStringLiteral {
    pub kind: StringKind,
    pub data: std::string::String,
}

impl Parseable for AstStringLiteral {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::String)
            .map(|token| {
                ParseResult::Ok(AstElement {
                    element: match token.data.unwrap() {
                        TokenData::String(kind, data) => AstStringLiteral { kind, data },
                        _ => unreachable!(),
                    },
                    loc: token.loc,
                })
            })
            .unwrap_or_else(ParseResult::Fail)
    }
}

#[derive(Debug, Clone)]
pub struct AstNumber(pub u64);

impl Parseable for AstNumber {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::Number)
            .map(|token| {
                ParseResult::Ok(AstElement {
                    element: AstNumber(match token.data.unwrap() {
                        TokenData::Number(num) => num,
                        _ => unreachable!(),
                    }),
                    loc: token.loc,
                })
            })
            .unwrap_or_else(ParseResult::Fail)
    }
}

impl Keyword {
    pub fn match_keyword(keyword: Keyword, context: ParseContext) -> ParseResult<Keyword> {
        let parse = Self::parse(context);
        match parse {
            ParseResult::Ok(ast) => {
                if ast.element == keyword {
                    ParseResult::Ok(ast)
                } else {
                    ParseResult::Fail(Error {
                        kind: ErrorKind::ExpectingKeyword {
                            expecting: keyword,
                            found: ast.element,
                        },
                        loc: ast.loc,
                    })
                }
            }
            _ => parse,
        }
    }
}

impl Parseable for Keyword {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::Keyword)
            .map(|token| {
                ParseResult::Ok(AstElement {
                    element: match token.data.unwrap() {
                        TokenData::Keyword(num) => num,
                        _ => unreachable!(),
                    },
                    loc: token.loc,
                })
            })
            .unwrap_or_else(ParseResult::Fail)
    }
}

impl Parseable for Operator {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::Operator)
            .map(|token| {
                ParseResult::Ok(AstElement {
                    element: match token.data.unwrap() {
                        TokenData::Operator(num) => num,
                        _ => unreachable!(),
                    },
                    loc: token.loc,
                })
            })
            .unwrap_or_else(ParseResult::Fail)
    }
}

impl Operator {
    pub fn match_operator(operator: Operator, context: ParseContext) -> ParseResult<Operator> {
        let parse = Self::parse(context);
        match parse {
            ParseResult::Ok(ast) => {
                if ast.element == operator {
                    ParseResult::Ok(ast)
                } else {
                    ParseResult::Fail(Error {
                        kind: ErrorKind::ExpectingOperator {
                            expecting: operator,
                            found: ast.element,
                        },
                        loc: ast.loc,
                    })
                }
            }
            _ => parse,
        }
    }
}

pub struct ParseContext<'a> {
    pub tokens: &'a mut TokenStream,
    start: usize,
}

impl<'a> ParseContext<'a> {
    pub fn new(tokens: &'a mut TokenStream) -> ParseContext<'a> {
        ParseContext {
            start: tokens.cursor(),
            tokens,
        }
    }

    pub fn element<T>(&self, element: T) -> AstElement<T> {
        AstElement {
            element,
            loc: self.loc(),
        }
    }

    pub fn loc(&self) -> Loc {
        Loc {
            offset: self.start,
            len: self.tokens.cursor() - self.start,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AstElement<T> {
    pub element: T,
    pub loc: Loc,
}

impl<T> AstElement<T> {
    pub fn new(element: T, loc: Loc) -> AstElement<T> {
        AstElement { element, loc }
    }

    pub fn map<U, F>(self, mapper: F) -> AstElement<U>
    where
        F: FnOnce(T) -> U,
    {
        AstElement::new(mapper(self.element), self.loc)
    }

    pub const fn as_ref(&self) -> AstElement<&T> {
        AstElement {
            element: &self.element,
            loc: self.loc,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    pub offset: usize,
    pub len: usize,
}

impl Loc {
    pub fn new(offset: usize, len: usize) -> Loc {
        Loc { offset, len }
    }

    pub fn at(offset: usize) -> Loc {
        Loc { offset, len: 0 }
    }
}

impl Default for Loc {
    fn default() -> Self {
        Self::new(0, 0)
    }
}

pub enum ParseResult<T: Sized> {
    Ok(AstElement<T>),
    Fail(Error),
    Error(Error),
}

macro_rules! ok {
    ($parse_result:expr) => {{
        let parse_result = $parse_result;
        match parse_result {
            ParseResult::Ok(element) => element,
            ParseResult::Fail(err) => return ParseResult::Fail(err),
            ParseResult::Error(err) => return ParseResult::Error(err),
        }
    }};
}

macro_rules! not {
    ($parse_result:expr, $context:expr) => {{
        let cursor = $context.tokens.cursor();
        let parse_result = $parse_result;
        match parse_result {
            ParseResult::Ok(element) => {
                return ParseResult::Fail(Error {
                    kind: ErrorKind::ExpectingNegative,
                    loc: element.loc,
                })
            }
            ParseResult::Fail(_) => {
                $context.tokens.set_cursor(cursor);
            }
            ParseResult::Error(err) => return ParseResult::Error(err),
        }
    }};
}

macro_rules! persistent_ok {
    ($parse_result:expr, $context: expr) => {{
        let parse_result = $parse_result;
        match parse_result {
            ParseResult::Ok(element) => element,
            ParseResult::Fail(err) => return ParseResult::Error(err),
            ParseResult::Error(err) => return ParseResult::Error(err),
        }
    }};
}

macro_rules! repeated_ok {
    ($parse_result:expr) => {{
        let parse_result = $parse_result;
        match parse_result {
            ParseResult::Ok(element) => element,
            ParseResult::Fail(_) => break,
            ParseResult::Error(err) => return ParseResult::Error(err),
        }
    }};
}

macro_rules! optional_ok {
    ($parse_result:expr, $block_label:tt) => {{
        let parse_result = $parse_result;
        match parse_result {
            ParseResult::Ok(element) => element,
            ParseResult::Fail(_) => break $block_label,
            ParseResult::Error(err) => return ParseResult::Error(err),
        }
    }};
}

macro_rules! optional {
    ($parse_result:expr, $context:expr) => {{
        let cursor = $context.tokens.cursor();
        let parse_result = $parse_result;
        match parse_result {
            ParseResult::Ok(element) => Some(element),
            ParseResult::Fail(err) => {
                if err.kind.is_token_err() {
                    return ParseResult::Error(err);
                }
                $context.tokens.set_cursor(cursor);
                None
            }
            ParseResult::Error(err) => return ParseResult::Error(err),
        }
    }};
}

macro_rules! try_enum {
    ($variant:ident, $ty:ident, $context:expr) => {{
        let cursor = $context.tokens.cursor();
        let parse_result = $ty::parse(ParseContext::new($context.tokens));
        match parse_result {
            ParseResult::Ok(element) => {
                return ParseResult::Ok($context.element(Self::$variant(element)))
            }
            ParseResult::Fail(err) => {
                if err.kind.is_token_err() {
                    return ParseResult::Error(err);
                }
                $context.tokens.set_cursor(cursor);
            }
            ParseResult::Error(err) => return ParseResult::Error(err),
        }
    }};
}

pub trait Parseable
where
    Self: Sized,
{
    fn parse(context: ParseContext<'_>) -> ParseResult<Self>;
}
