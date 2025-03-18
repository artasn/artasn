#![allow(dead_code)]
#![allow(unused_macros)]

use super::tokenizer::*;

pub struct TokenStream {
    tokenizer: Box<dyn Tokenizer>,
}

impl TokenStream {
    pub fn from_string(source: &str, permit_lowercase_string_indicator: bool) -> TokenStream {
        TokenStream {
            tokenizer: Box::new(StringTokenizer::new(
                source,
                permit_lowercase_string_indicator,
            )),
        }
    }

    pub fn from_tokens(tokens: Vec<Token>) -> TokenStream {
        TokenStream {
            tokenizer: Box::new(AotTokenizer::new(tokens)),
        }
    }

    pub fn try_parse(&mut self, kind: TokenKind, data: Option<TokenData>) -> Result<Token> {
        let token = self.tokenizer.tokenize_next()?;
        if token.kind == kind {
            Ok(token)
        } else {
            Err(Error {
                kind: ErrorKind::ExpectingOther {
                    expecting: vec![(kind, data)],
                    found: token.clone(),
                },
                loc: token.loc,
            })
        }
    }

    pub fn try_next(&mut self) -> Result<Token> {
        Ok(self.tokenizer.tokenize_next()?)
    }

    pub fn set_cursor(&mut self, cursor: usize) {
        self.tokenizer.set_cursor(cursor);
    }

    pub fn cursor(&self) -> usize {
        self.tokenizer.cursor()
    }

    pub fn offset(&self) -> usize {
        self.tokenizer.offset()
    }

    pub fn into_tokenizer(self) -> Box<dyn Tokenizer> {
        self.tokenizer
    }

    pub fn as_tokens(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        loop {
            let token = self.tokenizer.tokenize_next()?;
            if token.kind == TokenKind::Eoi {
                tokens.push(token);
                return Ok(tokens);
            }
            tokens.push(token);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Soi;

impl Parseable for Soi {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::Soi, None)
            .map(|token| {
                ParseResult::Ok(AstElement {
                    element: Soi,
                    loc: token.loc,
                })
            })
            .unwrap_or_else(ParseResult::Fail)
    }
}

#[derive(Debug, Clone)]
pub struct Eoi;

impl Parseable for Eoi {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::Eoi, None)
            .map(|token| {
                ParseResult::Ok(AstElement {
                    element: Eoi,
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
            .try_parse(TokenKind::TypeReference, None)
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
            .try_parse(TokenKind::TypeReference, None)
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
                                expecting: vec![(
                                    TokenKind::EncodingReference,
                                    Some(TokenData::Named(name.clone())),
                                )],
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
            .try_parse(TokenKind::ValueReference, None)
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
            .try_parse(TokenKind::String, None)
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
pub struct AstNumber(pub num::BigUint);

impl Parseable for AstNumber {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::Number, None)
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

impl ParseableEnum for Keyword {
    fn parse(context: ParseContext, data: Option<TokenData>) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::Keyword, data)
            .map(|token| {
                ParseResult::Ok(AstElement {
                    element: match token.data.unwrap() {
                        TokenData::Keyword(keyword) => keyword,
                        _ => unreachable!(),
                    },
                    loc: token.loc,
                })
            })
            .unwrap_or_else(ParseResult::Fail)
    }
}

impl Keyword {
    pub fn match_keyword(keyword: Keyword, context: ParseContext) -> ParseResult<Keyword> {
        let parse = Self::parse(context, Some(TokenData::Keyword(keyword.clone())));
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

impl ParseableEnum for Operator {
    fn parse(context: ParseContext, data: Option<TokenData>) -> ParseResult<Self> {
        context
            .tokens
            .try_parse(TokenKind::Operator, data)
            .map(|token| {
                ParseResult::Ok(AstElement {
                    element: match token.data.unwrap() {
                        TokenData::Operator(op) => op,
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
        let parse = Self::parse(context, Some(TokenData::Operator(operator.clone())));
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

#[derive(Debug, Clone)]
pub struct AstBracedTokenStream(pub Vec<Token>);

impl Parseable for AstBracedTokenStream {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        let mut tokens = Vec::new();
        match context.tokens.try_parse(
            TokenKind::Operator,
            Some(TokenData::Operator(Operator::OpenBrace)),
        ) {
            Ok(token) => tokens.push(token),
            Err(err) => return ParseResult::Fail(err),
        };

        let mut brace_depth = 1;
        loop {
            let next_token = match context.tokens.try_next() {
                Ok(token) => token,
                Err(err) => return ParseResult::Fail(err),
            };

            match next_token.kind {
                TokenKind::Operator => {
                    let op = match next_token.data.as_ref().unwrap() {
                        TokenData::Operator(op) => op,
                        _ => unreachable!(),
                    };
                    match op {
                        Operator::OpenBrace => brace_depth += 1,
                        Operator::CloseBrace => brace_depth -= 1,
                        _ => (),
                    }
                }
                TokenKind::Eoi => {
                    return ParseResult::Fail(Error {
                        loc: next_token.loc,
                        kind: ErrorKind::ExpectingOther {
                            expecting: vec![(
                                TokenKind::Operator,
                                Some(TokenData::Operator(Operator::CloseBrace)),
                            )],
                            found: next_token,
                        },
                    })
                }
                _ => (),
            }

            tokens.push(next_token);
            if brace_depth == 0 {
                let last_token = tokens.last().unwrap();
                let loc = Loc::new(
                    tokens[0].loc.offset,
                    (last_token.loc.offset + last_token.loc.len) - tokens[0].loc.offset,
                );
                return ParseResult::Ok(AstElement::new(AstBracedTokenStream(tokens), loc));
            }
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
            start: tokens.offset(),
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
            len: self.tokens.offset() - self.start,
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

pub trait ParseableEnum
where
    Self: Sized,
{
    fn parse(context: ParseContext<'_>, data: Option<TokenData>) -> ParseResult<Self>;
}
