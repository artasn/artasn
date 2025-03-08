#![allow(unused)]

use crate::parser::*;
use std::{
    collections::HashSet,
    fmt::{format, Display, Write},
};

macro_rules! enum_str {
    (pub enum $name:ident {
        $($variant:ident = $val:literal),*,
    }) => {
        #[derive(Debug, Clone, PartialEq)]
        pub enum $name {
            $($variant),*
        }

        impl $name {
            pub const VARIANTS: &[$name] = &[$(Self::$variant),*];
            pub const NAMES: &[&'static str] = &[$($val),*];

            pub fn from_name(name: &str) -> Option<$name> {
                Some(match name {
                    $($val => $name::$variant),*,
                    _ => return None,
                })
            }

            pub const fn name(&self) -> &'static str {
                match self {
                    $(Self::$variant => $val),*
                }
            }

            pub const fn variant_name(&self) -> &'static str {
                match self {
                    $(Self::$variant => stringify!($variant)),*
                }
            }
        }
    };
}

enum_str! {
    pub enum Keyword {
        Absent = "ABSENT",
        Encoded = "ENCODED",
        Intersection = "INTERSETION",
        Sequence = "SEQUENCE",
        AbstractSyntax = "ABSTRACT-SYNTAX",
        EncodingControl = "ENCODING-CONTROL",
        Iso646String = "ISO646String",
        Set = "SET",
        All = "ALL",
        End = "END",
        Max = "MAX",
        Settings = "SETTINGS",
        Application = "APPLICATION",
        Enumerated = "ENUMERATED",
        Min = "MIN",
        Size = "SIZE",
        Automatic = "AUTOMATIC",
        Except = "EXCEPT",
        MinusInfinity = "MINUS-INFINITY",
        String = "STRING",
        Begin = "BEGIN",
        Explicit = "EXPLICIT",
        NotANumber = "NOT-A-NUMBER",
        Syntax = "SYNTAX",
        Bit = "BIT",
        Exports = "EXPORTS",
        Null = "NULL",
        T61String = "T61String",
        BmpString = "BMPString",
        Extensibility = "EXTENSIBILITY",
        NumericString = "NumericString",
        Tags = "TAGS",
        Boolean = "BOOLEAN",
        External = "EXTERNAL",
        Object = "OBJECT",
        Identifier = "IDENTIFIER",
        TeletexString = "TeletexString",
        By = "BY",
        False = "FALSE",
        ObjectDescriptor = "ObjectDescriptor",
        Time = "TIME",
        Character = "CHARACTER",
        From = "FROM",
        Octet = "OCTET",
        TimeOfDay = "TIME-OF-DAY",
        Choice = "CHOICE",
        GeneralizedTime = "GeneralizedTime",
        Of = "OF",
        True = "TRUE",
        Class = "CLASS",
        GeneralString = "GeneralString",
        OidIri = "OID-IRI",
        TypeIdentifier = "TYPE-IDENTIFIER",
        Component = "COMPONENT",
        GraphicString = "GraphicString",
        Optional = "OPTIONAL",
        Union = "UNION",
        Components = "COMPONENTS",
        IA5String = "IA5String",
        Pattern = "PATTERN",
        Unique = "UNIQUE",
        Constrained = "CONSTRAINED",
        Pdv = "PDV",
        Universal = "UNIVERSAL",
        Containing = "CONTAINING",
        Implicit = "IMPLICIT",
        PlusInfinity = "PLUS-INFINITY",
        UniversalString = "UniversalString",
        Date = "DATE",
        Implied = "IMPLIED",
        Present = "PRESENT",
        UtcTime = "UTCTime",
        DateTime = "DATE-TIME",
        Imports = "IMPORTS",
        PrintableString = "PrintableString",
        Utf8String = "UTF8String",
        Default = "DEFAULT",
        Includes = "INCLUDES",
        Private = "PRIVATE",
        VideotexString = "VideotexString",
        Definitions = "DEFINITIONS",
        Instance = "INSTANCE",
        Real = "REAL",
        VisibleString = "VisibleString",
        Duration = "DURATION",
        Instructions = "INSTRUCTIONS",
        RelativeOid = "RELATIVE-OID",
        With = "WITH",
        Embedded = "EMBEDDED",
        Integer = "INTEGER",
        RelativeOidIri = "RELATIVE-OID-IRI",
        Successors = "SUCCESSORS",
        Descendants = "DESCENDANTS",
    }
}

enum_str! {
    pub enum Operator {
        Assignment = "::=",
        OpenBrace = "{",
        CloseBrace = "}",
        OpenParen = "(",
        CloseParen = ")",
        LeftVersionBrackets = "[[",
        RightVersionBrackets = "]]",
        OpenBracket = "[",
        CloseBracket = "]",
        Ellipsis = "...",
        RangeSeparator = "..",
        Colon = ":",
        LeftAngleBracket = "<",
        RightAngleBracket = ">",
        Ampersand = "&",
        Pipe = "|",
        SingleQuote = "'",
        DoubleQuote = "\"",
        At = "@",
        Exclamation = "!",
        Caret = "^",
        Semicolon = ";",
        Comma = ",",
        Period = ".",
        Negative = "-",
    }
}

lazy_static::lazy_static! {
    static ref FAST_KEYWORD_CHECK: std::collections::HashSet<&'static str> = {
        let keywords = Keyword::NAMES;
        let mut set = std::collections::HashSet::with_capacity(keywords.len());
        for keyword in keywords {
            set.insert(*keyword);
        }
        set
    };
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub data: Option<TokenData>,
    pub loc: Loc,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.kind.to_string())?;
        if let Some(data) = &self.data {
            f.write_str("(")?;
            f.write_str(&match data {
                TokenData::Keyword(keyword) => keyword.name().to_string(),
                TokenData::Named(name) => name.clone(),
                TokenData::Number(num) => num.to_string(),
                TokenData::Operator(op) => op.name().to_string(),
                TokenData::String(kind, value) => format!("{} '{}'", kind, value),
            })?;
            f.write_str(")")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Soi,
    ValueReference,
    TypeReference,
    EncodingReference,
    Keyword,
    Number,
    String,
    Operator,
    Eoi,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Soi => "Soi",
            Self::ValueReference => "valuereference",
            Self::TypeReference => "typereference",
            Self::EncodingReference => "encodingreference",
            Self::Keyword => "keyword",
            Self::Number => "integer",
            Self::String => "string",
            Self::Operator => "operator",
            Self::Eoi => "Eoi",
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum StringKind {
    CString,
    HString,
    BString,
}

impl Display for StringKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::CString => "cstring",
            Self::HString => "hstring",
            Self::BString => "bstring",
        })
    }
}

#[derive(Debug, Clone)]
pub enum TokenData {
    Named(String),
    Number(u64),
    String(StringKind, String),
    Keyword(Keyword),
    Operator(Operator),
}

impl TokenData {
    pub fn to_string(&self, kind: &TokenKind) -> String {
        match self {
            Self::Named(name) => format!("{} '{}'", kind, name),
            Self::Number(value) => format!("integer {}", value),
            Self::String(kind, value) => format!("{} '{}'", kind, value),
            Self::Keyword(keyword) => format!("keyword {}", keyword.name()),
            Self::Operator(op) => format!("operator {}", op.name()),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub loc: Loc,
}

impl Error {
    pub fn pos(&self, module_source: &str) -> (usize, usize) {
        if self.loc.offset == 0 {
            return (1, 1);
        }
        let line = 1 + module_source[..self.loc.offset]
            .chars()
            .filter(|c| *c == '\n')
            .count();
        let col = self.loc.offset - module_source[..self.loc.offset].rfind('\n').unwrap_or(1);
        (line, col)
    }

    pub fn get_message(&self, module_source: &str) -> String {
        let (line, col) = self.pos(module_source);
        format!("{} at {}:{}", self.kind.message(), line, col)
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnexpectedToken(char),
    ExpectingEoi,
    ExpectingOther {
        expecting: Vec<TokenKind>,
        found: Token,
    },
    ExpectingKeyword {
        expecting: Keyword,
        found: Keyword,
    },
    ExpectingOperator {
        expecting: Operator,
        found: Operator,
    },
    InvalidStringIndicator(char),
    IllegalStringCharacter {
        string_kind: StringKind,
        character: char,
    },
    MalformedIdentifier {
        ident: String,
    },
    MalformedNumber {
        number: String,
    },
    NumberTooLarge {
        number: String,
    },
    VariantUnmatched {
        variant: String,
    },
    UnterminatedString,
    ExpectingNegative,
    Ast(String),
}

impl ErrorKind {
    /// Returns true if this error resulted from parsing a single token.
    /// Otherwise returns false (i.e. this error resulted from parsing a multi-token rule).
    pub fn is_token_err(&self) -> bool {
        matches!(
            self,
            Self::InvalidStringIndicator(_)
                | Self::IllegalStringCharacter { .. }
                | Self::MalformedIdentifier { .. }
                | Self::MalformedNumber { .. }
                | Self::UnterminatedString
        )
    }

    pub fn message(&self) -> String {
        match self {
            ErrorKind::UnexpectedToken(token) => {
                format!("unexpected token '{}'", token)
            }
            ErrorKind::ExpectingEoi => String::from("expecting end of input"),
            ErrorKind::ExpectingOther { expecting, found } => {
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
                if let Some(found_data) = &found.data {
                    format!(
                        "expecting {}, found {}",
                        expecting_str,
                        found_data.to_string(&found.kind)
                    )
                } else {
                    format!(
                        "expecting {}, found {}",
                        expecting_str,
                        found.kind
                    )
                }
            }
            ErrorKind::IllegalStringCharacter {
                string_kind,
                character,
            } => format!(
                "illegal {} character '{}'",
                string_kind,
                character
            ),
            ErrorKind::InvalidStringIndicator(indicator) => format!(
                "illegal string indicator suffix '{}' (expecting 'B', 'H', or a cstring)",
                indicator
            ),
            ErrorKind::ExpectingKeyword { expecting, found } => format!(
                "expecting keyword {}, found keyword {}",
                expecting.name(),
                found.name()
            ),
            ErrorKind::ExpectingOperator { expecting, found } => format!(
                "expecting operator '{}', found operator '{}'",
                expecting.name(),
                found.name()
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
            ErrorKind::ExpectingNegative => {
                "expecting provided token not to be present".to_string()
            }
            ErrorKind::Ast(message) => message.clone(),
        }
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message())
    }
}

pub struct Tokenizer {
    source: Vec<char>,
    cursor: usize,
    yielded_soi: bool,
    permit_lowercase_string_indicator: bool,
}

lazy_static::lazy_static! {
    static ref BSTRING_NEGATIVE_REGEX: regex::Regex = regex::Regex::new(r"[^01]").unwrap();
    static ref HSTRING_NEGATIVE_REGEX: regex::Regex = regex::Regex::new(r"[^A-F0-9]").unwrap();
    static ref IDENT_REGEX: regex::Regex = regex::Regex::new(r"^[a-zA-Z](-?[a-zA-Z0-9])*$").unwrap();
}

impl Tokenizer {
    const LINE_COMMENT: &[char] = &['-', '-'];
    const BLOCK_COMMENT_START: &[char] = &['/', '*'];
    const BLOCK_COMMENT_END: &[char] = &['*', '/'];

    pub fn new(source: &str, permit_lowercase_string_indicator: bool) -> Tokenizer {
        Tokenizer {
            source: source.chars().collect(),
            cursor: 0,
            yielded_soi: false,
            permit_lowercase_string_indicator,
        }
    }

    pub fn set_cursor(&mut self, cursor: usize) {
        self.cursor = cursor;
    }

    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn tokenize_next(&mut self) -> Result<Token> {
        if !self.yielded_soi {
            self.yielded_soi = true;
            return Ok(Token {
                kind: TokenKind::Soi,
                data: None,
                loc: Loc { offset: 0, len: 0 },
            });
        }
        // continuously skip comments and whitespace
        loop {
            let cursor = self.cursor;
            self.skip_comments();
            self.skip_whitespace();
            if self.cursor == cursor {
                break;
            }
        }
        if self.is_ended() {
            return Ok(Token {
                kind: TokenKind::Eoi,
                data: None,
                loc: Loc {
                    offset: self.source.len(),
                    len: 0,
                },
            });
        } else {
            if let Some(string) = self.tokenize_string()? {
                return Ok(string);
            }
            if let Some(number) = self.tokenize_number()? {
                return Ok(number);
            }
            if let Some(named) = self.tokenize_name()? {
                return Ok(named);
            }
            if let Some(op) = self.tokenize_operator() {
                return Ok(op);
            }
        }

        Err(Error {
            loc: Loc::at(self.cursor),
            kind: ErrorKind::UnexpectedToken(self.current_char()),
        })
    }

    fn is_ended(&self) -> bool {
        self.cursor == self.source.len()
    }

    fn skip_comments(&mut self) {
        'comment_loop: while !self.is_ended() {
            if self.starts_with(Self::LINE_COMMENT) {
                self.cursor += Self::LINE_COMMENT.len(); // skip "--" prefix
                while !self.is_ended() {
                    if self.current_char() == '\n' {
                        self.cursor += 1;
                        break 'comment_loop;
                    } else if self.starts_with(Self::LINE_COMMENT) {
                        // inline comment, e.g. --hello world--
                        self.cursor += Self::LINE_COMMENT.len(); // skip "--" suffix
                        break 'comment_loop;
                    } else {
                        // inside a comment; ignore character
                        self.cursor += 1;
                    }
                }
                // if we reached here, there was an unclosed comment
                // TODO: throw error?
                break 'comment_loop;
            } else if self.starts_with(Self::BLOCK_COMMENT_START) {
                self.cursor += Self::BLOCK_COMMENT_START.len(); // skip "/*"" prefix
                while !self.is_ended() {
                    if self.starts_with(Self::BLOCK_COMMENT_END) {
                        self.cursor += Self::BLOCK_COMMENT_END.len(); // skip "*/" prefix
                        break 'comment_loop;
                    } else {
                        // inside a comment; ignore character
                        self.cursor += 1;
                    }
                }
            } else {
                break;
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.is_ended() {
            let current = self.current_char();
            // see https://www.unicode.org/reports/tr44/#PropList.txt for all included whitespace characters
            if current.is_whitespace() {
                self.cursor += 1;
            } else {
                break;
            }
        }
    }

    fn tokenize_string(&mut self) -> Result<Option<Token>> {
        let (loc, text, kind) = if let Some((loc, text, _)) = self.tokenize_string_delim('"')? {
            (loc, text, StringKind::CString)
        } else if let Some((loc, text, suffix)) = self.tokenize_string_delim('\'')? {
            match self.assert_string_validity(loc, text, suffix) {
                Ok(string) => string,
                Err(err) => return Err(err),
            }
        } else {
            return Ok(None);
        };
        Ok(Some(Token {
            kind: TokenKind::String,
            data: Some(TokenData::String(kind, text)),
            loc,
        }))
    }

    fn assert_string_validity(
        &self,
        loc: Loc,
        text: String,
        suffix: char,
    ) -> Result<(Loc, String, StringKind)> {
        let bstring_indicators;
        let hstring_indicators;
        if self.permit_lowercase_string_indicator {
            bstring_indicators = "Bb";
            hstring_indicators = "Hh";
        } else {
            bstring_indicators = "B";
            hstring_indicators = "H";
        }
        let (invalid_char, kind) = if bstring_indicators.contains(suffix) {
            (BSTRING_NEGATIVE_REGEX.find(&text), StringKind::BString)
        } else if hstring_indicators.contains(suffix) {
            (HSTRING_NEGATIVE_REGEX.find(&text), StringKind::HString)
        } else {
            return Err(Error {
                kind: ErrorKind::InvalidStringIndicator(suffix),
                loc,
            });
        };
        if let Some(invalid_char) = invalid_char {
            Err(Error {
                kind: ErrorKind::IllegalStringCharacter {
                    string_kind: kind,
                    character: invalid_char.as_str().chars().next().unwrap(),
                },
                loc: Loc::new(loc.offset + 1 + invalid_char.start(), 1),
            })
        } else {
            Ok((loc, text, kind))
        }
    }

    fn tokenize_string_delim(&mut self, string_delim: char) -> Result<Option<(Loc, String, char)>> {
        if self.current_char() == string_delim {
            let loc_start = self.cursor;
            self.cursor += 1;
            let value_start = self.cursor;
            while !self.is_ended() {
                if self.current_char() == string_delim {
                    break;
                }
                self.cursor += 1;
            }
            if self.is_ended() {
                return Err(Error {
                    kind: ErrorKind::UnterminatedString,
                    loc: Loc::new(loc_start, self.cursor),
                });
            }
            // increment cursor to account for closed deliiter
            let value_end = self.cursor;
            self.cursor += 1;
            let indicator = self.current_char();
            let loc_end = self.cursor;
            let valid_indicator = "BbHh".contains(indicator);
            if valid_indicator {
                // move cursor past indicator if valid
                self.cursor += 1;
            }
            let mut value: String = self.source[value_start..value_end].iter().collect();
            if valid_indicator {
                // only replace spaces in the string if it's not a cstring
                // TODO: should cstring have newlines removed?
                value = value.replace([' ', '\n'], "");
            }
            Ok(Some((
                Loc::new(loc_start, loc_end - loc_start),
                value,
                indicator,
            )))
        } else {
            Ok(None)
        }
    }

    fn tokenize_number(&mut self) -> Result<Option<Token>> {
        if self.current_char().is_ascii_digit() {
            let start = self.cursor;
            while !self.is_ended() {
                if !self.current_char().is_ascii_digit() {
                    break;
                }
                self.cursor += 1;
            }
            let num: String = self.source[start..self.cursor].iter().collect();
            let loc = Loc {
                offset: start,
                len: num.len(),
            };

            if Self::is_name_char(self.current_char()) {
                return Err(Error {
                    kind: ErrorKind::MalformedNumber {
                        number: format!("{}{}", num, self.current_char()),
                    },
                    loc,
                });
            }

            if let Ok(number) = num.parse::<u64>() {
                Ok(Some(Token {
                    kind: TokenKind::Number,
                    data: Some(TokenData::Number(number)),
                    loc,
                }))
            } else {
                Err(Error {
                    kind: ErrorKind::NumberTooLarge { number: num },
                    loc,
                })
            }
        } else {
            Ok(None)
        }
    }

    fn tokenize_name(&mut self) -> Result<Option<Token>> {
        if Self::is_name_char(self.current_char()) && self.current_char() != '-' {
            let start = self.cursor;
            let mut name = String::with_capacity(1);
            while !self.is_ended() {
                if Self::is_name_char(self.current_char()) {
                    name.write_char(self.current_char()).unwrap();
                    self.cursor += 1;
                } else if self.current_char() == '\n' {
                    if let Some(last_char) = name.chars().last() {
                        if last_char == '-' {
                            // if we get a hyphen followed by a newline
                            // the identifier can still be parsed as a multi-line identifier
                            // skip all the whitespace and continue parsing
                            //
                            // TODO: check is this is compliant with the X.680 standard;
                            // this occurs in RFC3126 module 'ETS-ElectronicSignatureFormats-88syntax':
                            // FROM PKIX1Explicit88
                            //      {iso(1) identified-organization(3) dod(6) internet(1)
                            //      security(5) mechanisms(5) pkix(7) id-mod(0) id-pkix1-explicit-
                            //      88(1)}
                            self.skip_whitespace();
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            let loc = Loc::new(start, name.len());
            if FAST_KEYWORD_CHECK.contains(&name.as_str()) {
                return Ok(Some(Token {
                    kind: TokenKind::Keyword,
                    data: Some(TokenData::Keyword(Keyword::from_name(&name).unwrap())),
                    loc,
                }));
            }

            if !IDENT_REGEX.is_match(&name) {
                return Err(Error {
                    kind: ErrorKind::MalformedIdentifier { ident: name },
                    loc,
                });
            }

            let first_char = name.chars().next().unwrap();
            let kind = match first_char {
                'a'..='z' => TokenKind::ValueReference,
                'A'..='Z' => TokenKind::TypeReference,
                _ => {
                    return Err(Error {
                        kind: ErrorKind::UnexpectedToken(first_char),
                        loc,
                    })
                }
            };

            Ok(Some(Token {
                kind,
                data: Some(TokenData::Named(name)),
                loc,
            }))
        } else {
            Ok(None)
        }
    }

    fn tokenize_operator(&mut self) -> Option<Token> {
        let start = self.cursor;
        for operator in Operator::VARIANTS {
            let text = operator.name();
            if self.starts_with_str(text) {
                self.cursor += text.len();
                return Some(Token {
                    kind: TokenKind::Operator,
                    data: Some(TokenData::Operator(operator.clone())),
                    loc: Loc::new(start, text.len()),
                });
            }
        }
        None
    }

    fn is_name_char(ch: char) -> bool {
        ch.is_ascii_alphanumeric() || ch == '-'
    }

    fn current_char(&self) -> char {
        if self.is_ended() {
            return 0 as char;
        }
        self.source[self.cursor]
    }

    fn starts_with_str(&self, s: &str) -> bool {
        if self.cursor + s.len() >= self.source.len() {
            return false;
        }
        for (i, ch) in s.chars().enumerate() {
            if self.source[self.cursor + i] != ch {
                return false;
            }
        }
        true
    }

    fn starts_with(&self, prefix: &[char]) -> bool {
        self.source[self.cursor..].starts_with(prefix)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_ok {
        ($result:expr) => {{
            let r = $result;
            assert!(r.is_ok(), "{} is not Ok: {:?}", stringify!($result), r);
            r.unwrap()
        }};
    }

    #[test]
    pub fn empty_input() {
        let mut t = Tokenizer::new("", true);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Soi), "{:?}", r);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Eoi), "{:?}", r);
    }

    #[test]
    pub fn only_comments_is_equivalent_to_empty_input() {
        let mut t = Tokenizer::new("-- Hello world", true);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Soi), "{:?}", r);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Eoi), "{:?}", r);
        let mut t = Tokenizer::new("-- Hello world\n", true);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Soi), "{:?}", r);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Eoi), "{:?}", r);

        let mut t = Tokenizer::new("--Goodbye world--", true);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Soi), "{:?}", r);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Eoi), "{:?}", r);
        let mut t = Tokenizer::new("--Goodbye world--\n", true);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Soi), "{:?}", r);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Eoi), "{:?}", r);

        let mut t = Tokenizer::new("-- Hello world\n--Goodbye world--", true);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Soi), "{:?}", r);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Eoi), "{:?}", r);
        let mut t = Tokenizer::new("-- Hello world\n--Goodbye world--\n", true);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Soi), "{:?}", r);
        let r = assert_ok!(t.tokenize_next());
        assert!(matches!(r.kind, TokenKind::Eoi), "{:?}", r);
    }
}
