use super::*;

#[derive(Debug, Clone)]
pub struct AstSyntaxTokenLiteral {
    pub token: std::string::String,
}

lazy_static::lazy_static! {
    // from X.681 clause 10.6
    static ref ILLEGAL_TOKEN_LITERALS: std::collections::HashSet<&'static str> = std::collections::HashSet::from_iter(vec![
        "BIT",
        "BOOLEAN",
        "CHARACTER",
        "CHOICE",
        "CONTAINING",
        "DATE",
        "DATE-TIME",
        "DURATION",
        "EMBEDDED",
        "END",
        "ENUMERATED",
        "EXTERNAL",
        "FALSE",
        "INSTANCE",
        "INTEGER",
        "MINUS-INFINITY",
        "NOT-A-NUMBER",
        "NULL",
        "OBJECT",
        "OCTET",
        "OID-IRI",
        "PLUS-INFINITY",
        "REAL",
        "RELATIVE-OID",
        "RELATIVE-OID-IRI",
        "SEQUENCE",
        "SET",
        "TIME",
        "TIME-OF-DAY",
        "TRUE",
        "TYPE-IDENTIFIER",
    ]);
}

impl Parseable for AstSyntaxTokenLiteral {
    fn parse(context: ParseContext) -> ParseResult<Self> {
        context
            .tokens
            .try_parse_token_literal()
            .map(|token| {
                let token_data = token.data.unwrap();
                let token_text = match &token_data {
                    TokenData::Named(named) => named.as_str(),
                    TokenData::Keyword(keyword) => keyword.name(),
                    TokenData::Operator(op) => op.name(),
                    _ => unreachable!(),
                };
                if ILLEGAL_TOKEN_LITERALS.contains(token_text) {
                    ParseResult::Error(Error {
                        kind: ErrorKind::IllegalTokenLiteral {
                            token: token_text.to_string(),
                        },
                        loc: token.loc,
                    })
                } else {
                    ParseResult::Ok(AstElement {
                        element: AstSyntaxTokenLiteral {
                            token: token_text.to_string(),
                        },
                        loc: token.loc,
                    })
                }
            })
            .unwrap_or_else(ParseResult::Fail)
    }
}

pub trait TokenLiteralParser {
    fn try_parse_token_literal(&mut self) -> Result<Token>;
}

impl TokenLiteralParser for TokenStream {
    fn try_parse_token_literal(&mut self) -> Result<Token> {
        let token = self.tokenizer.tokenize_next(Some(OperatorMode::Single))?;
        match token.kind {
            TokenKind::TypeReference => {
                let name = match token.data.as_ref().unwrap() {
                    TokenData::Named(name) => name,
                    _ => unreachable!(),
                };

                if name.chars().all(|ch| ch.is_ascii_uppercase() || ch == '-') {
                    return Ok(token);
                }
            }
            TokenKind::Keyword => return Ok(token),
            TokenKind::Operator => match token.data.as_ref().unwrap() {
                TokenData::Operator(op) => {
                    if op.clone() == Operator::Comma {
                        return Ok(token);
                    }
                }
                _ => unreachable!(),
            },
            _ => (),
        }

        Err(Error {
            kind: ErrorKind::ExpectingOther {
                expecting: vec![(TokenKind::TokenLiteral, None)],
                found: token.clone(),
            },
            loc: token.loc,
        })
    }
}
