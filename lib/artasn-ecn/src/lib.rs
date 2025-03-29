#[path = "./compiler/ecn.gen.rs"]
mod parser;

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::parser::{self, ParseContext, ParseResult, Parseable, TokenStream};

    #[test]
    fn test_parse_ecn() {
        let source = include_str!("../test-data/Test.edm");
        let mut token_stream = TokenStream::from_string(source, false);
        let parser = parser::AstProgram::parse(ParseContext::new(&mut token_stream));
        match parser {
            ParseResult::Ok(program) => {
                fs::write("/tmp/ecn-ast.txt", format!("{:#?}", program)).unwrap();
                println!("Success! Wrote AST to /tmp/ecn-ast.txt");
            }
            ParseResult::Fail(err) | ParseResult::Error(err) => {
                fs::write("/tmp/ecn-ast.txt", err.get_message(source)).unwrap();
                println!("Error! Wrote error message to /tmp/ecn-ast.txt");
            }
        }
    }
}
