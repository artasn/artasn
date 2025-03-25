use std::{fs::File, io::Read};

const SYNTAX_FILE: &str = "src/compiler/ecn.syntax";
const GEN_FILE: &str = "src/compiler/ecn.gen.rs";

fn main() {
    println!("cargo:rerun-if-changed={}", SYNTAX_FILE);
    let mut syntax = String::with_capacity(16 * 1024);
    File::open(SYNTAX_FILE)
        .unwrap()
        .read_to_string(&mut syntax)
        .unwrap();
    parsegen::generate_parser(
        SYNTAX_FILE,
        &syntax,
        GEN_FILE,
        syn::parse_quote! {
            enum_str! {
                pub enum Keyword {
                    All = "ALL",
                    As = "AS",
                    Begin = "BEGIN",
                    Ber = "BER",
                    Bits = "BITS",
                    By = "BY",
                    Cer = "CER",
                    Completed = "COMPLETED",
                    Decode = "DECODE",
                    Der = "DER",
                    Distribution = "DISTRIBUTION",
                    Encode = "ENCODE",
                    EncodingClass = "ENCODING-CLASS",
                    EncodeDecode = "ENCODE-DECODE",
                    EncodingDefinitions = "ENCODING-DEFINITIONS",
                    End = "END",
                    Except = "EXCEPT",
                    Exports = "EXPORTS",
                    False = "FALSE",
                    Fields = "FIELDS",
                    From = "FROM",
                    Generates = "GENERATES",
                    If = "IF",
                    Imports = "IMPORTS",
                    In = "IN",
                    LinkDefinitions = "LINK-DEFINITIONS",
                    Mapping = "MAPPING",
                    Max = "MAX",
                    Min = "MIN",
                    MinusInfinity = "MINUS-INFINITY",
                    NonEcnBegin = "NON-ECN-BEGIN",
                    NonEcnEnd = "NON-ECN-END",
                    Null = "NULL",
                    OptionalEncoding = "OPTIONAL-ENCODING",
                    Options = "OPTIONS",
                    Ordered = "ORDERED",
                    Outer = "OUTER",
                    PerBasicAligned = "PER-BASIC-ALIGNED",
                    PerBasicUnaligned = "PER-BASIC-UNALIGNED",
                    PerCanonicalAligned = "PER-CANONICAL-ALIGNED",
                    PerCanonicalUnaligned = "PER-CANONICAL-UNALIGNED",
                    PlusInfinity = "PLUS-INFINITY",
                    Reference = "REFERENCE",
                    Remainder = "REMAINDER",
                    Renames = "RENAMES",
                    Size = "SIZE",
                    Structure = "STRUCTURE",
                    Structured = "STRUCTURED",
                    To = "TO",
                    Transforms = "TRANSFORMS",
                    True = "TRUE",
                    Union = "UNION",
                    Use = "USE",
                    UseSet = "USE-SET",
                    Values = "VALUES",
                    With = "WITH",
                }
            }

            enum_str! {
                pub enum Operator {
                    Assignment = "::=",
                    OpenBrace = "{",
                    CloseBrace = "}",
                    OpenParen = "(",
                    CloseParen = ")",
                    OpenBracket = "[",
                    CloseBracket = "]",
                    Hash = "#",
                    Semicolon = ";",
                    Comma = ",",
                    Pipe = "|",
                    Period = ".",
                }
            }
        },
    )
}
