use std::{fs::File, io::Read};

const SYNTAX_FILE: &str = "src/compiler/asn1.syntax";
const GEN_FILE: &str = "src/compiler/asn1.gen.rs";

fn main() {
    if cfg!(feature = "parsegen-js-serialize") {
        // force the build script to always run
        // this is necessary because  parsegen-js-serialize
        // causes parsegen to output TypeScript files,
        // even when none of the Rust files in the crate have actually changed
        // this behavior is generally gross (as all of parsegen is) and should be improved upon at some point
        println!("cargo:rerun-if-changed=NULL");
    } else {
        println!("cargo:rerun-if-changed={}", SYNTAX_FILE);
    }

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
            use crate::compiler::ast::extra::AstSyntaxTokenLiteral;

            enum_str! {
                pub enum Keyword {
                    Absent = "ABSENT",
                    Encoded = "ENCODED",
                    Intersection = "INTERSECTION",
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
                    Any = "ANY",
                    Defined = "DEFINED",
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
        },
    )
}
