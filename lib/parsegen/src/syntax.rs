use std::collections::HashMap;

use parser::{PestSyntaxParser, Rule};
use pest::{
    iterators::{Pair, Pairs},
    Parser,
};

use crate::code_builder::*;

pub mod parser {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "./syntax.pest"]
    pub struct PestSyntaxParser;
}

struct Expr {
    pub code: String,
    pub optional: bool,
}

enum BlockType {
    General,
    Repeated,
    Optional,
}

struct SyntaxParser {
    lang: Language,
    data: RuleData,
    block_stack: Vec<BlockType>,
    persist: bool,
    not: bool,
    keywords: HashMap<String, String>,
    operators: HashMap<String, String>,
}

impl SyntaxParser {
    fn write_indented(&mut self, str: &str) {
        self.data.code.write_indented(str);
    }

    fn indent(&mut self) {
        self.data.code.indent();
    }

    fn outdent(&mut self) {
        self.data.code.outdent();
    }

    fn make_ok(&self, inner: &str) -> String {
        if inner == "true" {
            return inner.to_string();
        }
        match self.block_stack.last().unwrap() {
            BlockType::Repeated => format!("repeated_ok!({})", inner),
            BlockType::Optional => format!("optional_ok!({}, 'optional_block)", inner),
            BlockType::General => match self.not {
                true => format!("not!({}, ParseContext::new(context.tokens))", inner),
                false => match self.persist {
                    true => format!("persistent_ok!({}, context)", inner),
                    false => format!("ok!({})", inner),
                },
            },
        }
    }

    fn build_expression(&mut self, pair: Pair<Rule>) -> Expr {
        match pair.as_rule() {
            Rule::def_statement => {
                let ident = pair.into_inner().next().unwrap().as_str();
                Expr {
                    code: match ident {
                        "true" => String::from("true"),
                        _ => format!("Ast{}::parse(ParseContext::new(context.tokens))", ident),
                    },
                    optional: false,
                }
            }
            Rule::optional_statement => {
                let inner_pair = pair.into_inner().nth(1).unwrap();
                self.block_stack.push(BlockType::Optional);
                let expr = if inner_pair.as_rule() == Rule::block_statement {
                    let code_start = self.data.code.len();
                    self.write_indented("let mut cursor = Some(context.tokens.cursor());\n");
                    self.write_indented("'optional_block: {\n");
                    self.indent();
                    self.parse_statement(inner_pair);
                    self.write_indented("cursor = None;\n");
                    self.outdent();
                    self.write_indented("}\n");
                    self.write_indented(
                        "if let Some(cursor) = cursor { context.tokens.set_cursor(cursor); }\n",
                    );
                    let code_end = self.data.code.len();
                    let code = self.data.code.substring(code_start, code_end);
                    self.data.code.remove_range(code_start..code_end);

                    Expr {
                        code,
                        optional: true,
                    }
                } else {
                    let inner_expr = self.build_expression(inner_pair);
                    Expr {
                        code: format!(
                            "optional!({}, ParseContext::new(context.tokens))",
                            inner_expr.code
                        ),
                        optional: true,
                    }
                };
                self.block_stack.pop();
                expr
            }
            other => panic!("not a valid expression type: {:?}", other),
        }
    }

    pub fn parse_statement(&mut self, pair: Pair<Rule>) {
        match pair.as_rule() {
            Rule::block_statement => {
                for pair in pair.into_inner() {
                    self.parse_statement(pair);
                }
            }
            Rule::repeated_statement => {
                let mut pairs = pair.into_inner();
                let pair = pairs.next().unwrap();
                let (optional, statement_pair) = match pair.as_rule() {
                    Rule::optional => (true, pairs.next().unwrap()),
                    _ => (false, pair),
                };
                if !optional {
                    self.parse_statement(statement_pair.clone());
                }

                self.write_indented("let mut cursor: usize;\n");
                self.write_indented("loop {\n");
                self.indent();
                self.write_indented("cursor = context.tokens.cursor();\n");
                self.block_stack.push(BlockType::Repeated);
                self.parse_statement(statement_pair);
                self.block_stack.pop();
                self.outdent();
                self.write_indented("}\n");
                self.write_indented("context.tokens.set_cursor(cursor);\n");
            }
            Rule::persist_statement => {
                self.persist = true;
                self.write_indented("// persist\n");
            }
            Rule::def_statement => {
                let expr = self.build_expression(pair);
                self.write_indented(&format!("{};\n", self.make_ok(&expr.code)));
            }
            Rule::optional_statement => {
                let expr = self.build_expression(pair);
                self.write_indented(&format!("{};\n", expr.code));
            }
            Rule::not_statement => {
                self.not = true;
                self.write_indented("// not\n");
                self.parse_statement(pair.into_inner().next().unwrap());
                self.not = false;
            }
            Rule::var_statement => {
                let mut var_pairs = pair.into_inner();
                let var = var_pairs.next().unwrap().as_str();
                let value = var_pairs.next().unwrap();
                let expr = self.build_expression(value);
                let mut expr_code = expr.code;
                let field = self
                    .data
                    .variables
                    .iter()
                    .find(|var_decl| var_decl.name == var)
                    .unwrap();
                if !expr.optional {
                    expr_code = self.make_ok(&expr_code);
                    if field.optional {
                        if field.boxed {
                            expr_code = format!("Box::new({})", expr_code);
                        }
                        expr_code = format!("Some({})", expr_code);
                    }
                }
                if field.boxed && !(!expr.optional && field.optional) {
                    expr_code = format!("Box::new({})", expr_code);
                }
                if field.repeated {
                    self.write_indented(&format!("{}.push({});\n", var, expr_code));
                } else {
                    self.write_indented(&format!("{} = {};\n", var, expr_code));
                }
            }
            Rule::keyword_statement => {
                let keyword = pair.into_inner().as_str();
                if let Some(keyword_name) = self.keywords.get(keyword) {
                    self.write_indented(&format!(
                        "{};\n",
                        self.make_ok(&format!("Keyword::match_keyword(Keyword::{}, ParseContext::new(context.tokens))", keyword_name)),
                    ));
                } else {
                    panic!(
                        "invalid keyword '{}' in rule {}",
                        keyword, self.data.rule_name
                    )
                }
            }
            Rule::operator_statement => {
                let operator = pair.into_inner().next().unwrap().as_str();
                if let Some(operator_name) = self.operators.get(operator) {
                    self.write_indented(&format!(
                        "{};\n",
                        self.make_ok(&format!("Operator::match_operator(Operator::{}, OperatorMode::Normal, ParseContext::new(context.tokens))", operator_name)),
                    ));
                } else {
                    panic!(
                        "invalid operator '{}' in rule {}",
                        operator, self.data.rule_name
                    )
                }
            }
            Rule::single_operator_statement => {
                let operator = pair.into_inner().next().unwrap().as_str();
                if let Some(operator_name) = self.operators.get(operator) {
                    self.write_indented(&format!(
                        "{};\n",
                        self.make_ok(&format!("Operator::match_operator(Operator::{}, OperatorMode::Single, ParseContext::new(context.tokens))", operator_name)),
                    ));
                } else {
                    panic!(
                        "invalid operator '{}' in rule {}",
                        operator, self.data.rule_name
                    )
                }
            }
            Rule::captures_statement => {
                for pair in pair.into_inner() {
                    let mut capture = pair.into_inner();
                    let var_name = capture.next().unwrap().as_str();

                    let mut type_modifier = None;
                    let mut boxed = false;
                    let mut type_name = loop {
                        let pair = capture.next().unwrap();
                        match pair.as_rule() {
                            Rule::capture_type => {
                                type_modifier = Some(pair.as_str().to_string());
                            }
                            Rule::boxed => {
                                boxed = true;
                            }
                            Rule::ident => {
                                break pair.as_str().to_string();
                            }
                            _ => unreachable!(),
                        }
                    };
                    if type_name != "bool" {
                        type_name = format!("AstElement<Ast{}>", type_name);
                    }
                    let (full_type, default_value, optional, repeated) = match self.lang {
                        Language::Rust => {
                            if boxed {
                                type_name = format!("Box<{}>", type_name);
                            }

                            if let Some(full_type) = type_modifier {
                                match full_type.as_str() {
                                    "optional" => (
                                        format!("Option<{}>", type_name),
                                        Some(String::from("None")),
                                        true,
                                        false,
                                    ),
                                    "repeated" => (
                                        format!("Vec<{}>", type_name),
                                        Some(String::from("Vec::new()")),
                                        false,
                                        true,
                                    ),
                                    _ => unreachable!(),
                                }
                            } else {
                                (type_name, None, false, false)
                            }
                        }
                        Language::TypeScript => {
                            if type_name == "bool" {
                                (
                                    String::from("boolean"),
                                    Some(String::from("false")),
                                    false,
                                    false,
                                )
                            } else if let Some(full_type) = type_modifier {
                                match full_type.as_str() {
                                    "optional" => (
                                        format!("{} | null", type_name),
                                        Some(String::from("null")),
                                        true,
                                        false,
                                    ),
                                    "repeated" => (
                                        format!("{}[]", type_name),
                                        Some(String::from("[]")),
                                        false,
                                        true,
                                    ),
                                    _ => unreachable!(),
                                }
                            } else {
                                (type_name, None, false, false)
                            }
                        }
                    };

                    let statement = if full_type == "bool" {
                        format!("let mut {}: bool = false;\n", var_name)
                    } else if let Some(default_value) = default_value {
                        format!("let mut {}: {} = {};\n", var_name, full_type, default_value)
                    } else {
                        format!("let mut {}: {};\n", var_name, full_type)
                    };
                    self.write_indented(&statement);
                    self.data.variables.push(StructField {
                        name: var_name.to_string(),
                        ty: full_type,
                        optional,
                        repeated,
                        boxed,
                    });
                }
            }
            Rule::return_statement => {
                let return_variable = pair.into_inner().next().unwrap().as_str();
                self.data.return_variable = Some(return_variable.to_string());
            }
            Rule::soi_statement => self.data.code.write_indented(&format!(
                "{};\n",
                self.make_ok("Soi::parse(ParseContext::new(context.tokens))")
            )),
            Rule::eoi_statement => self.data.code.write_indented(&format!(
                "{};\n",
                self.make_ok("Eoi::parse(ParseContext::new(context.tokens))")
            )),
            other => unimplemented!("{:?}", other),
        }
    }
}

pub struct RuleData {
    pub rule_name: String,
    pub variables: Vec<StructField>,
    pub code: CodeBuilder,
    pub return_variable: Option<String>,
}

fn parse_rule(
    cb: &mut CodeBuilder,
    mut pairs: Pairs<Rule>,
    keywords: &HashMap<String, String>,
    operators: &HashMap<String, String>,
) -> RuleData {
    let name = pairs.next().unwrap().as_str();
    let data = {
        let mut parser = SyntaxParser {
            lang: cb.lang,
            data: RuleData {
                rule_name: format!("Ast{}", name),
                variables: Vec::with_capacity(64),
                code: CodeBuilder::with_capacity(Language::Rust, 1024),
                return_variable: None,
            },
            block_stack: Vec::with_capacity(4),
            persist: false,
            not: false,
            keywords: keywords.clone(),
            operators: operators.clone(),
        };
        parser.block_stack.push(BlockType::General);
        for pair in pairs {
            parser.parse_statement(pair);
        }
        parser.data
    };
    cb.build_struct(&data);
    if cb.lang == Language::Rust {
        cb.build_struct_parser(&data);
    }
    data
}

fn parse_variant(
    cb: &mut CodeBuilder,
    mut pairs: Pairs<Rule>,
    keywords: &HashMap<String, String>,
    operators: &HashMap<String, String>,
) {
    let mut variants: Vec<String> = Vec::new();
    let name = format!("Ast{}", pairs.next().unwrap().as_str());
    let mut error_message = None;
    for pair in pairs {
        let rule_name = match pair.as_rule() {
            Rule::named_variant => parse_rule(cb, pair.into_inner(), keywords, operators).rule_name,
            Rule::ident => format!("Ast{}", pair.as_str()),
            Rule::error_definition => {
                error_message = Some(pair.into_inner().next().unwrap().as_str());
                continue;
            }
            _ => unreachable!(),
        };
        variants.push(rule_name);
    }

    cb.build_enum(&name, &variants);
    if cb.lang == Language::Rust {
        cb.build_enum_parser(&name, &variants, error_message);
    }
}

pub fn parse_syntax(
    lang: Language,
    syntax_path: &str,
    syntax_defs: &str,
    keywords: &HashMap<String, String>,
    operators: &HashMap<String, String>,
) -> Result<String, pest::error::Error<Rule>> {
    let parser = PestSyntaxParser::parse(Rule::syntax, syntax_defs);
    match parser {
        Ok(pairs) => {
            let mut cb = CodeBuilder::with_capacity(lang, 64 * 1024);
            for pair in pairs {
                match pair.as_rule() {
                    Rule::rule_definition => {
                        parse_rule(&mut cb, pair.into_inner(), keywords, operators);
                    }
                    Rule::variant_definition => {
                        parse_variant(&mut cb, pair.into_inner(), keywords, operators);
                    }
                    Rule::extern_definition | Rule::EOI => (),
                    x => unreachable!("{:?}", x),
                }
            }
            Ok(cb.into_string())
        }
        Err(err) => Err(err.with_path(syntax_path)),
    }
}

pub fn list_definitions(syntax_defs: &str) -> Vec<String> {
    let parser = PestSyntaxParser::parse(Rule::syntax, syntax_defs);
    match parser {
        Ok(pairs) => {
            let mut defs = Vec::with_capacity(1024);
            for pair in pairs {
                match pair.as_rule() {
                    Rule::rule_definition | Rule::variant_definition => {
                        let name = pair.into_inner().next().unwrap().as_str();
                        defs.push(format!("Ast{name}"));
                    }
                    Rule::extern_definition | Rule::EOI => (),
                    x => unreachable!("{:?}", x),
                }
            }
            defs
        }
        Err(err) => panic!("{err}"),
    }
}
