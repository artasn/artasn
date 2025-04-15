use std::{fmt::Write, ops::Range};

use crate::syntax::RuleData;

static INDENT_UNIT: &str = "    ";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Language {
    Rust,
    TypeScript,
}

pub struct CodeBuilder {
    pub lang: Language,
    buf: String,
    /// The identation prefix (in spaces) before each line is written.
    /// Always equals zero ("") or a multiple of 4 (e.g. "    ").
    indent: String,
}

impl CodeBuilder {
    pub fn with_capacity(lang: Language, capacity: usize) -> CodeBuilder {
        CodeBuilder {
            buf: String::with_capacity(capacity),
            indent: String::with_capacity(80),
            lang,
        }
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn substring(&self, start: usize, end: usize) -> String {
        self.buf[start..end].to_string()
    }

    pub fn into_string(self) -> String {
        self.buf
    }

    pub fn remove_range(&mut self, range: Range<usize>) {
        self.buf.replace_range(range, "");
    }

    pub fn to_indented_string(&self, indent: &str) -> String {
        self.buf
            .split('\n')
            .filter(|line| !line.is_empty())
            .map(|line| format!("{}{}", indent, line))
            .reduce(|a, b| format!("{}\n{}", a, b))
            .unwrap_or_else(String::new)
    }

    pub fn write_indented(&mut self, str: &str) {
        self.buf.write_str(&self.indent).unwrap();
        self.buf.write_str(str).unwrap();
    }

    pub fn write(&mut self, str: &str) {
        self.buf.write_str(str).unwrap();
    }

    pub fn indent(&mut self) {
        self.indent.write_str(INDENT_UNIT).unwrap();
    }

    pub fn outdent(&mut self) {
        self.indent.replace_range(0..INDENT_UNIT.len(), "");
    }

    pub fn build_struct(&mut self, data: &RuleData) {
        match self.lang {
            Language::Rust => {
                self.write("#[derive(Debug, Clone)]\n");
                if let Some(return_variable) = &data.return_variable {
                    self.write("pub struct ");
                    self.write(&data.rule_name);
                    self.write("(pub ");
                    let return_variable_type = &data
                        .variables
                        .iter()
                        .find(|var| &var.name == return_variable)
                        .unwrap()
                        .ty;
                    self.write(return_variable_type);
                    self.write(");\n\n");
                } else {
                    self.write("pub struct ");
                    self.write(&data.rule_name);
                    self.write(" {\n");
                    self.indent();
                    for field in &data.variables {
                        self.write_indented("pub ");
                        self.write(&field.name);
                        self.write(": ");
                        self.write(&field.ty);
                        self.write(",\n");
                    }
                    self.outdent();
                    self.write("}\n\n");
                }

                if cfg!(feature = "js-serialize") {
                    self.write("impl JsSerialize for ");
                    self.write(&data.rule_name);
                    self.write("{\n");
                    self.indent();
                    self.write_indented("fn serialize(&self) -> JsValue {\n");
                    self.indent();
                    self.write_indented("let obj = Object::new();\n");
                    self.write_indented("Reflect::set(&obj, &\"ast\".into(), &\"");
                    self.write(&data.rule_name[3..]); // drop the Ast prefix
                    self.write("\".into()).unwrap();\n");
                    if data.return_variable.is_some() {
                        self.write_indented(
                            "Reflect::set(&obj, &\"item\".into(), &self.0.serialize()).unwrap();\n",
                        );
                    } else {
                        for field in &data.variables {
                            self.write_indented("Reflect::set(&obj, &\"");
                            self.write(&field.name);
                            self.write("\".into(), &self.");
                            self.write(&field.name);
                            self.write(".serialize()).unwrap();\n");
                        }
                    }
                    self.write_indented("obj.into()\n");
                    self.outdent();
                    self.write_indented("}\n");
                    self.outdent();
                    self.write("}\n\n");
                }
            }
            Language::TypeScript => {
                self.write("export interface ");
                self.write(&data.rule_name);
                self.write(" {\n");
                self.indent();

                self.write_indented("ast: '");
                self.write(&data.rule_name[3..]); // drop the 'Ast' prefix
                self.write("';\n");

                if let Some(return_variable) = &data.return_variable {
                    let return_variable = data
                        .variables
                        .iter()
                        .find(|var| &var.name == return_variable)
                        .unwrap();
                    self.write_indented("item: ");
                    self.write(&return_variable.ty);
                    self.write(";\n");
                } else {
                    for field in &data.variables {
                        self.write_indented(&field.name);
                        self.write(": ");
                        self.write(&field.ty);
                        self.write(";\n");
                    }
                }

                self.outdent();
                self.write("}\n\n");
            }
        }
    }

    pub fn build_enum(&mut self, name: &str, variants: &[String]) {
        match self.lang {
            Language::Rust => {
                self.write("#[derive(Debug, Clone)]\n");
                self.write("pub enum ");
                self.write(name);
                self.write(" {\n");
                self.indent();
                for variant in variants {
                    self.write_indented(&variant[3..]); // drop the Ast prefix
                    self.write("(AstElement<");
                    self.write(variant);
                    self.write(">),\n");
                }
                self.outdent();
                self.write("}\n\n");

                if cfg!(feature = "js-serialize") {
                    self.write("impl JsSerialize for ");
                    self.write(name);
                    self.write("{\n");
                    self.indent();
                    self.write_indented("fn serialize(&self) -> JsValue {\n");
                    self.indent();
                    self.write_indented("let obj = Object::new();\n");
                    self.write_indented("Reflect::set(&obj, &\"ast\".into(), &\"");
                    self.write(&name[3..]); // drop the Ast prefix
                    self.write("\".into()).unwrap();\n");

                    self.write_indented("let (variant, value) = match self {\n");
                    self.indent();
                    for variant in variants {
                        self.write_indented("Self::");
                        self.write(&variant[3..]); // drop the Ast prefix
                        self.write("(value) => (\"");
                        self.write(&variant[3..]);
                        self.write("\", value.serialize()),\n");
                    }
                    self.outdent();
                    self.write_indented("};\n");

                    self.write_indented(
                        "Reflect::set(&obj, &\"variant\".into(), &variant.into()).unwrap();\n",
                    );
                    self.write_indented(
                        "Reflect::set(&obj, &\"value\".into(), &value).unwrap();\n",
                    );

                    self.write_indented("obj.into()\n");
                    self.outdent();
                    self.write_indented("}\n");
                    self.outdent();
                    self.write("}\n\n");
                }
            }
            Language::TypeScript => {
                self.write("export type ");
                self.write(name);
                self.write(" = { ast: '");
                self.write(&name[3..]); // drop the 'Ast' prefix
                self.write("'; } & (");

                for (i, variant) in variants.iter().enumerate() {
                    if i > 0 {
                        self.write(" | ");
                    }

                    self.write("{\n");
                    self.indent();

                    self.write_indented("variant: '");
                    self.write(&variant[3..]); // drop the Ast prefix
                    self.write("';\n");

                    self.write_indented("value: AstElement<");
                    self.write(variant);
                    self.write(">;\n");

                    self.outdent();
                    self.write("}");
                }

                self.write(");\n\n");
            }
        }
    }

    pub fn build_struct_parser(&mut self, data: &RuleData) {
        assert_eq!(self.lang, Language::Rust);

        self.write("impl Parseable for ");
        self.write(&data.rule_name);
        self.write(" {\n");
        self.indent();
        self.write_indented("fn parse(mut context: ParseContext) -> ParseResult<Self> {\n");
        self.indent();

        self.write(&data.code.to_indented_string(&self.indent));

        self.write("\n\n");

        // return the element object
        self.write_indented("ParseResult::Ok(context.element(");
        self.write(&data.rule_name);
        match data.return_variable.as_ref() {
            Some(return_variable) => {
                self.write("(");
                self.write(return_variable);
                self.write(")");
            }
            None => {
                self.write(" {");
                for variable in &data.variables {
                    self.write(&variable.name);
                    self.write(", ");
                }
                self.write(" }");
            }
        }
        self.write("))");

        self.outdent();
        self.write("\n");
        self.write_indented("}\n");
        self.outdent();
        self.write("}\n\n");
    }

    pub fn build_enum_parser(
        &mut self,
        enum_name: &str,
        variants: &[String],
        error_message: Option<&str>,
    ) {
        assert_eq!(self.lang, Language::Rust);

        self.write("impl Parseable for ");
        self.write(enum_name);
        self.write(" {\n");
        self.indent();
        self.write_indented("fn parse(mut context: ParseContext) -> ParseResult<Self> {\n");
        self.indent();

        for variant in variants {
            self.write_indented("try_enum!(");
            self.write(&variant[3..]); // drop the Ast prefix
            self.write(", ");
            self.write(variant); // full type name
            self.write(", ParseContext::new(context.tokens));\n");
        }

        self.write_indented("ParseResult::Fail(Error{kind: ");
        match error_message {
            Some(message) => {
                self.write("ErrorKind::VariantUnmatchedMessage { message: String::from(\"");
                self.write(message);
                self.write("\"), }");
            }
            None => {
                self.write("ErrorKind::VariantUnmatched { variant: \"");
                self.write(enum_name);
                self.write("\".to_string(), }");
            }
        }

        self.write(", loc: context.loc(), })\n");

        self.outdent();
        self.write_indented("}\n");
        self.outdent();
        self.write("}\n\n");
    }
}

pub struct StructField {
    pub name: String,
    pub ty: String,
    pub optional: bool,
    pub boxed: bool,
    pub repeated: bool,
}
