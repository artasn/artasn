use std::path::Path;

use serde::Deserialize;

use crate::{
    encoding::*,
    module::{ModuleIdentifier, QualifiedIdentifier},
    types::UntaggedType,
    values::ValueResolve,
};

use super::{context::DeclaredValue, options::CompilerConfig, CompileError, Compiler, Context};

pub fn compile_modules_fallible(
    context: &mut Context,
    modules: &[(&str, &str)],
) -> Vec<CompileError> {
    context.clear();

    let mut compiler = Compiler::new(CompilerConfig::default());
    match compiler.add_stdlib() {
        Ok(()) => (),
        Err(err) => {
            eprintln!("{}", err);
            panic!("parse stdlib failed");
        }
    }

    for (path, source) in modules {
        match compiler.add_source(path.to_string(), source.to_string()) {
            Ok(()) => (),
            Err(err) => {
                eprintln!("{}", err);
                panic!("parse module '{}' failed", path);
            }
        }
    }
    compiler.compile(context)
}

pub fn compile_module(context: &mut Context, path: &str, source: &str) {
    let errors = compile_modules_fallible(context, &[(path, source)]);
    if !errors.is_empty() {
        for error in errors {
            eprintln!("{}", error);
        }
        panic!("compile module '{}' failed", path);
    }
}

#[derive(Deserialize, Debug, PartialEq, Eq)]
enum TestMode {
    Encode,
    Decode,
    #[serde(rename = "DER")]
    Distinguished,
    #[serde(rename = "PER")]
    Packed,
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
enum PackedEncodings {
    BothAlignments(String),
    ByAlignment { aligned: String, unaligned: String },
}

#[derive(Deserialize, Debug)]
struct ValueTestEntry {
    pub tests: Option<Vec<TestMode>>,
    pub name: String,
    pub der: Option<String>,
    pub per: Option<PackedEncodings>,
    pub value: Option<serde_json::Value>,
}

#[derive(Deserialize, Debug)]
struct TestFile {
    pub module: String,
    pub tests: Option<Vec<TestMode>>,
    pub values: Vec<ValueTestEntry>,
}

fn test_encode_value(
    context: &Context,
    ident: &QualifiedIdentifier,
    declared_value: &DeclaredValue,
    expected_encodings: &[(TransferSyntax, Vec<u8>)],
) {
    let typed_value = declared_value
        .value
        .resolve(context)
        .expect("failed to resolve value");

    for (syntax, expected_encoding) in expected_encodings {
        let mut buf = Vec::with_capacity(expected_encoding.len());
        let encoder = syntax.get_codec().encoder.expect("no encoder");
        encoder(syntax, EncodeMode::Normal, &mut buf, context, &typed_value).unwrap_or_else(
            |err| {
                panic!(
                    "failed to {} encode value '{}': {}",
                    syntax, ident, err.kind
                )
            },
        );

        assert!(
            expected_encoding == buf.as_slice(),
            "value    = {}\ntransfer syntax = {}\nexpected = {}\nfound    = {}",
            ident,
            syntax,
            hex::encode_upper(expected_encoding),
            hex::encode_upper(&buf)
        );
    }

    println!("validated encoding of value '{}'", ident);
}

fn compare_constructed_decoded_value_to_json_values(
    constructed_values: &[DecodedValue],
    json_value: &serde_json::Value,
) {
    if json_value.is_object() {
        let components = json_value.as_object().unwrap();
        assert_eq!(constructed_values.len(), components.len());
        for (i, (name, component_value)) in components.iter().enumerate() {
            let encoded_value = &constructed_values[i];
            let metadata = encoded_value.metadata.as_ref().expect("missing metadata");
            assert_eq!(
                name,
                metadata
                    .component_name
                    .as_ref()
                    .expect("missing component name")
            );

            compare_decoded_value_to_json_value(encoded_value, component_value);
        }
    } else if json_value.is_array() {
        todo!("SEQUENCE/SET OF");
    } else {
        panic!("got Constructed value, but JSON value is not an object or an array")
    }
}

fn compare_decoded_values_to_json_values(
    decoded_values: &[DecodedValue],
    json_value: &serde_json::Value,
) {
    for decoded_value in decoded_values {
        match &decoded_value.form {
            DecodedValueForm::Constructed(values) => {
                compare_constructed_decoded_value_to_json_values(values, json_value);
            }
            DecodedValueForm::Primitive(_) => {
                compare_decoded_value_to_json_value(decoded_value, json_value)
            }
        };
    }
}

fn compare_decoded_value_to_json_value(
    decoded_value: &DecodedValue,
    json_value: &serde_json::Value,
) {
    match &decoded_value.form {
        DecodedValueForm::Primitive(kind) => match kind {
            DecodedValueKind::Integer(i) => {
                let i: u64 = i.try_into().expect("INTEGER is too large");
                assert_eq!(json_value.as_u64().expect("expecting INTEGER"), i);
            }
            DecodedValueKind::Boolean(b) => {
                assert_eq!(json_value.as_bool().expect("expecting BOOLEAN"), *b)
            }
            DecodedValueKind::Enumerated(i) => {
                assert_eq!(json_value.as_i64().expect("expecting INTEGER"), *i);
            }
            DecodedValueKind::CharacterString(_, str) => assert_eq!(
                json_value.as_str().expect("expecting character string"),
                str
            ),
            other => panic!("unexpected value: {:?}", other),
        },
        DecodedValueForm::Constructed(values) => {
            compare_constructed_decoded_value_to_json_values(values, json_value);
        }
    }
}

fn test_decode_value(
    context: &Context,
    ident: &QualifiedIdentifier,
    declared_value: &DeclaredValue,
    encodings: &[(TransferSyntax, Vec<u8>)],
    json_value: &serde_json::Value,
) {
    let mode = DecodeMode::SpecificType {
        source_ident: match &declared_value.ty.ty {
            UntaggedType::BuiltinType(_) | UntaggedType::ObjectClassField(_) => {
                panic!("value type is not a typereference")
            }
            UntaggedType::Reference(typeref) => Some(typeref.element.clone()),
        },
        component_name: None,
        resolved: declared_value
            .ty
            .resolve(context)
            .expect("failed resolving type"),
    };

    for (syntax, encoding) in encodings {
        let decoder = syntax.get_codec().decoder.expect("no decoder");
        let values = decoder(syntax, &mode, encoding, context)
            .unwrap_or_else(|_| panic!("failed to decode value '{}'", ident));

        compare_decoded_values_to_json_values(&values, json_value)
    }
}

pub fn execute_json_test(module_file: &str, data_file: &str) {
    let test_file: TestFile = serde_json::from_str(data_file).expect("malformed data file");

    let mut context = Context::new();
    compile_module(
        &mut context,
        &format!("{}.asn", test_file.module),
        module_file,
    );

    let default_tests = vec![TestMode::Encode, TestMode::Decode];
    for entry in &test_file.values {
        let name = &entry.name;

        let mut encodings = Vec::new();
        if let Some(tests) = test_file.tests.as_ref().or(entry.tests.as_ref()) {
            if tests.contains(&TestMode::Distinguished) {
                encodings.push((
                    TransferSyntax::Basic(BasicEncodingKind::Distinguished),
                    hex::decode(entry.der.as_ref().expect("missing field 'der'"))
                        .expect("invalid DER hex"),
                ));
            }
            if tests.contains(&TestMode::Packed) {
                let per = entry.per.as_ref().expect("missing field 'per'");
                match per {
                    PackedEncodings::BothAlignments(hex) => {
                        let binary = hex::decode(hex).expect("invalid PER hex");
                        encodings.push((
                            TransferSyntax::Packed(PackedEncodingKind::CanonicalAligned),
                            binary.clone(),
                        ));
                        encodings.push((
                            TransferSyntax::Packed(PackedEncodingKind::CanonicalUnaligned),
                            binary,
                        ));
                    }
                    PackedEncodings::ByAlignment { aligned, unaligned } => {
                        encodings.push((
                            TransferSyntax::Packed(PackedEncodingKind::CanonicalAligned),
                            hex::decode(aligned).expect("invalid CPER hex"),
                        ));
                        encodings.push((
                            TransferSyntax::Packed(PackedEncodingKind::CanonicalUnaligned),
                            hex::decode(unaligned).expect("invalid CUPER hex"),
                        ));
                    }
                }
            }
        }

        // default to DER
        if encodings.is_empty() {
            encodings.push((
                TransferSyntax::Basic(BasicEncodingKind::Distinguished),
                hex::decode(entry.der.as_ref().expect("missing field 'der'"))
                    .expect("invalid DER hex"),
            ));
        }

        let ident = QualifiedIdentifier::new(
            ModuleIdentifier::with_name(test_file.module.clone()),
            name.to_string(),
        );
        let declared_value = context
            .lookup_value(&ident)
            .unwrap_or_else(|| panic!("value '{}.{}' does not exist", test_file.module, name));

        let tests = entry
            .tests
            .as_ref()
            .or(test_file.tests.as_ref())
            .unwrap_or(&default_tests);
        if tests.contains(&TestMode::Encode) {
            test_encode_value(&context, &ident, declared_value, &encodings);
        }
        if tests.contains(&TestMode::Decode) {
            let value = entry.value.as_ref().expect("missing 'value' in test JSON");
            test_decode_value(&context, &ident, declared_value, &encodings, value);
        }
    }
}

fn execute_json_compile_test(data_file: &str, modules: &[(&str, &str)]) {
    let mut context = Context::new();
    let errors = compile_modules_fallible(&mut context, modules);

    let entries: Vec<String> = serde_json::from_str(data_file).expect("malformed data file");

    assert_eq!(
        errors.len(),
        entries.len(),
        "\nerrors = {}",
        errors
            .into_iter()
            .map(|err| err.to_string())
            .collect::<Vec<String>>()
            .join("\n")
    );

    for i in 0..errors.len() {
        let error = &errors[i].to_string();
        let entry = &entries[i];
        assert!(
            error.starts_with(entry),
            "error = {}\nentry = {}",
            error,
            entry
        );
    }
}

macro_rules! json_test {
    ( $test:ident, $name:literal ) => {
        #[test]
        pub fn $test() {
            let module_file = include_str!(concat!($name, ".asn"));
            let data_file = include_str!(concat!($name, ".test.json"));

            crate::compiler::test::execute_json_test(module_file, data_file);
        }
    };
}
pub(crate) use json_test;

macro_rules! json_compile_test {
    ( $test:ident, $main:literal ) => {
        #[test]
        pub fn $test() {
            let data_file = include_str!(concat!($main, ".test.json"));

            let mut modules = Vec::new();
            let main_path = Path::new(concat!($main, ".asn"))
                .file_name()
                .unwrap()
                .to_str()
                .unwrap();
            let main_module = include_str!(concat!($main, ".asn"));
            modules.push((main_path, main_module));

            crate::compiler::test::execute_json_compile_test(
                data_file,
                &modules,
            );
        }
    };
    ( $test:ident, $root:literal, $main:literal, $($extra:literal),* ) => {
        #[test]
        pub fn $test() {
            let data_file = include_str!(concat!($root, $main, ".test.json"));

            let mut modules = Vec::new();
            let main_path = Path::new(concat!($root, $main, ".asn"))
                .file_name()
                .unwrap()
                .to_str()
                .unwrap();
            let main_module = include_str!(concat!($root, $main, ".asn"));
            modules.push((main_path, main_module));

            $(
                let extra_path = Path::new(concat!($root, $extra, ".asn"))
                    .file_name()
                    .unwrap()
                    .to_str()
                    .unwrap();
                let extra_module = include_str!(concat!($root, $extra, ".asn"));
                modules.push((extra_path, extra_module));
            )+

            crate::compiler::test::execute_json_compile_test(
                data_file,
                &modules,
            );
        }
    };
}

json_compile_test!(
    test_unique_tag_compliance,
    "../../test-data/compile/UniqueTagTest"
);
json_compile_test!(
    test_unique_alternative_compliance_implicit_tagging,
    "../../test-data/compile/UniqueAlternativeTestImplicitTagging"
);
// TODO: make this work
// json_compile_test!(test_unique_alternative_compliance_automatic_tagging, "../../test-data/compile/UniqueAlternativeTestAutomaticTagging");
json_compile_test!(
    test_constraint_verifier,
    "../../test-data/compile/ConstraintTest"
);
json_compile_test!(
    test_matching_imports,
    "../../test-data/compile/import/",
    "MatchingImportTest",
    "ModuleWithOID",
    "ModuleWithoutOID"
);
json_compile_test!(
    test_omitted_oid_import,
    "../../test-data/compile/import/",
    "OmittedOIDImportTest",
    "ModuleWithOID",
    "ModuleWithoutOID"
);
json_compile_test!(
    test_misplaced_oid_import,
    "../../test-data/compile/import/",
    "MisplacedOIDImportTest",
    "ModuleWithOID",
    "ModuleWithoutOID"
);
json_compile_test!(
    test_mismatch_oid_import,
    "../../test-data/compile/import/",
    "MismatchOIDImportTest",
    "ModuleWithOID",
    "ModuleWithoutOID"
);
json_compile_test!(
    test_parameter_violation,
    "../../test-data/compile/ParameterViolationTest"
);
json_compile_test!(
    test_object_class,
    "../../test-data/compile/classes/ObjectClassTest"
);
json_compile_test!(
    test_object_set,
    "../../test-data/compile/classes/ObjectSetTest"
);
json_compile_test!(
    test_component_reference,
    "../../test-data/compile/classes/ComponentReferenceTest"
);
json_compile_test!(
    test_value_set,
    "../../test-data/compile/classes/ValueSetTest"
);
json_compile_test!(
    test_import,
    "../../test-data/compile/import/",
    "ImportTest",
    "ImportTestIntermediate",
    "ImportTestRemote"
);
json_compile_test!(
    test_cyclic_import,
    "../../test-data/compile/import/",
    "CyclicImportTest",
    "CyclicImportTestIntermediate",
    "CyclicImportTestRemote"
);
