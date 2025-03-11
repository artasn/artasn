use serde::Deserialize;

use crate::{
    encoding::{
        DecodeError, DecodeMode, DecodeResult, DecodedValue, DecodedValueForm, DecodedValueKind,
        DerReader,
    },
    module::{ModuleIdentifier, QualifiedIdentifier},
    types::UntaggedType,
    values::ValueResolve,
};

use super::{context::DeclaredValue, options::CompilerConfig, CompileError, Compiler, Context};

pub fn compile_module_fallible(
    context: &mut Context,
    path: &str,
    source: &str,
) -> Vec<CompileError> {
    context.clear();

    let mut compiler = Compiler::new(CompilerConfig::default());
    match compiler.add_source(path.to_string(), source.to_string()) {
        Ok(()) => (),
        Err(err) => {
            eprintln!("{}", err);
            panic!("parse module '{}' failed", path);
        }
    }
    compiler.compile(context)
}

pub fn compile_module(context: &mut Context, path: &str, source: &str) {
    let errors = compile_module_fallible(context, path, source);
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
}

#[derive(Deserialize, Debug)]
struct ValueTestEntry {
    pub tests: Option<Vec<TestMode>>,
    pub name: String,
    pub der: String,
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
    expected_der: &[u8],
) {
    let tagged_type = declared_value
        .ty
        .resolve(context)
        .expect("failed to resolve type");
    let value = declared_value
        .value
        .resolve(context)
        .expect("failed to resolve value");

    let mut buf = Vec::with_capacity(expected_der.len());
    value
        .der_encode(&mut buf, context, &tagged_type)
        .unwrap_or_else(|_| panic!("failed to encode value '{}'", ident));
    let buf = buf.into_iter().rev().collect::<Vec<u8>>();

    assert!(
        expected_der == buf.as_slice(),
        "value    = {}\nexpected = {}\nfound    = {}",
        ident,
        hex::encode_upper(expected_der),
        hex::encode_upper(&buf)
    );

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
    declared_value: &DeclaredValue,
    der: &[u8],
    json_value: &serde_json::Value,
) {
    let mode = DecodeMode::SpecificType {
        source_ident: match &declared_value.ty.ty {
            UntaggedType::Reference(typeref) => Some(typeref.element.clone()),
            UntaggedType::BuiltinType(_) => panic!("value type is not a typereference"),
        },
        component_name: None,
        resolved: declared_value
            .ty
            .resolve(context)
            .expect("failed resolving type"),
    };
    let reader = DerReader::new(der, 0);
    let values = reader
        .into_iter()
        .map(|tlv| DecodedValue::der_decode(context, tlv.map_err(DecodeError::Io)?, &mode))
        .collect::<DecodeResult<Vec<DecodedValue>>>()
        .unwrap();
    compare_decoded_values_to_json_values(&values, json_value)
}

pub fn execute_json_test(module_file: &str, data_file: &str) {
    let test_file: TestFile = serde_json::from_str(data_file).expect("malformed data file");

    let mut context = Context::new();
    compile_module(
        &mut context,
        &format!("{}.asn", test_file.module),
        module_file,
    );

    let all_tests = vec![TestMode::Encode, TestMode::Decode];
    for entry in &test_file.values {
        let name = &entry.name;
        let der = hex::decode(&entry.der).expect("invalid DER hex");

        let ident = QualifiedIdentifier {
            module: ModuleIdentifier {
                name: test_file.module.clone(),
                oid: None,
            },
            name: name.to_string(),
        };
        let declared_value = context
            .lookup_value(&ident)
            .unwrap_or_else(|| panic!("value '{}.{}' does not exist", test_file.module, name));

        let tests = entry
            .tests
            .as_ref()
            .or(test_file.tests.as_ref())
            .unwrap_or(&all_tests);
        if tests.contains(&TestMode::Encode) {
            test_encode_value(&context, &ident, declared_value, &der);
        }
        if tests.contains(&TestMode::Decode) {
            let value = entry.value.as_ref().expect("missing 'value' in test JSON");
            test_decode_value(&context, declared_value, &der, value);
        }
    }
}

fn execute_json_compile_test(module_name: &str, module_file: &str, data_file: &str) {
    let mut context = Context::new();
    let errors = compile_module_fallible(&mut context, module_name, module_file);

    let entries: Vec<String> = serde_json::from_str(data_file).expect("malformed data file");

    assert_eq!(errors.len(), entries.len());

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

#[test]
pub fn test_unique_tag_compliance() {
    let module_file = include_str!("../../test-data/compile/UniqueTagTest.asn");
    let data_file = include_str!("../../test-data/compile/UniqueTagTest.test.json");

    execute_json_compile_test("UniqueTagTest.asn", module_file, data_file);
}

#[test]
pub fn test_unique_alternative_compliance_implicit_tagging() {
    let module_file =
        include_str!("../../test-data/compile/UniqueAlternativeTestImplicitTagging.asn");
    let data_file =
        include_str!("../../test-data/compile/UniqueAlternativeTestImplicitTagging.test.json");

    execute_json_compile_test(
        "UniqueAlternativeTestImplicitTagging.asn",
        module_file,
        data_file,
    );
}

// TODO: make this work
// #[test]
// pub fn test_unique_alternative_compliance_automatic_tagging() {
//     let module_file =
//         include_str!("../../test-data/compile/UniqueAlternativeTestAutomaticTagging.asn");
//     let data_file =
//         include_str!("../../test-data/compile/UniqueAlternativeTestAutomaticTagging.test.json");

//     execute_json_compile_test(
//         "UniqueAlternativeTestAutomaticTagging.asn",
//         module_file,
//         data_file,
//     );
// }
