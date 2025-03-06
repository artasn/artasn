use super::{context_mut, CompileError, Compiler};

pub fn compile_module_fallible(path: &str, source: &str) -> Vec<CompileError> {
    context_mut().clear();

    let mut compiler = Compiler::new();
    match compiler.add_source(path.to_string(), source.to_string()) {
        Ok(()) => (),
        Err(err) => {
            eprintln!("{}", err);
            assert!(false);
        }
    }
    compiler.compile()
}

pub fn compile_module(path: &str, source: &str) {
    let errors = compile_module_fallible(path, source);
    if errors.len() > 0 {
        for error in errors {
            eprintln!("{}", error);
        }
        assert!(false);
    }
}

#[test]
pub fn test_unique_tag_compliance() {
    let module_file = include_str!("../../test-data/compile/UniqueTagTest.asn");
    let errors = compile_module_fallible("UniqueTagTest.asn", module_file);

    let data_file = include_str!("../../test-data/compile/UniqueTagTest.data");
    let entries: Vec<String> = serde_json::from_str(&data_file).expect("malformed data file");

    assert_eq!(errors.len(), entries.len());

    for i in 0..errors.len() {
        let error = &errors[i].to_string();
        let entry = &entries[i];
        assert!(error.starts_with(entry), "error = {}\nentry = {}", error, entry);
    }
}
