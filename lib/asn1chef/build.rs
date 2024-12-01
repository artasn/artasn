use std::{fs::File, io::Read};

const SYNTAX_FILE: &str = "src/compiler/asn1.syntax";
const GEN_FILE: &str = "src/compiler/asn1.gen.rs";

fn main() {
    println!("cargo:rerun-if-changed={}", SYNTAX_FILE);
    let mut syntax = String::with_capacity(16 * 1024);
    File::open(SYNTAX_FILE).unwrap().read_to_string(&mut syntax).unwrap();
    parsegen::generate_parser(SYNTAX_FILE, &syntax, GEN_FILE)
}
