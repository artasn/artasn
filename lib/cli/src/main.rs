use std::{fs, time::Instant};

use asn1chef::compiler::Compiler;

fn main() -> anyhow::Result<()> {
    let start = Instant::now();

    let mut compiler = Compiler::new();
    for test_file in fs::read_dir("cli/test")? {
        let test_file = test_file?;
        if test_file.file_name().to_string_lossy().ends_with(".asn") {
            let asn_source = fs::read_to_string(test_file.path())?;
            compiler.add_source(
                test_file
                    .path()
                    .into_os_string()
                    .into_string()
                    .expect("path() -> String"),
                asn_source,
            )?;
        }
    }

    let errors = compiler.compile();
    if errors.len() > 0 {
        for error in errors {
            println!("{error}");
        }
        std::process::exit(1);
    }

    let elapsed = {
        let elapsed = start.elapsed();
        let ms = elapsed.as_millis();
        if ms == 0 {
            format!("{}us", elapsed.as_micros())
        } else {
            let s = elapsed.as_secs();
            if s == 0 {
                format!("{}ms", ms)
            } else {
                format!("{}.{:03}s", s, ms % 1000)
            }
        }
    };
    println!("finished in {}", elapsed);

    Ok(())
}
