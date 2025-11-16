use std::process::Command;
use tempfile::TempDir;

fn run(air_source: &str, expected_output: &str) {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let output_path = temp_dir.path().join("test-program");
    let runtime_path = std::env::var("RUNTIME_LIB_PATH").unwrap();

    airc::compile_air(air_source, output_path.to_str().unwrap(), &runtime_path)
        .expect("Compilation failed");

    let output = Command::new(&output_path)
        .output()
        .expect("Failed to execute program");

    let stdout = String::from_utf8(output.stdout).expect("Invalid UTF-8 output");

    assert_eq!(stdout, expected_output);
}

#[test]
fn test_hello() {
    let air = r#"
    (def pub main do:
      (log "Hello, World!")
    )
    "#;

    let stdout = "Hello, World!\n";

    run(air, stdout);
}

#[test]
fn test_concat() {
    let air = r#"
    (def pub main do:
      %greeting Str = (str-concat "Hello, " "World!")
      (log %greeting)
    )
    "#;

    let stdout = "Hello, World!\n";

    run(air, stdout);
}

#[test]
fn test_greet() {
    let air = r#"
    (def priv func $greet (%name Str) -> Void do:
      %hello Str = (str-concat "Hello, " %name)
      %message Str = (str-concat %hello "!")
      (log %message)
    )

    (def pub main do:
      %name1 Str = "Bradley"
      %_ Void = ($greet %name1)
      %name2 Str = "World"
      ($greet %name2)
    )
    "#;

    let stdout = "Hello, Bradley!\nHello, World!\n";

    run(air, stdout);
}

#[test]
fn test_funcs() {
    let air = r#"
    (def priv func $say-hello (%name Str) -> Void do:
      (log %name)
    )

    (def pub main do:
      %name Str = "Bradley"
      ($say-hello %name)
    )
    "#;

    let stdout = "Bradley\n";

    run(air, stdout);
}

#[test]
fn test_free() {
    let air = r#"
    (def pub main do:
      %greeting Str = (str-concat "Hello, " "World!")
      %_ Void = (log %greeting)
      (free %greeting)
    )
    "#;

    let stdout = "Hello, World!\n";

    run(air, stdout);
}
