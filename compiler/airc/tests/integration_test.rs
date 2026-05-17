use std::process::Command;
use tempfile::TempDir;

fn run(air_source: &str, expected_output: &str) {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let output_path = temp_dir.path().join("test-program");
    let runtime_lib = std::env::var("SUPPORT_PATH").unwrap();
    let support_path = std::path::Path::new(&runtime_lib).parent().unwrap();

    airc::compile_air(
        air_source,
        "<test>",
        output_path.to_str().unwrap(),
        support_path.to_str().unwrap(),
    )
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
      (:log "Hello, World!")
    )
    "#;

    run(air, "Hello, World!\n");
}

#[test]
fn test_concat() {
    let air = r#"
    (def pub main do:
      $greeting = (:str/concat "Hello, " "World!")
      _ = (:log $greeting)
      (:str/free $greeting)
    )
    "#;

    run(air, "Hello, World!\n");
}

#[test]
fn test_greet() {
    let air = r#"
    (def priv func %greet ($name :str) -> :void do:
      $hello = (:str/concat "Hello, " $name)
      $message = (:str/concat $hello "!")
      _ = (:str/free $hello)
      _ = (:log $message)
      (:str/free $message)
    )

    (def pub main do:
      $name-1 = "Bradley"
      _ = (%greet $name-1)
      $name-2 = "World"
      (%greet $name-2)
    )
    "#;

    run(air, "Hello, Bradley!\nHello, World!\n");
}

#[test]
fn test_int_arithmetic() {
    let air = r#"
    (def priv func %double ($n :int) -> :int do:
      (:int/mul $n 2)
    )

    (def pub main do:
      $a = 10
      $b = 32
      $c = (:int/add $a $b)
      $d = (%double $c)
      $s = (:int/as-str $d)
      _ = (:log $s)
      (:str/free $s)
    )
    "#;

    run(air, "84\n");
}

#[test]
fn test_int_comparison() {
    let air = r#"
    (def priv func %is-positive ($n :int) -> :bool do:
      (:int/gt $n 0)
    )

    (def pub main do:
      $pos = (%is-positive 5)
      $s = (:bool/as-str $pos)
      _ = (:log $s)
      (:str/free $s)
    )
    "#;

    run(air, "true\n");
}

#[test]
fn test_if_expression() {
    let air = r#"
    (def priv func %abs ($n :int) -> :int do:
      $is-neg = (:int/lt $n 0)
      $result = (if $is-neg do: (:int/mul $n -1) else: $n)
      $result
    )

    (def pub main do:
      $a = (%abs 42)
      $b = (%abs -7)
      $sa = (:int/as-str $a)
      $sb = (:int/as-str $b)
      _ = (:log $sa)
      _ = (:log $sb)
      _ = (:str/free $sa)
      (:str/free $sb)
    )
    "#;

    run(air, "42\n7\n");
}

#[test]
fn test_struct() {
    let air = r#"
    (def pub struct #point :int :int)

    (def pub main do:
      $p = (#point 3 4)
      $x = $p.0
      $y = $p.1
      $sum = (:int/add $x $y)
      $s = (:int/as-str $sum)
      _ = (:log $s)
      (:str/free $s)
    )
    "#;
    run(air, "7\n");
}

#[test]
fn test_if_as_result() {
    let air = r#"
    (def priv func %max ($a :int $b :int) -> :int do:
      $gt = (:int/gt $a $b)
      (if $gt do: $a else: $b)
    )

    (def pub main do:
      $m = (%max 10 20)
      $s = (:int/as-str $m)
      _ = (:log $s)
      (:str/free $s)
    )
    "#;

    run(air, "20\n");
}

#[test]
fn test_if_with_block_assigns() {
    let air = r#"
    (def priv func %sign ($n :int) -> :int do:
      $pos = (:int/gt $n 0)
      (if $pos do:
        $one = 1
        $one
      else:
        $neg = -1
        $neg)
    )

    (def pub main do:
      $a = (%sign 5)
      $b = (%sign -3)
      $sa = (:int/as-str $a)
      $sb = (:int/as-str $b)
      _ = (:log $sa)
      _ = (:log $sb)
      _ = (:str/free $sa)
      (:str/free $sb)
    )
    "#;

    run(air, "1\n-1\n");
}

#[test]
fn test_function_pointer() {
    let air = r#"
    (def priv func %double ($n :int) -> :int do:
      (:int/mul $n 2)
    )

    (def priv func %triple ($n :int) -> :int do:
      (:int/mul $n 3)
    )

    (def pub main do:
      $f = (if true do: %double else: %triple)
      $result = ($f 10)
      $s = (:int/as-str $result)
      _ = (:log $s)
      (:str/free $s)
    )
    "#;

    run(air, "20\n");
}
