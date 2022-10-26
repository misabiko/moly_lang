use std::process::{Command};
use moly::lexer::Lexer;
use moly::parser::Parser;
use moly::token::TokenType;
use moly::type_checker::TypeChecker;
use moly::wasm::compile_block_with_header;

#[test]
fn test_print() {
	let tests = vec![
		(
			"",
			"[]\n",
		),
		(
			"print(43)",
			"[ 43 ]\n",
		),
		(
			"print(8); print(300); print(70000); ",
			"[ 8, 300, 70000 ]\n"
		),
		(
			"print(-8); print(-300); print(-70000); ",
			"[ -8, -300, -70000 ]\n"
		),
		(
			"printf(22.5)",
			"[ 22.5 ]\n",
		),
	];

	for (i, (input, expected)) in tests.into_iter().enumerate() {
		run_test(i, input, expected, "tests/wasm/executeWasm.ts");
	}
}

#[test]
fn test_infix() {
	let tests = vec![
		(
			"print(2 + 4)",
			"[ 6 ]\n",
		),
		(
			"print((6 - 4) + 10)",
			"[ 12 ]\n",
		),
	];

	for (i, (input, expected)) in tests.into_iter().enumerate() {
		run_test(i, input, expected, "tests/wasm/executeWasm.ts");
	}
}

#[test]
fn test_variables() {
	let tests = vec![
		(
			"let f = 22; print(f);",
			"[ 22 ]\n",
		),
		(
			"let f = 22.5; printf(f);",
			"[ 22.5 ]\n",
		),
		(
			"let f = 22; f = (f+1); print(f);",
			"[ 23 ]\n",
		),
		(
			"let f = 22.5; f = (f+1.5); printf(f);",
			"[ 24 ]\n",
		),
	];

	for (i, (input, expected)) in tests.into_iter().enumerate() {
		run_test(i, input, expected, "tests/wasm/executeWasm.ts");
	}
}

#[test]
fn test_while() {
	let tests = vec![
		(
			"let f = 0; while f < 5 {f = (f + 1); print(f);}",
			"[ 1, 2, 3, 4, 5 ]\n",
		),
	];

	for (i, (input, expected)) in tests.into_iter().enumerate() {
		run_test(i, input, expected, "tests/wasm/executeWasm.ts");
	}
}

#[test]
fn test_graphics() {
	let tests = vec![
		(
			"setpixel(1.0, 2.0, 3.0)",
			"[]\n[ [ 201, 3 ] ]\n",
		),
	];

	for (i, (input, expected)) in tests.into_iter().enumerate() {
		run_test(i, input, expected, "tests/wasm/executeGraphicsWasm.ts");
	}
}

fn run_test(i: usize, input: &'static str, expected: &'static str, executer: &'static str) {
	let program = Parser::new(Lexer::new(input)).parse_block_statement(TokenType::EOF);
	let program = match program {
		Ok(p) => p,
		Err(err) => panic!("test {}: parse error: {}", i, err),
	};

	let mut type_checker = TypeChecker::new();
	let program = match type_checker.check_block(program, true, false) {
		Ok(program) => program,
		Err(err) => panic!("test {}: type checking error: {:?}", i, err),
	};

	let bytecode = compile_block_with_header(program).unwrap();
	let bytecode_str = bytecode.iter()
		.map(|b| format!("{:#x}", b))
		.collect::<Vec<String>>()
		.join(" ");

	// println!("{:?}", bytecode);

	let cmd_output = Command::new("deno")
		.args(["run", executer, &bytecode_str])
		.output()
		.expect("failed to execute command");

	//println!("{:#?}", cmd_output);

	assert!(cmd_output.status.success(), "failed to execute wasm:\n{}\n\nWAT\n{}",
			std::str::from_utf8(cmd_output.stderr.as_slice()).unwrap(),
			wabt::wasm2wat(bytecode.clone()).unwrap(),
	);

	assert_eq!(
		std::str::from_utf8(cmd_output.stdout.as_slice()).unwrap(),
		expected,
		"output mismatched\n\nWAT:\n{}",
		wabt::wasm2wat(bytecode).unwrap(),
	);
}