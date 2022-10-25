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
			"[]",
		),
		(
			"print(43)",
			"[ 43 ]",
		),
		(
			"print(8); print(300); print(70000); ",
			"[ 8, 300, 70000 ]"
		),
		(
			"print(-8); print(-300); print(-70000); ",
			"[ -8, -300, -70000 ]"
		),
	];

	for (i, (input, expected)) in tests.into_iter().enumerate() {
		run_test(i, input, expected);
	}
}

#[test]
fn test_infix() {
	let tests = vec![
		(
			"print(2 + 4)",
			"[ 6 ]",
		),
		(
			"print((6 - 4) + 10)",
			"[ 12 ]",
		),
	];

	for (i, (input, expected)) in tests.into_iter().enumerate() {
		run_test(i, input, expected);
	}
}

fn run_test(i: usize, input: &'static str, expected: &'static str) {
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
	let bytecode = bytecode.into_iter()
		.map(|b| format!("{:#x}", b))
		.collect::<Vec<String>>()
		.join(" ");

	let cmd_output = Command::new("deno")
		.args(["run", "executeWasm.ts", &bytecode])
		.output()
		.expect("failed to execute command");

	//println!("{:#?}", cmd_output);

	assert!(cmd_output.status.success(), "failed to execute wasm:\n{}", std::str::from_utf8(cmd_output.stderr.as_slice()).unwrap());

	assert_eq!(
		std::str::from_utf8(cmd_output.stdout.as_slice()).unwrap(),
		format!("{}\n", expected)
	);
}