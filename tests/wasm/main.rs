use std::process::{Command};
use moly::lexer::Lexer;
use moly::parser::Parser;
use moly::token::TokenType;
use moly::type_checker::type_env::TypeExpr;
use moly::type_checker::TypeChecker;
use moly::type_checker::typed_ast::TypedStatementBlock;
use moly::wasm::emitter::compile_block_with_header;

#[test]
fn test_empty_program() {
	let wasm = compile_block_with_header(TypedStatementBlock {
		statements: vec![],
		return_type: TypeExpr::Void
	}).unwrap();
	assert_eq!(wasm, [
		//header
		0, 97, 115, 109,
		//module version
		1, 0, 0, 0,
		//types
		1, 8, 2, 96, 0, 0, 96, 1, 125, 0,
		//imports
		2, 13, 1, 3, 101, 110, 118, 5, 112, 114, 105, 110, 116, 0, 1,
		//functions
		3, 2, 1, 0,
		//exports
		7, 7, 1, 3, 114, 117, 110, 0, 1,
		//code
		10, 4, 1, 2, 0, 11
	]);
}

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
			"print(8); print(43);",
			"[ 8, 43 ]"
		),
	];

	for (i, (input, expected)) in tests.into_iter().enumerate() {
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
}