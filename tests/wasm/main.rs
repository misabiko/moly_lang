use std::process::{Command};
use moly_lib::lexer::Lexer;
use moly_lib::parser::Parser;
use moly_lib::token::TokenType;
use moly_lib::type_checker::TypeChecker;
use moly_lib::wasm::compile_block_with_header;

#[test]
fn test_print() {
	let tests = vec![
		(
			"",
			"[]\n",
		),
		(
			"printI32(43)",
			"[ 43 ]\n",
		),
		(
			"printI32(8); printI32(300); printI32(70000); ",
			"[ 8, 300, 70000 ]\n"
		),
		(
			"printI32(-8); printI32(-300); printI32(-70000); ",
			"[ -8, -300, -70000 ]\n"
		),
		(
			"printF32(22.5)",
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
			"printI32(2 + 4)",
			"[ 6 ]\n",
		),
		(
			"printI32((6 - 4) + 10)",
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
			"let f = 22; printI32(f);",
			"[ 22 ]\n",
		),
		(
			"let f = 22.5; printF32(f);",
			"[ 22.5 ]\n",
		),
		(
			"let f = 22; f = (f+1); printI32(f);",
			"[ 23 ]\n",
		),
		(
			"let f = 22.5; f = (f+1.5); printF32(f);",
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
			"let f = 0; while f < 5 {f = (f + 1); printI32(f);}",
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
			//Would need to get 1000+ length array from stdout "[]\n[ [ 201, 3 ] ]\n",
			"[]\n[]\n",
		),
		(
			"let y = 0.0; while y < 5.0 { y = y + 1.0; let x = 0.0; while x < 1.0 { x = x + 1.0; setpixel(50.0, 51.0, 100.0);}}",
			"[]\n[]\n",
		),
		(
			"let y = 0.0;
			while y < 100.0 {
				y = y + 1.0;
				let x = 0.0;
				while x < 100.0 {
					x = x + 1.0;

					let e = (y / 50.0) - 1.5;
					let f = (x / 50.0) - 1.0;

					let a = 0.0;
					let b = 0.0;
					let i = 0.0;
					let j = 0.0;
					let c = 0.0;

					while (((i * i) + (j * j)) < 4.0) && (c < 255.0) {
						i = ((a * a) - (b * b)) + e;
						j = ((2.0 * a) * b) + f;
						a = i;
						b = j;
						c = c + 1.0;
					}
					setpixel(x, y, c);
				}
			}",
			"[]\n[]\n",
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
	// println!("{}", wasmprinter::print_bytes(bytecode.clone()).unwrap());
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

	assert!(cmd_output.status.success(), "{}: failed to execute wasm:\n{}\n\nWAT\n{}",
			i,
			std::str::from_utf8(cmd_output.stderr.as_slice()).unwrap(),
			wasmprinter::print_bytes(bytecode.clone()).unwrap(),
	);

	assert_eq!(
		std::str::from_utf8(cmd_output.stdout.as_slice()).unwrap(),
		expected,
		"output mismatched\n\nWAT:\n{}",
		wasmprinter::print_bytes(bytecode).unwrap(),
	);
}