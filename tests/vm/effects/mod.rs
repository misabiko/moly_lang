use moly::compiler::Compiler;
use moly::lexer::Lexer;
use moly::object::Object;
use moly::parser::Parser;
use moly::token::TokenType;
use moly::type_checker::TypeChecker;
use moly::vm::VM;

//TODO test_defer #[test]
#[allow(dead_code)]
fn test_defer() {
	let tests = vec![
		TestCase {
			input: "tests/vm/effects/defer.moly",
			expected: Ok(Some(Object::String(
"<html>
	<head>
	<title>Hello</title>
	</head>
	<body>
		<h1>Hello World!</h1>
	</body>
</html>".into()
			)))
		}
	];

	run_vm_tests(tests, false)
}

struct TestCase {
	input: &'static str,
	expected: Result<Option<Object>, Option<String>>,
}

fn run_vm_tests(tests: Vec<TestCase>, compile_block: bool) {
	for (i, TestCase { input, expected }) in tests.into_iter().enumerate() {
		let input = std::fs::read_to_string(input).expect("failed to read file");

		//println!("{}", input);
		let program = if compile_block {
			Parser::new(Lexer::new(&input)).parse_block_statement(TokenType::EOF)
		}else {
			Parser::new(Lexer::new(&input)).parse_program()
		};
		let program = match program {
			Ok(p) => p,
			Err(err) => panic!("test {}: parse error: {}", i, err),
		};

		let mut type_checker = TypeChecker::new();
		let mut compiler = Compiler::new();
		let compile_result = if compile_block {
			let program = match type_checker.check_block(program, true, false) {
				Ok(program) => program,
				Err(err) => panic!("test {}: type checking error: {:?}", i, err),
			};
			compiler.compile_block(program)
		}else {
			let program = match type_checker.check(program, true) {
				Ok(program) => program,
				Err(err) => panic!("test {}: type checking error: {:?}", i, err),
			};
			compiler.compile(program)
		};
		if let Err(err) = compile_result {
			panic!("test {}: compiler error: {}", i, err)
		}

		let bytecode = compiler.bytecode();
		/*println!("{}", instruction_to_string(&bytecode.instructions));

		for (i, constant) in bytecode.constants.iter().enumerate() {
			println!("CONSTANT {} {:p} ({:?}):", i, constant, constant);

			match constant {
				Object::Function(f) => println!(" Instructions:\n{}", instruction_to_string(&f.instructions)),
				Object::U8(i) => println!(" Value: {}\n", i),
				_ => {}
			}
		}*/

		let mut vm = VM::new(bytecode);
		let vm_result = vm.run();
		let stack_elem = vm.stack_top().cloned();
		match (vm_result, expected) {
			(Err(vm_err), Err(Some(expected_err))) => assert_eq!(vm_err, expected_err),
			(Err(vm_err), Err(None) | Ok(_)) => panic!("vm error: {}", vm_err),
			(Ok(_), Err(Some(expected_err))) => panic!("expected vm error: {}", expected_err),
			(_, Err(None)) => assert!(stack_elem.is_none(), "{:?} should be None", stack_elem),
			(_, Ok(expected)) => assert_eq!(stack_elem, expected)
		}
	}
}