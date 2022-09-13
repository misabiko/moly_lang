use moly_lang::ast::Program;
use moly_lang::code::{Instructions, make, Opcode};
use moly_lang::object::Object;
use moly_lang::compiler::{Compiler};
use moly_lang::lexer::Lexer;
use moly_lang::parser::Parser;

struct CompilerTestCase {
	input: &'static str,
	expected_constants: Vec<Object>,
	expected_instructions: Vec<Instructions>,
}

#[test]
fn test_integer_arithmetic() {
	let tests = vec![
		CompilerTestCase {
			input: "1 + 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpAdd, &vec![]),
			],
		}
	];

	run_compiler_tests(tests);
}

fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
	for CompilerTestCase { input, expected_constants, expected_instructions } in tests {
		let program = parse(input);

		let mut compiler = Compiler::new();
		if let Err(err) = compiler.compile(program) {
			panic!("compiler error: {:?}", err)
		}

		let bytecode = compiler.bytecode();

		test_instructions(expected_instructions, bytecode.instructions);

		test_constants(expected_constants, bytecode.constants);
	}
}

fn parse(input: &str) -> Program {
	Parser::new(Lexer::new(input)).parse_program()
}

fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
	let concatted: Instructions = expected.into_iter().flatten().collect();

	assert_eq!(actual.len(), concatted.len(), "wrong instructions length");

	for (i, (ins, actual)) in concatted.into_iter().zip(actual.into_iter()).enumerate() {
		assert_eq!(ins, actual, "wrong instruction at {}", i)
	}
}

fn test_constants(expected: Vec<Object>, actual: Vec<Object>) {
	assert_eq!(expected.len(), actual.len(), "wrong number of constants");

	for (constant, actual) in expected.into_iter().zip(actual.into_iter()) {
		match constant {
			Object::Integer(value) => test_integer_object(value, actual)
		}
	}
}

fn test_integer_object(expected: i64, actual: Object) {
	if let Object::Integer(value) = actual {
		assert_eq!(value, expected)
	} else {
		panic!("{:?} is not Integer", actual)
	}
}