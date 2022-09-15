use moly_lang::ast::Program;
use moly_lang::code::{instruction_to_string, Instructions, make, Opcode};
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
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1; 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpPop, &vec![]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 - 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpSub, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 * 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpMul, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "2 / 1",
			expected_constants: vec![Object::Integer(2), Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpDiv, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 + 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpAdd, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "-1",
			expected_constants: vec![Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpMinus, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
	];

	run_compiler_tests(tests);
}

#[test]
fn test_boolean_expressions() {
	let tests = vec![
		CompilerTestCase {
			input: "true",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::OpTrue, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "false",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::OpFalse, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 > 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpGreaterThan, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 < 2",
			expected_constants: vec![Object::Integer(2), Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpGreaterThan, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 == 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpEqual, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 != 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpNotEqual, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "true == false",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::OpTrue, &vec![]),
				make(Opcode::OpFalse, &vec![]),
				make(Opcode::OpEqual, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "true != false",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::OpTrue, &vec![]),
				make(Opcode::OpFalse, &vec![]),
				make(Opcode::OpNotEqual, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "!true",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::OpTrue, &vec![]),
				make(Opcode::OpBang, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_conditionals() {
	let tests = vec![
		CompilerTestCase {
			input: "
            if (true) { 10 }; 3333;
            ",
			expected_constants: vec![Object::Integer(10), Object::Integer(3333)],
			expected_instructions: vec![
				// 0000
				make(Opcode::OpTrue, &vec![]),
				// 0001
				make(Opcode::OpJumpIfFalse, &vec![7]),
				// 0004
				make(Opcode::OpConstant, &vec![0]),
				// 0007
				make(Opcode::OpPop, &vec![]),
				// 0008
				make(Opcode::OpConstant, &vec![1]),
				// 0011
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "
            if (true) { 10 } else { 20 }; 3333;
            ",
			expected_constants: vec![Object::Integer(10), Object::Integer(20), Object::Integer(3333)],
			expected_instructions: vec![
				// 0000
				make(Opcode::OpTrue, &vec![]),
				// 0001
				make(Opcode::OpJumpIfFalse, &vec![10]),
				// 0004
				make(Opcode::OpConstant, &vec![0]),
				// 0007
				make(Opcode::OpJump, &vec![13]),
				// 0010
				make(Opcode::OpConstant, &vec![1]),
				// 0013
				make(Opcode::OpPop, &vec![]),
				// 0014
				make(Opcode::OpConstant, &vec![2]),
				// 0017
				make(Opcode::OpPop, &vec![]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_global_let_statements() {
	let tests = vec![
		CompilerTestCase {
			input: "
			let one = 1;
			let two = 2;
			",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpSetGlobal, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpSetGlobal, &vec![1]),
			],
		},
		CompilerTestCase {
			input: "
			let one = 1;
			one;
			",
			expected_constants: vec![Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpSetGlobal, &vec![0]),
				make(Opcode::OpGetGlobal, &vec![0]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "
			let one = 1;
			let two = one;
			two;
			",
			expected_constants: vec![Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpSetGlobal, &vec![0]),
				make(Opcode::OpGetGlobal, &vec![0]),
				make(Opcode::OpSetGlobal, &vec![1]),
				make(Opcode::OpGetGlobal, &vec![1]),
				make(Opcode::OpPop, &vec![]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_string_expressions() {
	let tests = vec![
		CompilerTestCase {
			input: r#""monkey"#,
			expected_constants: vec![Object::String("monkey".into())],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpPop, &vec![]),
			]
		},
		CompilerTestCase {
			input: r#""mon" + "key"#,
			expected_constants: vec![
				Object::String("mon".into()),
				Object::String("key".into()),
			],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpAdd, &vec![]),
				make(Opcode::OpPop, &vec![]),
			]
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_array_literals() {
	let tests = vec![
		CompilerTestCase {
			input: "[]",
			expected_constants: vec![],
			expected_instructions: vec![
				//TODO Profile copying operand vec
				make(Opcode::OpArray, &vec![0]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "[1, 2, 3]",
			expected_constants: vec![
				Object::Integer(1),
				Object::Integer(2),
				Object::Integer(3),
			],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpConstant, &vec![2]),
				make(Opcode::OpArray, &vec![3]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "[1 + 2, 3 - 4, 5 * 6]",
			expected_constants: vec![
				Object::Integer(1),
				Object::Integer(2),
				Object::Integer(3),
				Object::Integer(4),
				Object::Integer(5),
				Object::Integer(6),
			],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpAdd, &vec![]),
				make(Opcode::OpConstant, &vec![2]),
				make(Opcode::OpConstant, &vec![3]),
				make(Opcode::OpSub, &vec![]),
				make(Opcode::OpConstant, &vec![4]),
				make(Opcode::OpConstant, &vec![5]),
				make(Opcode::OpMul, &vec![]),
				make(Opcode::OpArray, &vec![3]),
				make(Opcode::OpPop, &vec![]),
			],
		},
	];

	run_compiler_tests(tests)
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

	assert_eq!(instruction_to_string(&actual), instruction_to_string(&concatted));
	/*assert_eq!(actual.len(), concatted.len(), "wrong instructions length");

	for (i, (ins, actual)) in concatted.into_iter().zip(actual.into_iter()).enumerate() {
		assert_eq!(ins, actual, "wrong instruction at {}", i)
	}*/
}

fn test_constants(expected: Vec<Object>, actual: Vec<Object>) {
	assert_eq!(expected.len(), actual.len(), "wrong number of constants");

	for (constant, actual) in expected.into_iter().zip(actual.into_iter()) {
		match constant {
			Object::Integer(value) => test_integer_object(value, actual),
			Object::Boolean(value) => test_boolean_object(value, actual),
			Object::String(value) => test_string_object(value, actual),
			Object::Array(value) => test_array_object(value, actual),
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

fn test_boolean_object(expected: bool, actual: Object) {
	if let Object::Boolean(value) = actual {
		assert_eq!(value, expected)
	} else {
		panic!("{:?} is not Boolean", actual)
	}
}

fn test_string_object(expected: String, actual: Object) {
	if let Object::String(value) = actual {
		assert_eq!(value, expected)
	} else {
		panic!("{:?} is not String", actual)
	}
}

fn test_array_object(expected: Vec<Object>, actual: Object) {
	if let Object::Array(elements) = actual {
		assert_eq!(expected.len(), elements.len(), "wrong num of elements");

		for (expected_el, el) in expected.into_iter().zip(elements.into_iter()) {
			if let Object::Integer(expected_el) = expected_el {
				test_integer_object(expected_el, el);
			}else {
				panic!("{:?} isn't Integer", expected_el);
			}
		}
	} else {
		panic!("{:?} is not Array", actual);
	}
}