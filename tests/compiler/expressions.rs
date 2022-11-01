use moly_lib::code::{make, Opcode};
use moly_lib::object::Object;
use crate::{run_compiler_tests, TestCase};

#[test]
fn test_integer_arithmetic() {
	let tests = vec![
		TestCase {
			input: "1 + 2",
			expected_constants: vec![Object::U8(1), Object::U8(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Add, &[]),
			],
		},
		TestCase {
			input: "1; 2",
			expected_constants: vec![Object::U8(1), Object::U8(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Pop, &[]),
				make(Opcode::Constant, &[1]),
			],
		},
		TestCase {
			input: "1 - 2",
			expected_constants: vec![Object::U8(1), Object::U8(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Sub, &[]),
			],
		},
		TestCase {
			input: "1 * 2",
			expected_constants: vec![Object::U8(1), Object::U8(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Mul, &[]),
			],
		},
		TestCase {
			input: "2 / 1",
			expected_constants: vec![Object::U8(2), Object::U8(1)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Div, &[]),
			],
		},
		TestCase {
			input: "1 + 2",
			expected_constants: vec![Object::U8(1), Object::U8(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Add, &[]),
			],
		},
		TestCase {
			input: "-1",
			expected_constants: vec![Object::U8(1)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Minus, &[]),
			],
		},
	];

	run_compiler_tests(tests, true);
}

#[test]
fn test_boolean_expressions() {
	let tests = vec![
		TestCase {
			input: "true",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::True, &[]),
			],
		},
		TestCase {
			input: "false",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::False, &[]),
			],
		},
		TestCase {
			input: "1 > 2",
			expected_constants: vec![Object::U8(1), Object::U8(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::GreaterThan, &[]),
			],
		},
		TestCase {
			input: "1 < 2",
			expected_constants: vec![Object::U8(2), Object::U8(1)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::GreaterThan, &[]),
			],
		},
		TestCase {
			input: "1 == 2",
			expected_constants: vec![Object::U8(1), Object::U8(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Equal, &[]),
			],
		},
		TestCase {
			input: "1 != 2",
			expected_constants: vec![Object::U8(1), Object::U8(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::NotEqual, &[]),
			],
		},
		TestCase {
			input: "true == false",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::True, &[]),
				make(Opcode::False, &[]),
				make(Opcode::Equal, &[]),
			],
		},
		TestCase {
			input: "true != false",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::True, &[]),
				make(Opcode::False, &[]),
				make(Opcode::NotEqual, &[]),
			],
		},
		TestCase {
			input: "!true",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::True, &[]),
				make(Opcode::Bang, &[]),
			],
		},
	];

	run_compiler_tests(tests, true)
}

#[test]
fn test_string_expressions() {
	let tests = vec![
		TestCase {
			input: r#""monkey"#,
			expected_constants: vec![Object::String("monkey".into())],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
			]
		},
		TestCase {
			input: r#""mon" + "key"#,
			expected_constants: vec![
				Object::String("mon".into()),
				Object::String("key".into()),
			],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Add, &[]),
			]
		},
	];

	run_compiler_tests(tests, true)
}

#[test]
fn test_array_literals() {
	let tests = vec![
		TestCase {
			input: "[1, 2, 3]",
			expected_constants: vec![
				Object::U8(1),
				Object::U8(2),
				Object::U8(3),
			],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Constant, &[2]),
				make(Opcode::Array, &[3]),
			],
		},
		TestCase {
			input: "[1 + 2, 3 - 4, 5 * 6]",
			expected_constants: vec![
				Object::U8(1),
				Object::U8(2),
				Object::U8(3),
				Object::U8(4),
				Object::U8(5),
				Object::U8(6),
			],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Add, &[]),
				make(Opcode::Constant, &[2]),
				make(Opcode::Constant, &[3]),
				make(Opcode::Sub, &[]),
				make(Opcode::Constant, &[4]),
				make(Opcode::Constant, &[5]),
				make(Opcode::Mul, &[]),
				make(Opcode::Array, &[3]),
			],
		},
	];

	run_compiler_tests(tests, true)
}