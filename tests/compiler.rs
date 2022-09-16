use std::collections::HashMap;
use moly_lang::ast::Program;
use moly_lang::code::{concat_instructions, instruction_to_string, Instructions, make, Opcode};
use moly_lang::object::{HashingObject, Object};
use moly_lang::compiler::{Compiler, EmittedInstruction};
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
				make(Opcode::OpJumpIfFalse, &vec![10]),
				// 0004
				make(Opcode::OpConstant, &vec![0]),
				// 0007
				make(Opcode::OpJump, &vec![10]),
				// 0010
				make(Opcode::OpPop, &vec![]),
				// 0011
				make(Opcode::OpConstant, &vec![1]),
				// 0013
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

#[test]
fn test_hash_literals() {
	let tests = vec![
		CompilerTestCase {
			input: "{}",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::OpHash, &vec![0]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "{1: 2, 3: 4, 5: 6}",
			expected_constants: vec![1, 2, 3, 4, 5, 6].into_iter()
				.map(|i| Object::Integer(i)).collect(),
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpConstant, &vec![2]),
				make(Opcode::OpConstant, &vec![3]),
				make(Opcode::OpConstant, &vec![4]),
				make(Opcode::OpConstant, &vec![5]),
				make(Opcode::OpHash, &vec![6]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "{1: 2 + 3, 4: 5 * 6}",
			expected_constants: vec![1, 2, 3, 4, 5, 6].into_iter()
				.map(|i| Object::Integer(i)).collect(),
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpConstant, &vec![2]),
				make(Opcode::OpAdd, &vec![]),
				make(Opcode::OpConstant, &vec![3]),
				make(Opcode::OpConstant, &vec![4]),
				make(Opcode::OpConstant, &vec![5]),
				make(Opcode::OpMul, &vec![]),
				make(Opcode::OpHash, &vec![4]),
				make(Opcode::OpPop, &vec![]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_index_expressions() {
	let tests = vec![
		CompilerTestCase {
			input: "[1, 2, 3][1 + 1]",
			expected_constants: vec![1, 2, 3, 1, 1].into_iter()
				.map(|i| Object::Integer(i)).collect(),
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpConstant, &vec![2]),
				make(Opcode::OpArray, &vec![3]),
				make(Opcode::OpConstant, &vec![3]),
				make(Opcode::OpConstant, &vec![4]),
				make(Opcode::OpAdd, &vec![]),
				make(Opcode::OpIndex, &vec![]),
				make(Opcode::OpPop, &vec![]),
			]
		},
		CompilerTestCase {
			input: "{1: 2}[2 - 1]",
			expected_constants: vec![1, 2, 2, 1].into_iter()
				.map(|i| Object::Integer(i)).collect(),
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpConstant, &vec![1]),
				make(Opcode::OpHash, &vec![2]),
				make(Opcode::OpConstant, &vec![2]),
				make(Opcode::OpConstant, &vec![3]),
				make(Opcode::OpSub, &vec![]),
				make(Opcode::OpIndex, &vec![]),
				make(Opcode::OpPop, &vec![]),
			]
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_functions() {
	let tests = vec![
		CompilerTestCase {
			input: "fn() { return 5 + 10 }",
			expected_constants: vec![
				Object::Integer(5),
				Object::Integer(10),
				Object::Function(concat_instructions(vec![
					make(Opcode::OpConstant, &vec![0]),
					make(Opcode::OpConstant, &vec![1]),
					make(Opcode::OpAdd, &vec![]),
					make(Opcode::OpReturnValue, &vec![]),
				])),
			],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![2]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "fn() { 5 + 10 }",
			expected_constants: vec![
				Object::Integer(5),
				Object::Integer(10),
				Object::Function(concat_instructions(vec![
					make(Opcode::OpConstant, &vec![0]),
					make(Opcode::OpConstant, &vec![1]),
					make(Opcode::OpAdd, &vec![]),
					make(Opcode::OpReturnValue, &vec![]),
				])),
			],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![2]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "fn() { 1; 2 }",
			expected_constants: vec![
				Object::Integer(1),
				Object::Integer(2),
				Object::Function(concat_instructions(vec![
					make(Opcode::OpConstant, &vec![0]),
					make(Opcode::OpPop, &vec![]),
					make(Opcode::OpConstant, &vec![1]),
					make(Opcode::OpReturnValue, &vec![]),
				])),
			],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![2]),
				make(Opcode::OpPop, &vec![]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_functions_without_return_value() {
	let tests = vec![
		CompilerTestCase {
			input: "fn() { }",
			expected_constants: vec![
				Object::Function(concat_instructions(vec![
					make(Opcode::OpReturn, &vec![]),
				]))
			],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![0]),
				make(Opcode::OpPop, &vec![]),
			]
		}
	];

	run_compiler_tests(tests)
}

#[test]
fn test_compiler_scopes() {
	let mut compiler = Compiler::new();
	assert_eq!(compiler.scope_index, 0, "scope_index wrong");

	compiler.emit(Opcode::OpMul, vec![]);

	compiler.enter_scope();
	assert_eq!(compiler.scope_index, 1, "scope_index wrong");

	compiler.emit(Opcode::OpSub, vec![]);

	assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 1, "instructions length wrong");

	let last = &compiler.scopes.get(compiler.scope_index).unwrap().last_instruction;
	assert!(matches!(last, Some(EmittedInstruction { opcode: Opcode::OpSub, .. })), "last_instruction wrong");

	compiler.leave_scope();
	assert_eq!(compiler.scope_index, 0, "scope_index wrong");

	compiler.emit(Opcode::OpAdd, vec![]);

	assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 2, "instructions length wrong");

	let last = &compiler.scopes.get(compiler.scope_index).unwrap().last_instruction;
	assert!(matches!(last, Some(EmittedInstruction { opcode: Opcode::OpAdd, .. })), "last_instruction wrong");

	let previous = &compiler.scopes.get(compiler.scope_index).unwrap().previous_instruction;
	assert!(matches!(previous, Some(EmittedInstruction { opcode: Opcode::OpMul, .. })), "previous_instruction wrong");
}

#[test]
fn test_function_calls() {
	let tests = vec![
		CompilerTestCase {
			input: "fn() { 24 }();",
			expected_constants: vec![
				Object::Integer(24),
				Object::Function(concat_instructions(vec![
					make(Opcode::OpConstant, &vec![0]), // The literal "24"
					make(Opcode::OpReturnValue, &vec![]),
				]))
			],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![1]), // The compiled function
				make(Opcode::OpCall, &vec![]),
				make(Opcode::OpPop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "
            let noArg = fn() { 24 };
            noArg();
            ",
			expected_constants: vec![
				Object::Integer(24),
				Object::Function(concat_instructions(vec![
					make(Opcode::OpConstant, &vec![0]), // The literal "24"
					make(Opcode::OpReturnValue, &vec![]),
				]))
			],
			expected_instructions: vec![
				make(Opcode::OpConstant, &vec![1]), // The compiled function
				//TODO Optimize unused variables?
				make(Opcode::OpSetGlobal, &vec![0]),
				make(Opcode::OpGetGlobal, &vec![0]),
				make(Opcode::OpCall, &vec![]),
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
	let concatted = concat_instructions(expected);

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
			Object::Integer(v) => test_integer_object(v, actual),
			Object::Boolean(v) => test_boolean_object(v, actual),
			Object::String(v) => test_string_object(v, actual),
			Object::Array(v) => test_array_object(v, actual),
			Object::Hash(v) => test_hash_object(v, actual),
			Object::Function(v) => test_function_object(v, actual),
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

fn test_hash_object(expected: HashMap<HashingObject, (HashingObject, Object)>, actual: Object) {
	if let Object::Hash(mut pairs) = actual {
		assert_eq!(expected.len(), pairs.len(), "wrong num of pairs");

		for (expected_k, expected_v) in expected {
			if let Some((_, actual_v)) = pairs.remove(&expected_k) {

				if let (_, Object::Integer(expected_v)) = expected_v {
					test_integer_object(expected_v, actual_v)
				}
			}else {
				panic!("no pair for given key {:?} in pair {:?}", expected_k, pairs)
			}
		}
	} else {
		panic!("{:?} is not Hash", actual);
	}
}

fn test_function_object(expected: Instructions, actual: Object) {
	if let Object::Function(instructions) = actual {
		test_instructions(vec![expected], instructions);
	} else {
		panic!("{:?} is not Function", actual);
	}
}