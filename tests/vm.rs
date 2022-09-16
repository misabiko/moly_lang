use std::collections::HashMap;
use moly_lang::ast::Program;
use moly_lang::compiler::Compiler;
use moly_lang::lexer::Lexer;
use moly_lang::object::{HashingObject, Object};
use moly_lang::parser::Parser;
use moly_lang::vm::VM;

struct VMTestCase {
	input: &'static str,
	expected: Option<Object>,
}

#[test]
fn test_integer_arithmetic() {
	let tests = vec![
		VMTestCase { input: "1", expected: Some(Object::Integer(1)) },
		VMTestCase { input: "2", expected: Some(Object::Integer(2)) },
		VMTestCase { input: "1 + 2", expected: Some(Object::Integer(3)) },
		VMTestCase { input: "1 - 2", expected: Some(Object::Integer(-1)) },
		VMTestCase { input: "1 * 2", expected: Some(Object::Integer(2)) },
		VMTestCase { input: "4 / 2", expected: Some(Object::Integer(2)) },
		VMTestCase { input: "50 / 2 * 2 + 10 - 5", expected: Some(Object::Integer(55)) },
		VMTestCase { input: "5 + 5 + 5 + 5 - 10", expected: Some(Object::Integer(10)) },
		VMTestCase { input: "2 * 2 * 2 * 2 * 2", expected: Some(Object::Integer(32)) },
		VMTestCase { input: "5 * 2 + 10", expected: Some(Object::Integer(20)) },
		VMTestCase { input: "5 + 2 * 10", expected: Some(Object::Integer(25)) },
		VMTestCase { input: "5 * (2 + 10)", expected: Some(Object::Integer(60)) },
		VMTestCase { input: "-5", expected: Some(Object::Integer(-5)) },
		VMTestCase { input: "-10", expected: Some(Object::Integer(-10)) },
		VMTestCase { input: "-50 + 100 + -50", expected: Some(Object::Integer(0)) },
		VMTestCase { input: "(5 + 10 * 2 + 15 / 3) * 2 + -10", expected: Some(Object::Integer(50)) },
	];

	run_vm_tests(tests);
}

#[test]
fn test_boolean_expressions() {
	let tests = vec![
		VMTestCase { input: "true", expected: Some(Object::Boolean(true)) },
		VMTestCase { input: "false", expected: Some(Object::Boolean(false)) },
		VMTestCase { input: "1 < 2", expected: Some(Object::Boolean(true)) },
		VMTestCase { input: "1 > 2", expected: Some(Object::Boolean(false)) },
		VMTestCase { input: "1 < 1", expected: Some(Object::Boolean(false)) },
		VMTestCase { input: "1 > 1", expected: Some(Object::Boolean(false)) },
		VMTestCase { input: "1 == 1", expected: Some(Object::Boolean(true)) },
		VMTestCase { input: "1 != 1", expected: Some(Object::Boolean(false)) },
		VMTestCase { input: "1 == 2", expected: Some(Object::Boolean(false)) },
		VMTestCase { input: "1 != 2", expected: Some(Object::Boolean(true)) },
		VMTestCase { input: "true == true", expected: Some(Object::Boolean(true)) },
		VMTestCase { input: "false == false", expected: Some(Object::Boolean(true)) },
		VMTestCase { input: "true == false", expected: Some(Object::Boolean(false)) },
		VMTestCase { input: "true != false", expected: Some(Object::Boolean(true)) },
		VMTestCase { input: "false != true", expected: Some(Object::Boolean(true)) },
		VMTestCase { input: "(1 < 2) == true", expected: Some(Object::Boolean(true)) },
		VMTestCase { input: "(1 < 2) == false", expected: Some(Object::Boolean(false)) },
		VMTestCase { input: "(1 > 2) == true", expected: Some(Object::Boolean(false)) },
		VMTestCase { input: "(1 > 2) == false", expected: Some(Object::Boolean(true)) },
		VMTestCase { input: "!true", expected: Some(Object::Boolean(false)) },
		VMTestCase { input: "!false", expected: Some(Object::Boolean(true)) },
		//TODO Test error VMTestCase { input: "!5", expected: Some(Object::Boolean(false)) },
		VMTestCase { input: "!!true", expected: Some(Object::Boolean(true)) },
		VMTestCase { input: "!!false", expected: Some(Object::Boolean(false)) },
		//TODO Test error VMTestCase { input: "!!5", expected: Some(Object::Boolean(true)) },
	];

	run_vm_tests(tests)
}

#[test]
fn test_conditionals() {
	let tests = vec![
		VMTestCase { input: "if (true) { 10 }", expected: Some(Object::Integer(10)) },
		VMTestCase { input: "if (true) { 10 } else { 20 }", expected: Some(Object::Integer(10)) },
		VMTestCase { input: "if (false) { 10 } else { 20 } ", expected: Some(Object::Integer(20)) },
		VMTestCase { input: "if (1) { 10 }", expected: Some(Object::Integer(10)) },
		VMTestCase { input: "if (1 < 2) { 10 }", expected: Some(Object::Integer(10)) },
		VMTestCase { input: "if (1 < 2) { 10 } else { 20 }", expected: Some(Object::Integer(10)) },
		VMTestCase { input: "if (1 > 2) { 10 } else { 20 }", expected: Some(Object::Integer(20)) },
	];

	run_vm_tests(tests)
}

#[test]
fn test_global_let_statements() {
	let tests = vec![
		VMTestCase { input: "let one = 1; one", expected: Some(Object::Integer(1)) },
		VMTestCase { input: "let one = 1; let two = 2; one + two", expected: Some(Object::Integer(3)) },
		VMTestCase { input: "let one = 1; let two = one + one; one + two", expected: Some(Object::Integer(3)) },
	];

	run_vm_tests(tests)
}

#[test]
fn test_string_expressions() {
	let tests = vec![
		VMTestCase { input: r#""monkey""#, expected: Some(Object::String("monkey".into())) },
		VMTestCase { input: r#""mon" + "key""#, expected: Some(Object::String("monkey".into())) },
		VMTestCase { input: r#""mon" + "key" + "banana""#, expected: Some(Object::String("monkeybanana".into())) },
	];

	run_vm_tests(tests)
}

#[test]
fn test_array_literals() {
	let tests = vec![
		VMTestCase { input: "[]", expected: Some(Object::Array(vec![])) },
		VMTestCase { input: "[1, 2, 3]", expected: Some(Object::Array(vec![
			Object::Integer(1),
			Object::Integer(2),
			Object::Integer(3),
		])) },
		VMTestCase { input: "[1 + 2, 3 * 4, 5 + 6]", expected: Some(Object::Array(vec![
			Object::Integer(3),
			Object::Integer(12),
			Object::Integer(11),
		])) },
	];

	run_vm_tests(tests)
}

//TODO What happens if {1 + 2: 0, 4 - 1: 0, 3: 0}
#[test]
fn test_hash_literals() {
	let tests = vec![
		VMTestCase { input: "{}", expected: Some(Object::Hash(HashMap::new()))},
		VMTestCase {
			input: "{1: 2, 2: 3}",
			expected: Some(Object::Hash(HashMap::from([
				(HashingObject::Integer(1), (HashingObject::Integer(1), Object::Integer(2))),
				(HashingObject::Integer(2), (HashingObject::Integer(2), Object::Integer(3))),
			])))
		},
		VMTestCase {
			input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
			expected: Some(Object::Hash(HashMap::from([
				(HashingObject::Integer(2), (HashingObject::Integer(2), Object::Integer(4))),
				(HashingObject::Integer(6), (HashingObject::Integer(6), Object::Integer(16))),
			])))
		},
	];

	run_vm_tests(tests)
}

#[test]
fn test_index_expressions() {
	let tests = vec![
		VMTestCase { input: "[1, 2, 3][1]", expected: Some(Object::Integer(2)) },
		VMTestCase { input: "[1, 2, 3][0 + 2]", expected: Some(Object::Integer(3)) },
		VMTestCase { input: "[[1, 1, 1]][0][0]", expected: Some(Object::Integer(1)) },
		//TODO Test run time error
		//VMTestCase { input: "[][0]", expected: Some(Object::Integer(Null)) },
		//VMTestCase { input: "[1, 2, 3][99]", expected: Some(Object::Integer(Null)) },
		//VMTestCase { input: "[1][-1]", expected: Some(Object::Integer(Null)) },
		VMTestCase { input: "{1: 1, 2: 2}[1]", expected: Some(Object::Integer(1)) },
		VMTestCase { input: "{1: 1, 2: 2}[2]", expected: Some(Object::Integer(2)) },
		//VMTestCase { input: "{1: 1}[0]", expected: Some(Object::Integer(Null)) },
		//VMTestCase { input: "{}[0]", expected: Some(Object::Integer(Null)) },
	];

	run_vm_tests(tests)
}

#[test]
fn test_calling_functions_without_arguments() {
	let tests = vec![
		VMTestCase {
			input: "
			let fivePlusTen = fn() { 5 + 10; };
			fivePlusTen();
			",
			expected: Some(Object::Integer(15)),
		},
		VMTestCase {
			input: "
			let one = fn() { 1; };
			let two = fn() { 2; };
			one() + two()
			",
			expected: Some(Object::Integer(3)),
		},
		VMTestCase {
			input: "
			let a = fn() { 1 };
			let b = fn() { a() + 1 };
			let c = fn() { b() + 1 };
			c();
			",
			expected: Some(Object::Integer(3)),
		},
	];

	run_vm_tests(tests)
}

#[test]
fn test_calling_functions_with_return_statement() {
	let tests = vec![
		VMTestCase {
			input: "
			let earlyExit = fn() { return 99; 100; };
			earlyExit();
			",
			expected: Some(Object::Integer(99)),
		},
		VMTestCase {
			input: "
			let earlyExit = fn() { return 99; return 100; };
			earlyExit();
			",
			expected: Some(Object::Integer(99)),
		},
	];

	run_vm_tests(tests)
}

//Might want to make it easier to check no value return
#[test]
fn test_functions_without_return_value() {
	let tests = vec![
		VMTestCase {
			input: "
			let noReturn = fn() { };
			noReturn();
			",
			expected: None,
		},
		VMTestCase {
			input: "
			let noReturn = fn() { };
			let noReturnTwo = fn() { noReturn(); };
			noReturn();
			noReturnTwo();
			",
			expected: Some(Object::Function(vec![15, 0, 0, 20, 21])),
		},
	];

	run_vm_tests(tests)
}

#[test]
fn test_first_class_functions() {
	let tests = vec![
		VMTestCase {
			input: "
			let returnsOne = fn() { 1; };
			let returnsOneReturner = fn() { returnsOne; };
			returnsOneReturner()();
			",
			expected: Some(Object::Integer(1)),
		},
	];

	run_vm_tests(tests)
}

fn run_vm_tests(tests: Vec<VMTestCase>) {
	for VMTestCase { input, expected } in tests {
		let program = parse(input);

		let mut compiler = Compiler::new();
		if let Err(err) = compiler.compile(program) {
			panic!("compiler error: {}", err)
		}

		let bytecode = compiler.bytecode();
		//println!("{}", instruction_to_string(&bytecode.instructions));

		let mut vm = VM::new(bytecode);
		if let Err(err) = vm.run() {
			panic!("vm error: {}", err)
		}

		let stack_elem = vm.last_popped_stack_elem;

		test_expected_object(expected, stack_elem);
	}
}

fn test_expected_object(expected: Option<Object>, actual: Option<Object>) {
	match expected {
		None => assert!(actual.is_none(), "{:?} should be None", actual),
		Some(expected) => match (expected, actual) {
			(_, None) => panic!("missing last popped stack element"),
			(Object::Integer(value), Some(actual)) => test_integer_object(value, actual),
			(Object::Boolean(value), Some(actual)) => test_boolean_object(value, actual),
			(Object::String(value), Some(actual)) => test_string_object(value, actual),
			(Object::Array(value), Some(actual)) => test_array_object(value, actual),
			(Object::Hash(value), Some(actual)) => test_hash_object(value, actual),
			(Object::Function(_), _) => {},	//TODO Check that never call this
		}
	}
}

fn test_integer_object(expected: i64, actual: Object) {
	if let Object::Integer(value) = actual {
		assert_eq!(expected, value);
	} else {
		panic!("{:?} is not Integer", actual);
	}
}

fn test_boolean_object(expected: bool, actual: Object) {
	if let Object::Boolean(value) = actual {
		assert_eq!(expected, value);
	} else {
		panic!("{:?} is not Boolean", actual);
	}
}

fn test_string_object(expected: String, actual: Object) {
	if let Object::String(value) = actual {
		assert_eq!(expected, value);
	} else {
		panic!("{:?} is not String", actual);
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

fn parse(input: &str) -> Program {
	Parser::new(Lexer::new(input)).parse_program()
}