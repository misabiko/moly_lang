use std::collections::HashMap;
use moly_lang::ast::Program;
use moly_lang::compiler::Compiler;
use moly_lang::lexer::Lexer;
use moly_lang::object::{HashingObject, Object};
use moly_lang::parser::Parser;
use moly_lang::vm::VM;

struct VMTestCase {
	input: &'static str,
	expected: Object,
}

#[test]
fn test_integer_arithmetic() {
	let tests = vec![
		VMTestCase { input: "1", expected: Object::Integer(1) },
		VMTestCase { input: "2", expected: Object::Integer(2) },
		VMTestCase { input: "1 + 2", expected: Object::Integer(3) },
		VMTestCase { input: "1 - 2", expected: Object::Integer(-1) },
		VMTestCase { input: "1 * 2", expected: Object::Integer(2) },
		VMTestCase { input: "4 / 2", expected: Object::Integer(2) },
		VMTestCase { input: "50 / 2 * 2 + 10 - 5", expected: Object::Integer(55) },
		VMTestCase { input: "5 + 5 + 5 + 5 - 10", expected: Object::Integer(10) },
		VMTestCase { input: "2 * 2 * 2 * 2 * 2", expected: Object::Integer(32) },
		VMTestCase { input: "5 * 2 + 10", expected: Object::Integer(20) },
		VMTestCase { input: "5 + 2 * 10", expected: Object::Integer(25) },
		VMTestCase { input: "5 * (2 + 10)", expected: Object::Integer(60) },
		VMTestCase { input: "-5", expected: Object::Integer(-5) },
		VMTestCase { input: "-10", expected: Object::Integer(-10) },
		VMTestCase { input: "-50 + 100 + -50", expected: Object::Integer(0) },
		VMTestCase { input: "(5 + 10 * 2 + 15 / 3) * 2 + -10", expected: Object::Integer(50) },
	];

	run_vm_tests(tests);
}

#[test]
fn test_boolean_expressions() {
	let tests = vec![
		VMTestCase { input: "true", expected: Object::Boolean(true) },
		VMTestCase { input: "false", expected: Object::Boolean(false) },
		VMTestCase { input: "1 < 2", expected: Object::Boolean(true) },
		VMTestCase { input: "1 > 2", expected: Object::Boolean(false) },
		VMTestCase { input: "1 < 1", expected: Object::Boolean(false) },
		VMTestCase { input: "1 > 1", expected: Object::Boolean(false) },
		VMTestCase { input: "1 == 1", expected: Object::Boolean(true) },
		VMTestCase { input: "1 != 1", expected: Object::Boolean(false) },
		VMTestCase { input: "1 == 2", expected: Object::Boolean(false) },
		VMTestCase { input: "1 != 2", expected: Object::Boolean(true) },
		VMTestCase { input: "true == true", expected: Object::Boolean(true) },
		VMTestCase { input: "false == false", expected: Object::Boolean(true) },
		VMTestCase { input: "true == false", expected: Object::Boolean(false) },
		VMTestCase { input: "true != false", expected: Object::Boolean(true) },
		VMTestCase { input: "false != true", expected: Object::Boolean(true) },
		VMTestCase { input: "(1 < 2) == true", expected: Object::Boolean(true) },
		VMTestCase { input: "(1 < 2) == false", expected: Object::Boolean(false) },
		VMTestCase { input: "(1 > 2) == true", expected: Object::Boolean(false) },
		VMTestCase { input: "(1 > 2) == false", expected: Object::Boolean(true) },
		VMTestCase { input: "!true", expected: Object::Boolean(false) },
		VMTestCase { input: "!false", expected: Object::Boolean(true) },
		//TODO Test error VMTestCase { input: "!5", expected: Object::Boolean(false) },
		VMTestCase { input: "!!true", expected: Object::Boolean(true) },
		VMTestCase { input: "!!false", expected: Object::Boolean(false) },
		//TODO Test error VMTestCase { input: "!!5", expected: Object::Boolean(true) },
	];

	run_vm_tests(tests)
}

#[test]
fn test_conditionals() {
	let tests = vec![
		VMTestCase { input: "if (true) { 10 }", expected: Object::Integer(10) },
		VMTestCase { input: "if (true) { 10 } else { 20 }", expected: Object::Integer(10) },
		VMTestCase { input: "if (false) { 10 } else { 20 } ", expected: Object::Integer(20) },
		VMTestCase { input: "if (1) { 10 }", expected: Object::Integer(10) },
		VMTestCase { input: "if (1 < 2) { 10 }", expected: Object::Integer(10) },
		VMTestCase { input: "if (1 < 2) { 10 } else { 20 }", expected: Object::Integer(10) },
		VMTestCase { input: "if (1 > 2) { 10 } else { 20 }", expected: Object::Integer(20) },
	];

	run_vm_tests(tests)
}

#[test]
fn test_global_let_statements() {
	let tests = vec![
		VMTestCase { input: "let one = 1; one", expected: Object::Integer(1) },
		VMTestCase { input: "let one = 1; let two = 2; one + two", expected: Object::Integer(3) },
		VMTestCase { input: "let one = 1; let two = one + one; one + two", expected: Object::Integer(3) },
	];

	run_vm_tests(tests)
}

#[test]
fn test_string_expressions() {
	let tests = vec![
		VMTestCase { input: r#""monkey""#, expected: Object::String("monkey".into()) },
		VMTestCase { input: r#""mon" + "key""#, expected: Object::String("monkey".into()) },
		VMTestCase { input: r#""mon" + "key" + "banana""#, expected: Object::String("monkeybanana".into()) },
	];

	run_vm_tests(tests)
}

#[test]
fn test_array_literals() {
	let tests = vec![
		VMTestCase { input: "[]", expected: Object::Array(vec![]) },
		VMTestCase { input: "[1, 2, 3]", expected: Object::Array(vec![
			Object::Integer(1),
			Object::Integer(2),
			Object::Integer(3),
		]) },
		VMTestCase { input: "[1 + 2, 3 * 4, 5 + 6]", expected: Object::Array(vec![
			Object::Integer(3),
			Object::Integer(12),
			Object::Integer(11),
		]) },
	];

	run_vm_tests(tests)
}

//TODO What happens if {1 + 2: 0, 4 - 1: 0, 3: 0}
#[test]
fn test_hash_literals() {
	let tests = vec![
		VMTestCase { input: "{}", expected: Object::Hash(HashMap::new())},
		VMTestCase {
			input: "{1: 2, 2: 3}",
			expected: Object::Hash(HashMap::from([
				(HashingObject::Integer(1), (HashingObject::Integer(1), Object::Integer(2))),
				(HashingObject::Integer(2), (HashingObject::Integer(2), Object::Integer(3))),
			]))
		},
		VMTestCase {
			input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
			expected: Object::Hash(HashMap::from([
				(HashingObject::Integer(2), (HashingObject::Integer(2), Object::Integer(4))),
				(HashingObject::Integer(6), (HashingObject::Integer(6), Object::Integer(16))),
			]))
		},
	];

	run_vm_tests(tests)
}

#[test]
fn test_index_expressions() {
	let tests = vec![
		VMTestCase { input: "[1, 2, 3][1]", expected: Object::Integer(2) },
		VMTestCase { input: "[1, 2, 3][0 + 2]", expected: Object::Integer(3) },
		VMTestCase { input: "[[1, 1, 1]][0][0]", expected: Object::Integer(1) },
		//TODO Test run time error
		//VMTestCase { input: "[][0]", expected: Object::Integer(Null) },
		//VMTestCase { input: "[1, 2, 3][99]", expected: Object::Integer(Null) },
		//VMTestCase { input: "[1][-1]", expected: Object::Integer(Null) },
		VMTestCase { input: "{1: 1, 2: 2}[1]", expected: Object::Integer(1) },
		VMTestCase { input: "{1: 1, 2: 2}[2]", expected: Object::Integer(2) },
		//VMTestCase { input: "{1: 1}[0]", expected: Object::Integer(Null) },
		//VMTestCase { input: "{}[0]", expected: Object::Integer(Null) },
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

		let mut vm = VM::new(bytecode);
		if let Err(err) = vm.run() {
			panic!("vm error: {}", err)
		}

		let stack_elem = vm.last_popped_stack_elem.expect("missing stack top");

		test_expected_object(expected, stack_elem);
	}
}

fn test_expected_object(expected: Object, actual: Object) {
	match expected {
		Object::Integer(value) => test_integer_object(value, actual),
		Object::Boolean(value) => test_boolean_object(value, actual),
		Object::String(value) => test_string_object(value, actual),
		Object::Array(value) => test_array_object(value, actual),
		Object::Hash(value) => test_hash_object(value, actual),
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