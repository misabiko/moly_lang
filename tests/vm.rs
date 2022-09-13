use moly_lang::ast::Program;
use moly_lang::compiler::Compiler;
use moly_lang::lexer::Lexer;
use moly_lang::object::Object;
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

fn run_vm_tests(tests: Vec<VMTestCase>) {
	for VMTestCase { input, expected } in tests {
		let program = parse(input);

		let mut compiler = Compiler::new();
		if let Err(err) = compiler.compile(program) {
			panic!("compiler error: {}", err)
		}

		let mut vm = VM::new(compiler.bytecode());
		if let Err(err) = vm.run() {
			panic!("vm error: {}", err)
		}

		let stack_elem = vm.last_popped_stack_elem.expect("missing stack top");

		test_expected_object(expected, &stack_elem);
	}
}

fn test_expected_object(expected: Object, actual: &Object) {
	match expected {
		Object::Integer(value) => test_integer_object(value, actual),
		Object::Boolean(value) => test_boolean_object(value, actual),
	}
}

fn test_integer_object(expected: i64, actual: &Object) {
	if let Object::Integer(value) = actual {
		assert_eq!(expected, *value);
	} else {
		panic!("{:?} is not Integer", actual);
	}
}

fn test_boolean_object(expected: bool, actual: &Object) {
	if let Object::Boolean(value) = actual {
		assert_eq!(expected, *value);
	} else {
		panic!("{:?} is not Boolean", actual);
	}
}

fn parse(input: &str) -> Program {
	Parser::new(Lexer::new(input)).parse_program()
}