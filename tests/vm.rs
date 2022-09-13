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
		VMTestCase {input: "1", expected: Object::Integer(1) },
		VMTestCase {input: "2", expected: Object::Integer(2) },
		VMTestCase {input: "1 + 2", expected: Object::Integer(3) },
	];

	run_vm_tests(tests);
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

		let stack_elem = vm.stack_top().expect("missing stack top");

		test_expected_object(expected, stack_elem);
	}
}

fn test_expected_object(expected: Object, actual: &Object) {
	match expected {
		Object::Integer(value) => test_integer_object(value, actual),
	}
}

fn test_integer_object(expected: i64, actual: &Object) {
	if let Object::Integer(value) = actual {
		assert_eq!(expected, *value);
	} else {
		panic!("{:?} is not Integer", actual);
	}
}

fn parse(input: &str) -> Program {
	Parser::new(Lexer::new(input)).parse_program()
}