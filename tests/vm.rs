use std::collections::HashMap;
use moly_lang::ast::Program;
use moly_lang::code::{concat_instructions, make, Opcode};
use moly_lang::compiler::Compiler;
use moly_lang::lexer::Lexer;
use moly_lang::object::{Closure, Function, HashingObject, Object};
use moly_lang::parser::Parser;
use moly_lang::vm::VM;

struct VMTestCase {
	input: &'static str,
	expected: Result<Object, Option<String>>,
}

#[test]
fn test_integer_arithmetic() {
	let tests = vec![
		VMTestCase { input: "1", expected: Ok(Object::Integer(1)) },
		VMTestCase { input: "2", expected: Ok(Object::Integer(2)) },
		VMTestCase { input: "1 + 2", expected: Ok(Object::Integer(3)) },
		VMTestCase { input: "1 - 2", expected: Ok(Object::Integer(-1)) },
		VMTestCase { input: "1 * 2", expected: Ok(Object::Integer(2)) },
		VMTestCase { input: "4 / 2", expected: Ok(Object::Integer(2)) },
		VMTestCase { input: "50 / 2 * 2 + 10 - 5", expected: Ok(Object::Integer(55)) },
		VMTestCase { input: "5 + 5 + 5 + 5 - 10", expected: Ok(Object::Integer(10)) },
		VMTestCase { input: "2 * 2 * 2 * 2 * 2", expected: Ok(Object::Integer(32)) },
		VMTestCase { input: "5 * 2 + 10", expected: Ok(Object::Integer(20)) },
		VMTestCase { input: "5 + 2 * 10", expected: Ok(Object::Integer(25)) },
		VMTestCase { input: "5 * (2 + 10)", expected: Ok(Object::Integer(60)) },
		VMTestCase { input: "-5", expected: Ok(Object::Integer(-5)) },
		VMTestCase { input: "-10", expected: Ok(Object::Integer(-10)) },
		VMTestCase { input: "-50 + 100 + -50", expected: Ok(Object::Integer(0)) },
		VMTestCase { input: "(5 + 10 * 2 + 15 / 3) * 2 + -10", expected: Ok(Object::Integer(50)) },
	];

	run_vm_tests(tests);
}

#[test]
fn test_boolean_expressions() {
	let tests = vec![
		VMTestCase { input: "true", expected: Ok(Object::Boolean(true)) },
		VMTestCase { input: "false", expected: Ok(Object::Boolean(false)) },
		VMTestCase { input: "1 < 2", expected: Ok(Object::Boolean(true)) },
		VMTestCase { input: "1 > 2", expected: Ok(Object::Boolean(false)) },
		VMTestCase { input: "1 < 1", expected: Ok(Object::Boolean(false)) },
		VMTestCase { input: "1 > 1", expected: Ok(Object::Boolean(false)) },
		VMTestCase { input: "1 == 1", expected: Ok(Object::Boolean(true)) },
		VMTestCase { input: "1 != 1", expected: Ok(Object::Boolean(false)) },
		VMTestCase { input: "1 == 2", expected: Ok(Object::Boolean(false)) },
		VMTestCase { input: "1 != 2", expected: Ok(Object::Boolean(true)) },
		VMTestCase { input: "true == true", expected: Ok(Object::Boolean(true)) },
		VMTestCase { input: "false == false", expected: Ok(Object::Boolean(true)) },
		VMTestCase { input: "true == false", expected: Ok(Object::Boolean(false)) },
		VMTestCase { input: "true != false", expected: Ok(Object::Boolean(true)) },
		VMTestCase { input: "false != true", expected: Ok(Object::Boolean(true)) },
		VMTestCase { input: "(1 < 2) == true", expected: Ok(Object::Boolean(true)) },
		VMTestCase { input: "(1 < 2) == false", expected: Ok(Object::Boolean(false)) },
		VMTestCase { input: "(1 > 2) == true", expected: Ok(Object::Boolean(false)) },
		VMTestCase { input: "(1 > 2) == false", expected: Ok(Object::Boolean(true)) },
		VMTestCase { input: "!true", expected: Ok(Object::Boolean(false)) },
		VMTestCase { input: "!false", expected: Ok(Object::Boolean(true)) },
		//TODO Test error VMTestCase { input: "!5", expected: Ok(Object::Boolean(false)) },
		VMTestCase { input: "!!true", expected: Ok(Object::Boolean(true)) },
		VMTestCase { input: "!!false", expected: Ok(Object::Boolean(false)) },
		//TODO Test error VMTestCase { input: "!!5", expected: Ok(Object::Boolean(true)) },
	];

	run_vm_tests(tests)
}

#[test]
fn test_conditionals() {
	let tests = vec![
		VMTestCase { input: "if (true) { 10 }", expected: Ok(Object::Integer(10)) },
		VMTestCase { input: "if (true) { 10 } else { 20 }", expected: Ok(Object::Integer(10)) },
		VMTestCase { input: "if (false) { 10 } else { 20 } ", expected: Ok(Object::Integer(20)) },
		VMTestCase { input: "if (1) { 10 }", expected: Ok(Object::Integer(10)) },
		VMTestCase { input: "if (1 < 2) { 10 }", expected: Ok(Object::Integer(10)) },
		VMTestCase { input: "if (1 < 2) { 10 } else { 20 }", expected: Ok(Object::Integer(10)) },
		VMTestCase { input: "if (1 > 2) { 10 } else { 20 }", expected: Ok(Object::Integer(20)) },
	];

	run_vm_tests(tests)
}

#[test]
fn test_global_let_statements() {
	let tests = vec![
		VMTestCase { input: "let one = 1; one", expected: Ok(Object::Integer(1)) },
		VMTestCase { input: "let one = 1; let two = 2; one + two", expected: Ok(Object::Integer(3)) },
		VMTestCase { input: "let one = 1; let two = one + one; one + two", expected: Ok(Object::Integer(3)) },
	];

	run_vm_tests(tests)
}

#[test]
fn test_string_expressions() {
	let tests = vec![
		VMTestCase { input: r#""monkey""#, expected: Ok(Object::String("monkey".into())) },
		VMTestCase { input: r#""mon" + "key""#, expected: Ok(Object::String("monkey".into())) },
		VMTestCase { input: r#""mon" + "key" + "banana""#, expected: Ok(Object::String("monkeybanana".into())) },
	];

	run_vm_tests(tests)
}

#[test]
fn test_array_literals() {
	let tests = vec![
		VMTestCase { input: "[]", expected: Ok(Object::Array(vec![])) },
		VMTestCase { input: "[1, 2, 3]", expected: Ok(Object::Array(vec![
			Object::Integer(1),
			Object::Integer(2),
			Object::Integer(3),
		])) },
		VMTestCase { input: "[1 + 2, 3 * 4, 5 + 6]", expected: Ok(Object::Array(vec![
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
		VMTestCase { input: "{}", expected: Ok(Object::Hash(HashMap::new()))},
		VMTestCase {
			input: "{1: 2, 2: 3}",
			expected: Ok(Object::Hash(HashMap::from([
				(HashingObject::Integer(1), (HashingObject::Integer(1), Object::Integer(2))),
				(HashingObject::Integer(2), (HashingObject::Integer(2), Object::Integer(3))),
			])))
		},
		VMTestCase {
			input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
			expected: Ok(Object::Hash(HashMap::from([
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
		VMTestCase { input: "[1, 2, 3][1]", expected: Ok(Object::Integer(2)) },
		VMTestCase { input: "[1, 2, 3][0 + 2]", expected: Ok(Object::Integer(3)) },
		VMTestCase { input: "[[1, 1, 1]][0][0]", expected: Ok(Object::Integer(1)) },
		//TODO Test run time error
		//VMTestCase { input: "[][0]", expected: Ok(Object::Integer(Null)) },
		//VMTestCase { input: "[1, 2, 3][99]", expected: Ok(Object::Integer(Null)) },
		//VMTestCase { input: "[1][-1]", expected: Ok(Object::Integer(Null)) },
		VMTestCase { input: "{1: 1, 2: 2}[1]", expected: Ok(Object::Integer(1)) },
		VMTestCase { input: "{1: 1, 2: 2}[2]", expected: Ok(Object::Integer(2)) },
		//VMTestCase { input: "{1: 1}[0]", expected: Ok(Object::Integer(Null)) },
		//VMTestCase { input: "{}[0]", expected: Ok(Object::Integer(Null)) },
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
			expected: Ok(Object::Integer(15)),
		},
		VMTestCase {
			input: "
			let one = fn() { 1; };
			let two = fn() { 2; };
			one() + two()
			",
			expected: Ok(Object::Integer(3)),
		},
		VMTestCase {
			input: "
			let a = fn() { 1 };
			let b = fn() { a() + 1 };
			let c = fn() { b() + 1 };
			c();
			",
			expected: Ok(Object::Integer(3)),
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
			expected: Ok(Object::Integer(99)),
		},
		VMTestCase {
			input: "
			let earlyExit = fn() { return 99; return 100; };
			earlyExit();
			",
			expected: Ok(Object::Integer(99)),
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
			expected: Err(None),
		},
		VMTestCase {
			input: "
			let noReturn = fn() { };
			let noReturnTwo = fn() { noReturn(); };
			noReturn();
			noReturnTwo();
			",
			expected: Ok(Object::Closure(Closure {
				func: Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetGlobal, &[0]),
						make(Opcode::Call, &[0]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 0,
					num_parameters: 0,
				},
				free: vec![],
			})),
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
			expected: Ok(Object::Integer(1)),
		},
		VMTestCase {
			input: "
			let returnsOneReturner = fn() {
				let returnsOne = fn() { 1; };
				returnsOne;
			};
			returnsOneReturner()();
			",
			expected: Ok(Object::Integer(1)),
		},
	];

	run_vm_tests(tests)
}

#[test]
fn test_calling_functions_with_bindings() {
	let tests = vec![
		VMTestCase {
			input: "
			let one = fn() { let one = 1; one };
			one();
			",
			expected: Ok(Object::Integer(1)),
		},
		VMTestCase {
			input: "
			let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
			oneAndTwo();
			",
			expected: Ok(Object::Integer(3)),
		},
		VMTestCase {
			input: "
			let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
			let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
			oneAndTwo() + threeAndFour();
			",
			expected: Ok(Object::Integer(10)),
		},
		VMTestCase {
			input: "
			let firstFoobar = fn() { let foobar = 50; foobar; };
			let secondFoobar = fn() { let foobar = 100; foobar; };
			firstFoobar() + secondFoobar();
			",
			expected: Ok(Object::Integer(150)),
		},
		VMTestCase {
			input: "
			let globalSeed = 50;
			let minusOne = fn() {
				let num = 1;
				globalSeed - num;
			}
			let minusTwo = fn() {
				let num = 2;
				globalSeed - num;
			}
			minusOne() + minusTwo();
			",
			expected: Ok(Object::Integer(97)),
		},
	];

	run_vm_tests(tests)
}

#[test]
fn test_calling_functions_with_arguments_and_bindings() {
	let tests = vec![
		VMTestCase {
			input: "
			let identity = fn(a) { a; };
			identity(4);
			",
			expected: Ok(Object::Integer(4))
		},
		VMTestCase {
			input: "
			let sum = fn(a, b) { a + b; };
			sum(1, 2);
			",
			expected: Ok(Object::Integer(3))
		},
		VMTestCase {
			input: "
			let sum = fn(a, b) {
				let c = a + b;
				c;
			};
			sum(1, 2);
			",
			expected: Ok(Object::Integer(3))
		},
		VMTestCase {
			input: "
			let sum = fn(a, b) {
				let c = a + b;
				c;
			};
			sum(1, 2) + sum(3, 4);",
			expected: Ok(Object::Integer(10))
		},
		VMTestCase {
			input: "
			let sum = fn(a, b) {
				let c = a + b;
				c;
			};
			let outer = fn() {
				sum(1, 2) + sum(3, 4);
			};
			outer();
			",
			expected: Ok(Object::Integer(10))
		},
		VMTestCase {
			input: "
			let globalNum = 10;

			let sum = fn(a, b) {
				let c = a + b;
				c + globalNum;
			};

			let outer = fn() {
				sum(1, 2) + sum(3, 4) + globalNum;
			};

			outer() + globalNum;
			",
			expected: Ok(Object::Integer(50))
		},
	];

	run_vm_tests(tests)
}

#[test]
fn test_calling_functions_with_wrong_arguments() {
	let tests = vec![
		VMTestCase {
			input:    "fn() { 1; }(1);",
			expected: Err(Some("wrong number of arguments: want=0, got=1".into())),
		},
		VMTestCase {
			input:    "fn(a) { a; }();",
			expected: Err(Some("wrong number of arguments: want=1, got=0".into())),
		},
		VMTestCase {
			input:    "fn(a, b) { a + b; }(1);",
			expected: Err(Some("wrong number of arguments: want=2, got=1".into())),
		},
	];

	run_vm_tests(tests)
}

//TODO Test without returns
#[test]
fn test_builtin_functions() {
	let tests = vec![
		VMTestCase {input: r#"len("")"#, expected: Ok(Object::Integer(0))},
		VMTestCase {input: r#"len("four")"#, expected: Ok(Object::Integer(4))},
		VMTestCase {input: r#"len("hello world")"#, expected: Ok(Object::Integer(11))},
		VMTestCase {
			input: "len(1)",
			expected: Ok(Object::Error("argument to `len` not supported, got Integer(1)".into()))
		},
		VMTestCase {
			input: r#"len("one", "two")"#,
			expected: Ok(Object::Error("wrong number of arguments. got=2, want=1".into()))
		},
		VMTestCase {input: "len([1, 2, 3])", expected: Ok(Object::Integer(3))},
		VMTestCase {input: "len([])", expected: Ok(Object::Integer(0))},
		//VMTestCase {input: r#"puts("hello", "world!")"#, expected: Ok(Object::Integer(Null))},
		VMTestCase {input: "first([1, 2, 3])", expected: Ok(Object::Integer(1))},
		//VMTestCase {input: r#"first([])"#, expected: Ok(Object::Integer(Null))},
		VMTestCase {
			input: "first(1)",
			expected: Ok(Object::Error("argument to `first` must be Array, got Integer(1)".into()))
		},
		VMTestCase {input: "last([1, 2, 3])", expected: Ok(Object::Integer(3))},
		//VMTestCase {input: r#"last([])"#, Null},
		VMTestCase {
			input: "last(1)",
			expected: Ok(Object::Error("argument to `last` must be Array, got Integer(1)".into()))
		},
		VMTestCase {input: "rest([1, 2, 3])", expected: Ok(Object::Array(vec![
			Object::Integer(2),
			Object::Integer(3),
		]))},
		//VMTestCase {input: r#"rest([])"#, Null},
		VMTestCase {input: "push([], 1)", expected: Ok(Object::Array(vec![
			Object::Integer(1)
		]))},
		VMTestCase {
			input: "push(1, 1)",
			expected: Ok(Object::Error("argument to `push` must be Array, got Integer(1)".into()))
		},
	];

	run_vm_tests(tests)
}

#[test]
fn test_closures() {
	let tests = vec![
		VMTestCase {
			input: "
			let newClosure = fn(a) {
				fn() { a; };
			};
			let closure = newClosure(99);
			closure();
			",
			expected: Ok(Object::Integer(99))
		},
		VMTestCase {
			input: "
			let newAdder = fn(a, b) {
				fn(c) { a + b + c };
			};
			let adder = newAdder(1, 2);
			adder(8);
			",
			expected: Ok(Object::Integer(11))
		},
		VMTestCase {
			input: "
			let newAdder = fn(a, b) {
				let c = a + b;
				fn(d) { c + d };
			};
			let adder = newAdder(1, 2);
			adder(8);
			",
			expected: Ok(Object::Integer(11))
		},
		VMTestCase {
			input: "
			let newAdderOuter = fn(a, b) {
				let c = a + b;
				fn(d) {
					let e = d + c;
					fn(f) { e + f; };
				};
			};
			let newAdderInner = newAdderOuter(1, 2)
			let adder = newAdderInner(3);
			adder(8);
			",
			expected: Ok(Object::Integer(14))
		},
		VMTestCase {
			input: "
			let a = 1;
			let newAdderOuter = fn(b) {
				fn(c) {
					fn(d) { a + b + c + d };
				};
			};
			let newAdderInner = newAdderOuter(2)
			let adder = newAdderInner(3);
			adder(8);
			",
			expected: Ok(Object::Integer(14))
		},
		VMTestCase {
			input: "
			let newClosure = fn(a, b) {
				let one = fn() { a; };
				let two = fn() { b; };
				fn() { one() + two(); };
			};
			let closure = newClosure(9, 90);
			closure();
			",
			expected: Ok(Object::Integer(99))
		},
	];

	run_vm_tests(tests)
}

#[test]
fn test_recursive_functions() {
	let tests = vec![
		VMTestCase {
			input: "
			let countDown = fn(x) {
				if (x == 0) {
					return 0;
				} else {
					countDown(x - 1);
				}
			};
			countDown(1);
			",
			expected: Ok(Object::Integer(0))
		},
		VMTestCase {
			input: "
			let countDown = fn(x) {
				if (x == 0) {
					return 0;
				} else {
					countDown(x - 1);
				}
			};
			let wrapper = fn() {
				countDown(1);
			};
			wrapper();
			",
			expected: Ok(Object::Integer(0))
		},
		VMTestCase {
			input: "
			let wrapper = fn() {
				let countDown = fn(x) {
					if (x == 0) {
						return 0;
					} else {
						countDown(x - 1);
					}
				};
				countDown(1);
			};
			wrapper();
			",
			expected: Ok(Object::Integer(0))
		},
	];

	run_vm_tests(tests)
}

#[test]
fn test_recursive_fibonacci() {
	let tests = vec![
		VMTestCase {
			input: "
			let fibonacci = fn(x) {
				if (x == 0) {
					return 0;
				} else {
					if (x == 1) {
						return 1;
					} else {
						fibonacci(x - 1) + fibonacci(x - 2);
					}
				}
			};
			fibonacci(15);
			",
			expected: Ok(Object::Integer(610)),
		}
	];

	run_vm_tests(tests)
}

fn run_vm_tests(tests: Vec<VMTestCase>) {
	for VMTestCase { input, expected } in tests {
		//println!("{}", input);
		let program = parse(input);

		let mut compiler = Compiler::new();
		if let Err(err) = compiler.compile(program) {
			panic!("compiler error: {}", err)
		}

		let bytecode = compiler.bytecode();
		/*println!("{}", instruction_to_string(&bytecode.instructions));

		for (i, constant) in bytecode.constants.iter().enumerate() {
			println!("CONSTANT {} {:p} ({:?}):", i, constant, constant);

			match constant {
				Object::Function(f) => println!(" Instructions:\n{}", instruction_to_string(&f.instructions)),
				Object::Integer(i) => println!(" Value: {}\n", i),
				_ => {}
			}
		}*/

		let mut vm = VM::new(bytecode);
		let vm_result = vm.run();
		let stack_elem = vm.last_popped_stack_elem;
		match (expected, vm_result) {
			(Err(Some(expected_err)), Err(vm_err)) => assert_eq!(vm_err, expected_err),
			(Err(None) | Ok(_), Err(vm_err)) => panic!("vm error: {}", vm_err),
			(Err(Some(expected_err)), Ok(_)) => panic!("expected vm error: {}", expected_err),
			(Err(None), _) => assert!(stack_elem.is_none(), "{:?} should be None", stack_elem),
			(Ok(expected), _) => if let Some(actual) = stack_elem {
				assert_eq!(actual, expected)
			}else {
				panic!("missing last popped stack element")
			}
		}
	}
}

fn parse(input: &str) -> Program {
	Parser::new(Lexer::new(input)).parse_program()
}