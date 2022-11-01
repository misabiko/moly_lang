use moly_lib::compiler::Compiler;
use moly_lib::lexer::Lexer;
use moly_lib::object::Object;
use moly_lib::parser::Parser;
use moly_lib::token::TokenType;
use moly_lib::type_checker::TypeChecker;
use moly_lib::vm::VM;

mod effects;

//TODO Move global tests to .moly files

#[test]
fn test_integer_arithmetic() {
	let tests = vec![
		TestCase { input: "1", expected: Ok(Some(Object::U8(1))) },
		TestCase { input: "2", expected: Ok(Some(Object::U8(2))) },
		TestCase { input: "1 + 2", expected: Ok(Some(Object::U8(3))) },
		TestCase { input: "1i8 - 2i8", expected: Ok(Some(Object::I8(-1))) },
		TestCase { input: "1 * 2", expected: Ok(Some(Object::U8(2))) },
		TestCase { input: "4 / 2", expected: Ok(Some(Object::U8(2))) },
		TestCase { input: "50 / 2 * 2 + 10 - 5", expected: Ok(Some(Object::U8(55))) },
		TestCase { input: "5 + 5 + 5 + 5 - 10", expected: Ok(Some(Object::U8(10))) },
		TestCase { input: "2 * 2 * 2 * 2 * 2", expected: Ok(Some(Object::U8(32))) },
		TestCase { input: "5 * 2 + 10", expected: Ok(Some(Object::U8(20))) },
		TestCase { input: "5 + 2 * 10", expected: Ok(Some(Object::U8(25))) },
		TestCase { input: "5 * (2 + 10)", expected: Ok(Some(Object::U8(60))) },
		TestCase { input: "-5", expected: Ok(Some(Object::I8(-5))) },
		TestCase { input: "-10", expected: Ok(Some(Object::I8(-10))) },
		TestCase { input: "-50 + 100i8 + -50", expected: Ok(Some(Object::I8(0))) },
		TestCase { input: "(5i8 + 10i8 * 2i8 + 15i8 / 3i8) * 2i8 + -10", expected: Ok(Some(Object::I8(50))) },
	];

	run_vm_tests(tests, true);
}

#[test]
fn test_boolean_expressions() {
	let tests = vec![
		TestCase { input: "true", expected: Ok(Some(Object::Boolean(true))) },
		TestCase { input: "false", expected: Ok(Some(Object::Boolean(false))) },
		TestCase { input: "1 < 2", expected: Ok(Some(Object::Boolean(true))) },
		TestCase { input: "1 > 2", expected: Ok(Some(Object::Boolean(false))) },
		TestCase { input: "1 < 1", expected: Ok(Some(Object::Boolean(false))) },
		TestCase { input: "1 > 1", expected: Ok(Some(Object::Boolean(false))) },
		TestCase { input: "1 == 1", expected: Ok(Some(Object::Boolean(true))) },
		TestCase { input: "1 != 1", expected: Ok(Some(Object::Boolean(false))) },
		TestCase { input: "1 == 2", expected: Ok(Some(Object::Boolean(false))) },
		TestCase { input: "1 != 2", expected: Ok(Some(Object::Boolean(true))) },
		TestCase { input: "true == true", expected: Ok(Some(Object::Boolean(true))) },
		TestCase { input: "false == false", expected: Ok(Some(Object::Boolean(true))) },
		TestCase { input: "true == false", expected: Ok(Some(Object::Boolean(false))) },
		TestCase { input: "true != false", expected: Ok(Some(Object::Boolean(true))) },
		TestCase { input: "false != true", expected: Ok(Some(Object::Boolean(true))) },
		TestCase { input: "(1 < 2) == true", expected: Ok(Some(Object::Boolean(true))) },
		TestCase { input: "(1 < 2) == false", expected: Ok(Some(Object::Boolean(false))) },
		TestCase { input: "(1 > 2) == true", expected: Ok(Some(Object::Boolean(false))) },
		TestCase { input: "(1 > 2) == false", expected: Ok(Some(Object::Boolean(true))) },
		TestCase { input: "!true", expected: Ok(Some(Object::Boolean(false))) },
		TestCase { input: "!false", expected: Ok(Some(Object::Boolean(true))) },
		TestCase { input: "!!true", expected: Ok(Some(Object::Boolean(true))) },
		TestCase { input: "!!false", expected: Ok(Some(Object::Boolean(false))) },
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_conditionals() {
	let tests = vec![
		TestCase { input: "if true { 10; }", expected: Ok(None) },
		TestCase { input: "if true { 10 } else { 20 }", expected: Ok(Some(Object::U8(10))) },
		TestCase { input: "if false { 10 } else { 20 } ", expected: Ok(Some(Object::U8(20))) },
		TestCase { input: "if 1 < 2 { 10; }", expected: Ok(None) },
		TestCase { input: "if 1 < 2 { 10 } else { 20 }", expected: Ok(Some(Object::U8(10))) },
		TestCase { input: "if 1 > 2 { 10 } else { 20 }", expected: Ok(Some(Object::U8(20))) },
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_global_let_statements() {
	let tests = vec![
		TestCase { input: "let one = 1; one", expected: Ok(Some(Object::U8(1))) },
		TestCase { input: "let one = 1; let two = 2; one + two", expected: Ok(Some(Object::U8(3))) },
		TestCase { input: "let one = 1; let two = one + one; one + two", expected: Ok(Some(Object::U8(3))) },
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_string_expressions() {
	let tests = vec![
		TestCase { input: r#""monkey""#, expected: Ok(Some(Object::String("monkey".into()))) },
		TestCase { input: r#""mon" + "key""#, expected: Ok(Some(Object::String("monkey".into()))) },
		TestCase { input: r#""mon" + "key" + "banana""#, expected: Ok(Some(Object::String("monkeybanana".into()))) },
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_array_literals() {
	let tests = vec![
		//TestCase { input: "[]", expected: Ok(Some(Object::Array(vec![]))) },
		TestCase { input: "[1, 2, 3]", expected: Ok(Some(Object::Array(vec![
			Object::U8(1),
			Object::U8(2),
			Object::U8(3),
		]))) },
		TestCase { input: "[1 + 2, 3 * 4, 5 + 6]", expected: Ok(Some(Object::Array(vec![
			Object::U8(3),
			Object::U8(12),
			Object::U8(11),
		]))) },
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_index_expressions() {
	let tests = vec![
		TestCase { input: "[1, 2, 3][1]", expected: Ok(Some(Object::U8(2))) },
		TestCase { input: "[1, 2, 3][0 + 2]", expected: Ok(Some(Object::U8(3))) },
		TestCase { input: "[[1, 1, 1]][0][0]", expected: Ok(Some(Object::U8(1))) },
		TestCase { input: "[1, 2, 3][99]", expected: Err(Some("index 99 out of bound [0..2]".into())) },
		TestCase { input: "[1][-1]", expected: Err(Some("index -1 out of bound [0..0]".into())) },
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_calling_functions_without_arguments() {
	let tests = vec![
		TestCase {
			input: "
			let fivePlusTen = fn() u8 { 5 + 10 };
			fivePlusTen()
			",
			expected: Ok(Some(Object::U8(15))),
		},
		TestCase {
			input: "
			let one = fn() u8 { 1 };
			let two = fn() u8 { 2 };
			one() + two()
			",
			expected: Ok(Some(Object::U8(3))),
		},
		TestCase {
			input: "
			let a = fn() u8 { 1 };
			let b = fn() u8 { a() + 1 };
			let c = fn() u8 { b() + 1 };
			c()
			",
			expected: Ok(Some(Object::U8(3))),
		},
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_calling_functions_with_return_statement() {
	let tests = vec![
		TestCase {
			input: "
			let earlyExit = fn() u8 { return 99; 100 };
			earlyExit()
			",
			expected: Ok(Some(Object::U8(99))),
		},
		TestCase {
			input: "
			let earlyExit = fn() u8 { return 99; return 100; };
			earlyExit()
			",
			expected: Ok(Some(Object::U8(99))),
		},
	];

	run_vm_tests(tests, true)
}

//Might want to make it easier to check no value return
#[test]
fn test_functions_without_return_value() {
	let tests = vec![
		TestCase {
			input: "
			let noReturn = fn() { };
			noReturn();
			",
			expected: Ok(None),
		},
		TestCase {
			input: "
			let noReturn = fn() { return; };
			noReturn();
			",
			expected: Ok(None),
		},
		TestCase {
			input: "
			let noReturn = fn() { };
			let noReturnTwo = fn() { noReturn(); };
			noReturn();
			noReturnTwo()
			",
			expected: Ok(None),
		},
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_first_class_functions() {
	let tests = vec![
		TestCase {
			input: "
			let returnsOne = fn() u8 { 1 };
			let returnsOneReturner = fn() fn() u8 { returnsOne };
			returnsOneReturner()()
			",
			expected: Ok(Some(Object::U8(1))),
		},
		TestCase {
			input: "
			let returnsOneReturner = fn() fn() u8 {
				let returnsOne = fn() u8 { 1 };
				returnsOne
			};
			returnsOneReturner()()
			",
			expected: Ok(Some(Object::U8(1))),
		},
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_calling_functions_with_bindings() {
	let tests = vec![
		TestCase {
			input: "
			let one = fn() u8 { let one = 1; one };
			one()
			",
			expected: Ok(Some(Object::U8(1))),
		},
		TestCase {
			input: "
			let oneAndTwo = fn() u8 { let one = 1; let two = 2; one + two };
			oneAndTwo()
			",
			expected: Ok(Some(Object::U8(3))),
		},
		TestCase {
			input: "
			let oneAndTwo = fn() u8 { let one = 1; let two = 2; one + two };
			let threeAndFour = fn() u8 { let three = 3; let four = 4; three + four };
			oneAndTwo() + threeAndFour()
			",
			expected: Ok(Some(Object::U8(10))),
		},
		TestCase {
			input: "
			let firstFoobar = fn() u8 { let foobar = 50; foobar };
			let secondFoobar = fn() u8 { let foobar = 100; foobar };
			firstFoobar() + secondFoobar()
			",
			expected: Ok(Some(Object::U8(150))),
		},
		TestCase {
			input: "
			let globalSeed = 50;
			let minusOne = fn() u8 {
				let num = 1;
				globalSeed - num
			}
			let minusTwo = fn() u8 {
				let num = 2;
				globalSeed - num
			}
			minusOne() + minusTwo()
			",
			expected: Ok(Some(Object::U8(97))),
		},
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_calling_functions_with_arguments_and_bindings() {
	let tests = vec![
		TestCase {
			input: "
			let identity = fn(a u8) u8 { a };
			identity(4)
			",
			expected: Ok(Some(Object::U8(4)))
		},
		TestCase {
			input: "
			let sum = fn(a u8, b u8) u8 { a + b };
			sum(1, 2)
			",
			expected: Ok(Some(Object::U8(3)))
		},
		TestCase {
			input: "
			let sum = fn(a u8, b u8) u8 {
				let c = a + b;
				c
			};
			sum(1, 2)
			",
			expected: Ok(Some(Object::U8(3)))
		},
		TestCase {
			input: "
			let sum = fn(a u8, b u8) u8 {
				let c = a + b;
				c
			};
			sum(1, 2) + sum(3, 4)
			",
			expected: Ok(Some(Object::U8(10)))
		},
		TestCase {
			input: "
			let sum = fn(a u8, b u8) u8 {
				let c = a + b;
				c
			};
			let outer = fn() u8 {
				sum(1, 2) + sum(3, 4)
			};
			outer()
			",
			expected: Ok(Some(Object::U8(10)))
		},
		TestCase {
			input: "
			let globalNum = 10;

			let sum = fn(a u8, b u8) u8 {
				let c = a + b;
				c + globalNum
			};

			let outer = fn() u8 {
				sum(1, 2) + sum(3, 4) + globalNum
			};

			outer() + globalNum
			",
			expected: Ok(Some(Object::U8(50)))
		},
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_builtin_functions() {
	let tests = vec![
		//TODO Add universal function call syntax
		TestCase {input: r#""".len()"#, expected: Ok(Some(Object::U64(0)))},
		TestCase {input: r#""four".len()"#, expected: Ok(Some(Object::U64(4)))},
		TestCase {input: r#""hello world".len()"#, expected: Ok(Some(Object::U64(11)))},
		/*TODO Add and test array trait
		   TestCase {
			input: "1.len()",
			expected: Ok(Some(Object::Error("argument to `len` not supported, got U8(1)".into())))
		},*/
		TestCase {input: "[1, 2, 3].len()", expected: Ok(Some(Object::U64(3)))},
		TestCase {input: "[1].len()", expected: Ok(Some(Object::U64(1)))},
		TestCase {input: "[1, 2, 3].first()", expected: Ok(Some(Object::U8(1)))},
		/*TestCase {
			input: "1.first()",
			expected: Ok(Some(Object::Error("argument to `first` must be Array, got U8(1)".into())))
		},*/
		TestCase {input: "[1, 2, 3].last()", expected: Ok(Some(Object::U8(3)))},
		/*TestCase {
			input: "1.last()",
			expected: Ok(Some(Object::Error("argument to `last` must be Array, got U8(1)".into())))
		},*/
		TestCase {input: "[1, 2, 3].rest()", expected: Ok(Some(Object::Array(vec![
			Object::U8(2),
			Object::U8(3),
		])))},
		TestCase {input: "[1].push(1)", expected: Ok(Some(Object::Array(vec![
			Object::U8(1),
			Object::U8(1),
		])))},
		/*TestCase {
			input: "1.push(1)",
			expected: Ok(Some(Object::Error("argument to `push` must be Array, got U8(1)".into())))
		},*/
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_closures() {
	let tests = vec![
		TestCase {
			input: "
			let newClosure = fn(a u8) fn() u8 {
				fn() u8 { a }
			};
			let closure = newClosure(99);
			closure()
			",
			expected: Ok(Some(Object::U8(99)))
		},
		TestCase {
			input: "
			let newAdder = fn(a u8, b u8) fn(u8) u8 {
				fn(c u8) u8 { a + b + c }
			};
			let adder = newAdder(1, 2);
			adder(8)
			",
			expected: Ok(Some(Object::U8(11)))
		},
		TestCase {
			input: "
			let newAdder = fn(a u8, b u8) fn(u8) u8 {
				let c = a + b;
				fn(d u8) u8 { c + d }
			};
			let adder = newAdder(1, 2);
			adder(8)
			",
			expected: Ok(Some(Object::U8(11)))
		},
		TestCase {
			input: "
			let newAdderOuter = fn(a u8, b u8) fn(u8) fn(u8) u8 {
				let c = a + b;
				fn(d u8) fn(u8) u8 {
					let e = d + c;
					fn(f u8) u8 { e + f }
				}
			};
			let newAdderInner = newAdderOuter(1, 2);
			let adder = newAdderInner(3);
			adder(8)
			",
			expected: Ok(Some(Object::U8(14)))
		},
		TestCase {
			input: "
			let a = 1;
			let newAdderOuter = fn(b u8) fn(u8) fn(u8) u8 {
				fn(c u8) fn(u8) u8 {
					fn(d u8) u8 { a + b + c + d }
				}
			};
			let newAdderInner = newAdderOuter(2);
			let adder = newAdderInner(3);
			adder(8)
			",
			expected: Ok(Some(Object::U8(14)))
		},
		TestCase {
			input: "
			let newClosure = fn(a u8, b u8) fn() u8 {
				let one = fn() u8 { a };
				let two = fn() u8 { b };
				fn() u8 { one() + two() }
			};
			let closure = newClosure(9, 90);
			closure()
			",
			expected: Ok(Some(Object::U8(99)))
		},
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_recursive_functions() {
	let tests = vec![
		TestCase {
			input: "
			let countDown = fn(x u8) u8 {
				if x == 0 {
					return 0;
				} else {
					countDown(x - 1)
				}
			};
			countDown(1)
			",
			expected: Ok(Some(Object::U8(0)))
		},
		TestCase {
			input: "
			let countDown = fn(x u8) u8 {
				if x == 0 {
					return 0;
				} else {
					countDown(x - 1)
				}
			};
			let wrapper = fn() {
				countDown(1);
			};
			wrapper()
			",
			expected: Ok(Some(Object::U8(0)))
		},
		TestCase {
			input: "
			let wrapper = fn() {
				let countDown = fn(x u8) u8 {
					if x == 0 {
						return 0;
					} else {
						countDown(x - 1)
					}
				};
				countDown(1);
			};
			wrapper()
			",
			expected: Ok(Some(Object::U8(0)))
		},
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_recursive_fibonacci() {
	let tests = vec![
		TestCase {
			input: "
			let fibonacci = fn(x u64) u64 {
				if x == 0u64 {
					return 0u64;
				} else {
					if x == 1u64 {
						1u64
					} else {
						fibonacci(x - 1u64) + fibonacci(x - 2u64)
					}
				}
			};
			fibonacci(15u64)
			",
			expected: Ok(Some(Object::U64(610))),
		}
	];

	run_vm_tests(tests, true)
}

#[test]
fn test_global_program() {
	let tests = vec![
		TestCase {
			input: "fn main() {}",
			expected: Ok(None)
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
		//println!("{}", input);
		let program = if compile_block {
			Parser::new(Lexer::new(input)).parse_block_statement(TokenType::EOF)
		}else {
			Parser::new(Lexer::new(input)).parse_program()
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