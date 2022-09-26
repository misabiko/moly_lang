use std::rc::Rc;
use moly::ast::Program;
use moly::code::{concat_instructions, instruction_to_string, Instructions, make, Opcode};
use moly::object::{Function, Object};
use moly::compiler::{Compiler, EmittedInstruction};
use moly::lexer::Lexer;
use moly::parser::{Parser, ParserError};
use moly::type_checker::TypeChecker;

#[test]
fn test_integer_arithmetic() {
	let tests = vec![
		TestCase {
			input: "1 + 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Add, &[]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "1; 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Pop, &[]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "1 - 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Sub, &[]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "1 * 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Mul, &[]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "2 / 1",
			expected_constants: vec![Object::Integer(2), Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Div, &[]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "1 + 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Add, &[]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "-1",
			expected_constants: vec![Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Minus, &[]),
				make(Opcode::Pop, &[]),
			],
		},
	];

	run_compiler_tests(tests);
}

#[test]
fn test_boolean_expressions() {
	let tests = vec![
		TestCase {
			input: "true",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::True, &[]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "false",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::False, &[]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "1 > 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::GreaterThan, &[]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "1 < 2",
			expected_constants: vec![Object::Integer(2), Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::GreaterThan, &[]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "1 == 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Equal, &[]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "1 != 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::NotEqual, &[]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "true == false",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::True, &[]),
				make(Opcode::False, &[]),
				make(Opcode::Equal, &[]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "true != false",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::True, &[]),
				make(Opcode::False, &[]),
				make(Opcode::NotEqual, &[]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "!true",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::True, &[]),
				make(Opcode::Bang, &[]),
				make(Opcode::Pop, &[]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_conditionals() {
	let tests = vec![
		TestCase {
			input: "
            if true { 10; }; 3333;
            ",
			expected_constants: vec![Object::Integer(10), Object::Integer(3333)],
			expected_instructions: vec![
				// 0000
				make(Opcode::True, &[]),
				// 0001
				make(Opcode::JumpIfFalse, &[10]),
				// 0004
				make(Opcode::Constant, &[0]),
				// 0007
				make(Opcode::Jump, &[10]),
				// 0010
				make(Opcode::Pop, &[]),
				// 0011
				make(Opcode::Constant, &[1]),
				// 0013
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "
            if true { 10 } else { 20 }; 3333;
            ",
			expected_constants: vec![Object::Integer(10), Object::Integer(20), Object::Integer(3333)],
			expected_instructions: vec![
				// 0000
				make(Opcode::True, &[]),
				// 0001
				make(Opcode::JumpIfFalse, &[10]),
				// 0004
				make(Opcode::Constant, &[0]),
				// 0007
				make(Opcode::Jump, &[13]),
				// 0010
				make(Opcode::Constant, &[1]),
				// 0013
				make(Opcode::Pop, &[]),
				// 0014
				make(Opcode::Constant, &[2]),
				// 0017
				make(Opcode::Pop, &[]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_global_let_statements() {
	let tests = vec![
		TestCase {
			input: "
			let one = 1;
			let two = 2;
			",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::SetGlobal, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::SetGlobal, &[1]),
			],
		},
		TestCase {
			input: "
			let one = 1;
			one;
			",
			expected_constants: vec![Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::SetGlobal, &[0]),
				make(Opcode::GetGlobal, &[0]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "
			let one = 1;
			let two = one;
			two;
			",
			expected_constants: vec![Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::SetGlobal, &[0]),
				make(Opcode::GetGlobal, &[0]),
				make(Opcode::SetGlobal, &[1]),
				make(Opcode::GetGlobal, &[1]),
				make(Opcode::Pop, &[]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_string_expressions() {
	let tests = vec![
		TestCase {
			input: r#""monkey"#,
			expected_constants: vec![Object::String("monkey".into())],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Pop, &[]),
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
				make(Opcode::Pop, &[]),
			]
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_array_literals() {
	let tests = vec![
		TestCase {
			input: "[]",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::Array, &[0]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "[1, 2, 3]",
			expected_constants: vec![
				Object::Integer(1),
				Object::Integer(2),
				Object::Integer(3),
			],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Constant, &[2]),
				make(Opcode::Array, &[3]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
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
				make(Opcode::Pop, &[]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_hash_literals() {
	let tests = vec![
		TestCase {
			input: "{}",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::Hash, &[0]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "{1: 2, 3: 4, 5: 6}",
			expected_constants: vec![1, 2, 3, 4, 5, 6].into_iter()
				.map(|i| Object::Integer(i)).collect(),
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Constant, &[2]),
				make(Opcode::Constant, &[3]),
				make(Opcode::Constant, &[4]),
				make(Opcode::Constant, &[5]),
				make(Opcode::Hash, &[6]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "{1: 2 + 3, 4: 5 * 6}",
			expected_constants: vec![1, 2, 3, 4, 5, 6].into_iter()
				.map(|i| Object::Integer(i)).collect(),
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Constant, &[2]),
				make(Opcode::Add, &[]),
				make(Opcode::Constant, &[3]),
				make(Opcode::Constant, &[4]),
				make(Opcode::Constant, &[5]),
				make(Opcode::Mul, &[]),
				make(Opcode::Hash, &[4]),
				make(Opcode::Pop, &[]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_index_expressions() {
	let tests = vec![
		TestCase {
			input: "[1, 2, 3][1 + 1]",
			expected_constants: vec![1, 2, 3, 1, 1].into_iter()
				.map(|i| Object::Integer(i)).collect(),
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Constant, &[2]),
				make(Opcode::Array, &[3]),
				make(Opcode::Constant, &[3]),
				make(Opcode::Constant, &[4]),
				make(Opcode::Add, &[]),
				make(Opcode::Index, &[]),
				make(Opcode::Pop, &[]),
			]
		},
		TestCase {
			input: "{1: 2}[2 - 1]",
			expected_constants: vec![1, 2, 2, 1].into_iter()
				.map(|i| Object::Integer(i)).collect(),
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Hash, &[2]),
				make(Opcode::Constant, &[2]),
				make(Opcode::Constant, &[3]),
				make(Opcode::Sub, &[]),
				make(Opcode::Index, &[]),
				make(Opcode::Pop, &[]),
			]
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_functions() {
	let tests = vec![
		TestCase {
			input: "fn() { return 5 + 10 }",
			expected_constants: vec![
				Object::Integer(5),
				Object::Integer(10),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &[0]),
						make(Opcode::Constant, &[1]),
						make(Opcode::Add, &[]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 0,
					num_parameters: 0,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[2, 0]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "fn() { 5 + 10 }",
			expected_constants: vec![
				Object::Integer(5),
				Object::Integer(10),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &[0]),
						make(Opcode::Constant, &[1]),
						make(Opcode::Add, &[]),
						make(Opcode::ReturnValue, &[]),
				]),
					num_locals: 0,
					num_parameters: 0,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[2, 0]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "fn() { 1; 2 }",
			expected_constants: vec![
				Object::Integer(1),
				Object::Integer(2),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &[0]),
						make(Opcode::Pop, &[]),
						make(Opcode::Constant, &[1]),
						make(Opcode::ReturnValue, &[]),
				]),
					num_locals: 0,
					num_parameters: 0,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[2, 0]),
				make(Opcode::Pop, &[]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_functions_without_return_value() {
	let tests = vec![
		TestCase {
			input: "fn() { }",
			expected_constants: vec![
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Return, &[]),
				]),
					num_locals: 0,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[0, 0]),
				make(Opcode::Pop, &[]),
			]
		}
	];

	run_compiler_tests(tests)
}

#[test]
fn test_compiler_scopes() {
	let mut compiler = Compiler::new();
	assert_eq!(compiler.scope_index, 0, "scope_index wrong");
	let global_symbol_table = compiler.symbol_table.clone();

	compiler.emit(Opcode::Mul, &[]);

	compiler.enter_scope();
	assert_eq!(compiler.scope_index, 1, "scope_index wrong");

	compiler.emit(Opcode::Sub, &[]);

	assert_eq!(compiler.current_scope().instructions.len(), 1, "instructions length wrong");

	let last = &compiler.current_scope().last_instruction;
	assert!(matches!(last, Some(EmittedInstruction { opcode: Opcode::Sub, .. })), "last_instruction wrong");

	assert!(Rc::ptr_eq(compiler.symbol_table.borrow().outer.as_ref().unwrap(), &global_symbol_table), "compiler did not enclose symbol_table");

	compiler.leave_scope();
	assert_eq!(compiler.scope_index, 0, "scope_index wrong");

	assert!(Rc::ptr_eq(&compiler.symbol_table, &global_symbol_table), "compiler did not restore global symbol table");
	assert_eq!(compiler.symbol_table.borrow().outer, None, "compiler modified global symbol table incorrectly");

	compiler.emit(Opcode::Add, &[]);

	assert_eq!(compiler.current_scope().instructions.len(), 2, "instructions length wrong");

	let last = &compiler.current_scope().last_instruction;
	assert!(matches!(last, Some(EmittedInstruction { opcode: Opcode::Add, .. })), "last_instruction wrong");

	let previous = &compiler.current_scope().previous_instruction;
	assert!(matches!(previous, Some(EmittedInstruction { opcode: Opcode::Mul, .. })), "previous_instruction wrong");
}

#[test]
fn test_function_calls() {
	let tests = vec![
		TestCase {
			input: "fn() { 24 }();",
			expected_constants: vec![
				Object::Integer(24),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &[0]), // The literal "24"
					make(Opcode::ReturnValue, &[]),
				]),
					num_locals: 0,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[1, 0]), // The compiled function
				make(Opcode::Call, &[0]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "
            let noArg = fn() { 24 };
            noArg();
            ",
			expected_constants: vec![
				Object::Integer(24),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &[0]), // The literal "24"
					make(Opcode::ReturnValue, &[]),
				]),
					num_locals: 0,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[1, 0]), // The compiled function
				//TODO Add tip message for redundant variables
				make(Opcode::SetGlobal, &[0]),
				make(Opcode::GetGlobal, &[0]),
				make(Opcode::Call, &[0]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "
            let oneArg = fn(a u8) { a };
            oneArg(24);
            ",
			expected_constants: vec![
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetLocal, &[0]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 1,
					num_parameters: 1,
				}),
				Object::Integer(24),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[0, 0]),
				make(Opcode::SetGlobal, &[0]),
				make(Opcode::GetGlobal, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Call, &[1]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "
            let manyArg = fn(a u8, b u8, c u8) { a; b; c };
            manyArg(24, 25, 26);
            ",
			expected_constants: vec![
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetLocal, &[0]),
						make(Opcode::Pop, &[]),
						make(Opcode::GetLocal, &[1]),
						make(Opcode::Pop, &[]),
						make(Opcode::GetLocal, &[2]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 3,
					num_parameters: 3,
				}),
				Object::Integer(24),
				Object::Integer(25),
				Object::Integer(26),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[0, 0]),
				make(Opcode::SetGlobal, &[0]),
				make(Opcode::GetGlobal, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Constant, &[2]),
				make(Opcode::Constant, &[3]),
				make(Opcode::Call, &[3]),
				make(Opcode::Pop, &[]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_let_statement_scopes() {
	let tests = vec![
		TestCase {
			input: "
			let num = 55;
			fn() { num }
			",
			expected_constants: vec![
				Object::Integer(55),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetGlobal, &[0]),
						make(Opcode::ReturnValue, &[]),
				]),
					num_locals: 0,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::SetGlobal, &[0]),
				make(Opcode::Closure, &[1, 0]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "
            fn() {
                let num = 55;
                num
            }
            ",
			expected_constants: vec![
				Object::Integer(55),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &[0]),
						make(Opcode::SetLocal, &[0]),
						make(Opcode::GetLocal, &[0]),
						make(Opcode::ReturnValue, &[]),
				]),
					num_locals: 1,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[1, 0]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "
            fn() {
                let a = 55;
                let b = 77;
                a + b
            }
            ",
			expected_constants: vec![
				Object::Integer(55),
				Object::Integer(77),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &[0]),
						make(Opcode::SetLocal, &[0]),
						make(Opcode::Constant, &[1]),
						make(Opcode::SetLocal, &[1]),
						make(Opcode::GetLocal, &[0]),
						make(Opcode::GetLocal, &[1]),
						make(Opcode::Add, &[]),
						make(Opcode::ReturnValue, &[]),
				]),
					num_locals: 2,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[2, 0]),
				make(Opcode::Pop, &[]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_builtins() {
	let tests = vec![
		TestCase {
			input: "
            len([]);
            push([], 1);
            ",
			expected_constants: vec![Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::GetBuiltin, &[0]),
				make(Opcode::Array, &[0]),
				make(Opcode::Call, &[1]),
				make(Opcode::Pop, &[]),
				make(Opcode::GetBuiltin, &[2]),
				make(Opcode::Array, &[0]),
				make(Opcode::Constant, &[0]),
				make(Opcode::Call, &[2]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "fn() { len([]) }",
			expected_constants: vec![
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetBuiltin, &[0]),
						make(Opcode::Array, &[0]),
						make(Opcode::Call, &[1]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 0,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[0, 0]),
				make(Opcode::Pop, &[]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_closures() {
	let tests = vec![
		TestCase {
			input: "
            fn(a u8) {
                fn(b u8) {
                    a + b
                }
            }
            ",
			expected_constants: vec![
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetFree, &[0]),
						make(Opcode::GetLocal, &[0]),
						make(Opcode::Add, &[]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 1,
					num_parameters: 1,
				}),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetLocal, &[0]),
						make(Opcode::Closure, &[0, 1]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 1,
					num_parameters: 1,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[1, 0]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "
            fn(a u8) {
                fn(b u8) {
                    fn(c u8) {
                        a + b + c
                    }
                }
            };
            ",
			expected_constants: vec![
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetFree, &[0]),
						make(Opcode::GetFree, &[1]),
						make(Opcode::Add, &[]),
						make(Opcode::GetLocal, &[0]),
						make(Opcode::Add, &[]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 1,
					num_parameters: 1,
				}),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetFree, &[0]),
						make(Opcode::GetLocal, &[0]),
						make(Opcode::Closure, &[0, 2]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 1,
					num_parameters: 1,
				}),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetLocal, &[0]),
						make(Opcode::Closure, &[1, 1]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 1,
					num_parameters: 1,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[2, 0]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "
            let global = 55;

            fn() {
                let a = 66;

                fn() {
                    let b = 77;

                    fn() {
                        let c = 88;

                        global + a + b + c;
                    }
                }
            }
            ",
			expected_constants: vec![
				Object::Integer(55),
				Object::Integer(66),
				Object::Integer(77),
				Object::Integer(88),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &[3]),
						make(Opcode::SetLocal, &[0]),
						make(Opcode::GetGlobal, &[0]),
						make(Opcode::GetFree, &[0]),
						make(Opcode::Add, &[]),
						make(Opcode::GetFree, &[1]),
						make(Opcode::Add, &[]),
						make(Opcode::GetLocal, &[0]),
						make(Opcode::Add, &[]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 1,
					num_parameters: 0,
				}),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &[2]),
						make(Opcode::SetLocal, &[0]),
						make(Opcode::GetFree, &[0]),
						make(Opcode::GetLocal, &[0]),
						make(Opcode::Closure, &[4, 2]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 1,
					num_parameters: 0,
				}),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &[1]),
						make(Opcode::SetLocal, &[0]),
						make(Opcode::GetLocal, &[0]),
						make(Opcode::Closure, &[5, 1]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 1,
					num_parameters: 0,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::SetGlobal, &[0]),
				make(Opcode::Closure, &[6, 0]),
				make(Opcode::Pop, &[]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_recursive_functions() {
	let tests = vec![
		TestCase {
			input: "
            let countDown = fn(x u8) { countDown(x - 1); };
            countDown(1);
            ",
			expected_constants: vec![
				Object::Integer(1),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::CurrentClosure, &[]),
						make(Opcode::GetLocal, &[0]),
						make(Opcode::Constant, &[0]),
						make(Opcode::Sub, &[]),
						make(Opcode::Call, &[1]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 1,
					num_parameters: 1,
				}),
				Object::Integer(1),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[1, 0]),
				make(Opcode::SetGlobal, &[0]),
				make(Opcode::GetGlobal, &[0]),
				make(Opcode::Constant, &[2]),
				make(Opcode::Call, &[1]),
				make(Opcode::Pop, &[]),
			]
		},
		TestCase {
			input: "
            let wrapper = fn() {
                let countDown = fn(x u8) { countDown(x - 1); };
                countDown(1);
            };
            wrapper();
            ",
			expected_constants: vec![
				Object::Integer(1),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::CurrentClosure, &[]),
						make(Opcode::GetLocal, &[0]),
						make(Opcode::Constant, &[0]),
						make(Opcode::Sub, &[]),
						make(Opcode::Call, &[1]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 1,
					num_parameters: 1,
				}),
				Object::Integer(1),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Closure, &[1, 0]),
						make(Opcode::SetLocal, &[0]),
						make(Opcode::GetLocal, &[0]),
						make(Opcode::Constant, &[2]),
						make(Opcode::Call, &[1]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 1,
					num_parameters: 0,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[3, 0]),
				make(Opcode::SetGlobal, &[0]),
				make(Opcode::GetGlobal, &[0]),
				make(Opcode::Call, &[0]),
				make(Opcode::Pop, &[]),
			]
		},
	];

	run_compiler_tests(tests)
}

struct TestCase {
	input: &'static str,
	expected_constants: Vec<Object>,
	expected_instructions: Vec<Instructions>,
}

fn run_compiler_tests(tests: Vec<TestCase>) {
	for TestCase { input, expected_constants, expected_instructions } in tests {
		let program = match parse(input) {
			Ok(p) => p,
			Err(err) => panic!("parse error: {}", err),
		};

		let mut type_checker = TypeChecker::new();
		let program = match type_checker.check(program) {
			Ok(program) => program,
			Err(err) => panic!("Type checking error: {}", err),
		};

		let mut compiler = Compiler::new();
		if let Err(err) = compiler.compile(program) {
			panic!("compiler error: {:?}", err)
		}

		let bytecode = compiler.bytecode();

		let concatted = concat_instructions(expected_instructions);
		assert_eq!(
			instruction_to_string(&bytecode.instructions),
			instruction_to_string(&concatted),
			"wrong instructions\nInput: `{}`", input
		);

		assert_eq!(
			bytecode.constants,
			expected_constants,
			"wrong constant\nInput `{}`", input
		);
	}
}

fn parse(input: &str) -> Result<Program, ParserError> {
	Parser::new(Lexer::new(input)).parse_program()
}