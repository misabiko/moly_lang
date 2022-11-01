use std::rc::Rc;
use moly_lib::code::{concat_instructions, instruction_to_string, Instructions, make, Opcode};
use moly_lib::object::{Function, Object};
use moly_lib::compiler::{Compiler, EmittedInstruction};
use moly_lib::lexer::Lexer;
use moly_lib::MolyError;
use moly_lib::parser::Parser;
use moly_lib::reporting::show_error;
use moly_lib::token::TokenType;
use moly_lib::type_checker::TypeChecker;

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
fn test_conditionals() {
	let tests = vec![
		TestCase {
			input: "
            if true { 10; }; 3333;
            ",
			expected_constants: vec![Object::U8(10), Object::U16(3333)],
			expected_instructions: vec![
				make(Opcode::True, &[]),
				make(Opcode::JumpIfFalse, &[11]),
				make(Opcode::Constant, &[0]),
				make(Opcode::Pop, &[]),
				make(Opcode::Jump, &[11]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Pop, &[]),
			],
		},
		TestCase {
			input: "
            if true { 10 } else { 20 }; 3333;
            ",
			expected_constants: vec![Object::U8(10), Object::U8(20), Object::U16(3333)],
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

	run_compiler_tests(tests, true)
}

#[test]
fn test_global_let_statements() {
	let tests = vec![
		TestCase {
			input: "
			let one = 1;
			let two = 2;
			",
			expected_constants: vec![Object::U8(1), Object::U8(2)],
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
			expected_constants: vec![Object::U8(1)],
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
			expected_constants: vec![Object::U8(1)],
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

#[test]
fn test_index_expressions() {
	let tests = vec![
		TestCase {
			input: "[1, 2, 3][1 + 1]",
			expected_constants: vec![1, 2, 3, 1, 1].into_iter()
				.map(|i| Object::U8(i)).collect(),
			expected_instructions: vec![
				make(Opcode::Constant, &[0]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Constant, &[2]),
				make(Opcode::Array, &[3]),
				make(Opcode::Constant, &[3]),
				make(Opcode::Constant, &[4]),
				make(Opcode::Add, &[]),
				make(Opcode::Index, &[]),
			]
		},
	];

	run_compiler_tests(tests, true)
}

#[test]
fn test_functions() {
	let tests = vec![
		TestCase {
			input: "fn() u8 { return 5 + 10 }",
			expected_constants: vec![
				Object::U8(5),
				Object::U8(10),
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
			],
		},
		TestCase {
			input: "fn() u8 { 5 + 10 }",
			expected_constants: vec![
				Object::U8(5),
				Object::U8(10),
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
			],
		},
		TestCase {
			input: "fn() u8 { 1; 2 }",
			expected_constants: vec![
				Object::U8(1),
				Object::U8(2),
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
			],
		},
	];

	run_compiler_tests(tests, true)
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
			]
		}
	];

	run_compiler_tests(tests, true)
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
			input: "fn() u8 { 24 }();",
			expected_constants: vec![
				Object::U8(24),
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
            let noArg = fn() u8 { 24 };
            noArg();
            ",
			expected_constants: vec![
				Object::U8(24),
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
            let oneArg = fn(a u8) u8 { a };
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
				Object::U8(24),
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
            let manyArg = fn(a u8, b u8, c u8) u8 { a; b; c };
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
				Object::U8(24),
				Object::U8(25),
				Object::U8(26),
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

	run_compiler_tests(tests, true)
}

#[test]
fn test_let_statement_scopes() {
	let tests = vec![
		TestCase {
			input: "
			let num = 55;
			fn() u8 { num }
			",
			expected_constants: vec![
				Object::U8(55),
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
			],
		},
		TestCase {
			input: "
            fn() u8 {
                let num = 55;
                num
            }
            ",
			expected_constants: vec![
				Object::U8(55),
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
			],
		},
		TestCase {
			input: "
            fn() u8 {
                let a = 55;
                let b = 77;
                a + b
            }
            ",
			expected_constants: vec![
				Object::U8(55),
				Object::U8(77),
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
			],
		},
	];

	run_compiler_tests(tests, true)
}

#[test]
fn test_builtins() {
	let tests = vec![
		TestCase {
			input: "
            [1].len()
            ",
			expected_constants: vec![Object::U8(1)],
			expected_instructions: vec![
				make(Opcode::GetBuiltin, &[0]),
				make(Opcode::Constant, &[0]),
				make(Opcode::Array, &[1]),
				make(Opcode::Call, &[1]),
			],
		},
		TestCase {
			input: "
            [1].len();
            [1].push(2)
            ",
			expected_constants: vec![Object::U8(1), Object::U8(1), Object::U8(2)],
			expected_instructions: vec![
				make(Opcode::GetBuiltin, &[0]),
				make(Opcode::Constant, &[0]),
				make(Opcode::Array, &[1]),
				make(Opcode::Call, &[1]),
				make(Opcode::Pop, &[]),
				make(Opcode::GetBuiltin, &[6]),
				make(Opcode::Constant, &[1]),
				make(Opcode::Array, &[1]),
				make(Opcode::Constant, &[2]),
				make(Opcode::Call, &[2]),
			],
		},
		TestCase {
			input: "fn() u64 { [1].len() }",
			expected_constants: vec![
				Object::U8(1),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetBuiltin, &[0]),
						make(Opcode::Constant, &[0]),
						make(Opcode::Array, &[1]),
						make(Opcode::Call, &[1]),
						make(Opcode::ReturnValue, &[]),
					]),
					num_locals: 0,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[1, 0]),
			],
		},
	];

	run_compiler_tests(tests, true)
}

#[test]
fn test_closures() {
	let tests = vec![
		//TODO Temporary callable syntax
		TestCase {
			input: "
            fn(a u8) fn(u8) u8 {
                fn(b u8) u8 {
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
			],
		},
		TestCase {
			input: "
            fn(a u8) fn(u8) fn(u8) u8 {
                fn(b u8) fn(u8) u8 {
                    fn(c u8) u8 {
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

            fn() fn() fn() {
                let a = 66;

                fn() fn() {
                    let b = 77;

                    fn() {
                        let c = 88;

                        global + a + b + c;
                    }
                }
            }
            ",
			expected_constants: vec![
				Object::U8(55),
				Object::U8(66),
				Object::U8(77),
				Object::U8(88),
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
			],
		},
	];

	run_compiler_tests(tests, true)
}

#[test]
fn test_recursive_functions() {
	let tests = vec![
		TestCase {
			input: "
            let countDown = fn(x u8) u8 { countDown(x - 1) };
            countDown(1);
            ",
			expected_constants: vec![
				Object::U8(1),
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
				Object::U8(1),
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
            let wrapper = fn() u8 {
                let countDown = fn(x u8) u8 { countDown(x - 1) };
                countDown(1)
            };
            wrapper();
            ",
			expected_constants: vec![
				Object::U8(1),
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
				Object::U8(1),
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

	run_compiler_tests(tests, true)
}

#[test]
fn test_global_program() {
	let tests = vec![
		TestCase {
			input: "fn main() {}",
			expected_constants: vec![
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Return, &[]),
					]),
					num_locals: 0,
					num_parameters: 0,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &[0, 0]),
				make(Opcode::SetGlobal, &[0]),
				make(Opcode::GetGlobal, &[0]),
				make(Opcode::Call, &[0]),
			]
		}
	];

	run_compiler_tests(tests, false);
}

#[test]
fn test_comments() {
	let tests = vec![
		TestCase {
			input: "//",
			expected_constants: vec![],
			expected_instructions: vec![]
		},
		TestCase {
			input: "//my comment",
			expected_constants: vec![],
			expected_instructions: vec![]
		},
		TestCase {
			input: "/// my eventual doc comment",
			expected_constants: vec![],
			expected_instructions: vec![]
		},
	];

	run_compiler_tests(tests, true);
}

struct TestCase {
	input: &'static str,
	expected_constants: Vec<Object>,
	expected_instructions: Vec<Instructions>,
}

fn run_compiler_tests(tests: Vec<TestCase>, compile_block: bool) {
	for (i, TestCase { input, expected_constants, expected_instructions }) in tests.into_iter().enumerate() {
		let program = if compile_block {
			Parser::new(Lexer::new(input)).parse_block_statement(TokenType::EOF)
		}else {
			Parser::new(Lexer::new(input)).parse_program()
		};
		let program = match program {
			Ok(p) => p,
			Err(err) => {
				eprintln!("{}", show_error(MolyError::Parse(err.clone()), input.into()));
				panic!("test {}: parse error: {}", i, err)
			}
		};

		let mut type_checker = TypeChecker::new();
		let mut compiler = Compiler::new();
		let compile_result = if compile_block {
			let program = match type_checker.check_block(program, true, false) {
				Ok(program) => program,
				Err(err) => {
					eprintln!("{}", show_error(MolyError::TypeCheck(err.clone()), input.into()));
					panic!("test {}: type checking error: {:?}", i, err)
				}
			};
			compiler.compile_block(program)
		}else {
			let program = match type_checker.check(program, true) {
				Ok(program) => program,
				Err(err) => {
					eprintln!("{}", show_error(MolyError::TypeCheck(err.clone()), input.into()));
					panic!("test {}: type checking error: {:?}", i, err)
				}
			};
			compiler.compile(program)
		};
		if let Err(err) = compile_result {
			panic!("test {}: compiler error: {:?}", i, err)
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