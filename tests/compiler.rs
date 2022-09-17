use std::collections::HashMap;
use std::rc::Rc;
use moly_lang::ast::Program;
use moly_lang::code::{concat_instructions, instruction_to_string, Instructions, make, Opcode};
use moly_lang::object::{Function, HashingObject, Object};
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
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Add, &vec![]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1; 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Pop, &vec![]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 - 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Sub, &vec![]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 * 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Mul, &vec![]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "2 / 1",
			expected_constants: vec![Object::Integer(2), Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Div, &vec![]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 + 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Add, &vec![]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "-1",
			expected_constants: vec![Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Minus, &vec![]),
				make(Opcode::Pop, &vec![]),
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
				make(Opcode::True, &vec![]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "false",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::False, &vec![]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 > 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::GreaterThan, &vec![]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 < 2",
			expected_constants: vec![Object::Integer(2), Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::GreaterThan, &vec![]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 == 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Equal, &vec![]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "1 != 2",
			expected_constants: vec![Object::Integer(1), Object::Integer(2)],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::NotEqual, &vec![]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "true == false",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::True, &vec![]),
				make(Opcode::False, &vec![]),
				make(Opcode::Equal, &vec![]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "true != false",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::True, &vec![]),
				make(Opcode::False, &vec![]),
				make(Opcode::NotEqual, &vec![]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "!true",
			expected_constants: vec![],
			expected_instructions: vec![
				make(Opcode::True, &vec![]),
				make(Opcode::Bang, &vec![]),
				make(Opcode::Pop, &vec![]),
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
				make(Opcode::True, &vec![]),
				// 0001
				make(Opcode::JumpIfFalse, &vec![10]),
				// 0004
				make(Opcode::Constant, &vec![0]),
				// 0007
				make(Opcode::Jump, &vec![10]),
				// 0010
				make(Opcode::Pop, &vec![]),
				// 0011
				make(Opcode::Constant, &vec![1]),
				// 0013
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "
            if (true) { 10 } else { 20 }; 3333;
            ",
			expected_constants: vec![Object::Integer(10), Object::Integer(20), Object::Integer(3333)],
			expected_instructions: vec![
				// 0000
				make(Opcode::True, &vec![]),
				// 0001
				make(Opcode::JumpIfFalse, &vec![10]),
				// 0004
				make(Opcode::Constant, &vec![0]),
				// 0007
				make(Opcode::Jump, &vec![13]),
				// 0010
				make(Opcode::Constant, &vec![1]),
				// 0013
				make(Opcode::Pop, &vec![]),
				// 0014
				make(Opcode::Constant, &vec![2]),
				// 0017
				make(Opcode::Pop, &vec![]),
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
				make(Opcode::Constant, &vec![0]),
				make(Opcode::SetGlobal, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::SetGlobal, &vec![1]),
			],
		},
		CompilerTestCase {
			input: "
			let one = 1;
			one;
			",
			expected_constants: vec![Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::SetGlobal, &vec![0]),
				make(Opcode::GetGlobal, &vec![0]),
				make(Opcode::Pop, &vec![]),
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
				make(Opcode::Constant, &vec![0]),
				make(Opcode::SetGlobal, &vec![0]),
				make(Opcode::GetGlobal, &vec![0]),
				make(Opcode::SetGlobal, &vec![1]),
				make(Opcode::GetGlobal, &vec![1]),
				make(Opcode::Pop, &vec![]),
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
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Pop, &vec![]),
			]
		},
		CompilerTestCase {
			input: r#""mon" + "key"#,
			expected_constants: vec![
				Object::String("mon".into()),
				Object::String("key".into()),
			],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Add, &vec![]),
				make(Opcode::Pop, &vec![]),
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
				make(Opcode::Array, &vec![0]),
				make(Opcode::Pop, &vec![]),
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
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Constant, &vec![2]),
				make(Opcode::Array, &vec![3]),
				make(Opcode::Pop, &vec![]),
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
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Add, &vec![]),
				make(Opcode::Constant, &vec![2]),
				make(Opcode::Constant, &vec![3]),
				make(Opcode::Sub, &vec![]),
				make(Opcode::Constant, &vec![4]),
				make(Opcode::Constant, &vec![5]),
				make(Opcode::Mul, &vec![]),
				make(Opcode::Array, &vec![3]),
				make(Opcode::Pop, &vec![]),
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
				make(Opcode::Hash, &vec![0]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "{1: 2, 3: 4, 5: 6}",
			expected_constants: vec![1, 2, 3, 4, 5, 6].into_iter()
				.map(|i| Object::Integer(i)).collect(),
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Constant, &vec![2]),
				make(Opcode::Constant, &vec![3]),
				make(Opcode::Constant, &vec![4]),
				make(Opcode::Constant, &vec![5]),
				make(Opcode::Hash, &vec![6]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "{1: 2 + 3, 4: 5 * 6}",
			expected_constants: vec![1, 2, 3, 4, 5, 6].into_iter()
				.map(|i| Object::Integer(i)).collect(),
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Constant, &vec![2]),
				make(Opcode::Add, &vec![]),
				make(Opcode::Constant, &vec![3]),
				make(Opcode::Constant, &vec![4]),
				make(Opcode::Constant, &vec![5]),
				make(Opcode::Mul, &vec![]),
				make(Opcode::Hash, &vec![4]),
				make(Opcode::Pop, &vec![]),
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
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Constant, &vec![2]),
				make(Opcode::Array, &vec![3]),
				make(Opcode::Constant, &vec![3]),
				make(Opcode::Constant, &vec![4]),
				make(Opcode::Add, &vec![]),
				make(Opcode::Index, &vec![]),
				make(Opcode::Pop, &vec![]),
			]
		},
		CompilerTestCase {
			input: "{1: 2}[2 - 1]",
			expected_constants: vec![1, 2, 2, 1].into_iter()
				.map(|i| Object::Integer(i)).collect(),
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Hash, &vec![2]),
				make(Opcode::Constant, &vec![2]),
				make(Opcode::Constant, &vec![3]),
				make(Opcode::Sub, &vec![]),
				make(Opcode::Index, &vec![]),
				make(Opcode::Pop, &vec![]),
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
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &vec![0]),
						make(Opcode::Constant, &vec![1]),
						make(Opcode::Add, &vec![]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 0,
					num_parameters: 0,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![2, 0]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "fn() { 5 + 10 }",
			expected_constants: vec![
				Object::Integer(5),
				Object::Integer(10),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &vec![0]),
						make(Opcode::Constant, &vec![1]),
						make(Opcode::Add, &vec![]),
						make(Opcode::ReturnValue, &vec![]),
				]),
					num_locals: 0,
					num_parameters: 0,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![2, 0]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "fn() { 1; 2 }",
			expected_constants: vec![
				Object::Integer(1),
				Object::Integer(2),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &vec![0]),
						make(Opcode::Pop, &vec![]),
						make(Opcode::Constant, &vec![1]),
						make(Opcode::ReturnValue, &vec![]),
				]),
					num_locals: 0,
					num_parameters: 0,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![2, 0]),
				make(Opcode::Pop, &vec![]),
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
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Return, &vec![]),
				]),
					num_locals: 0,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![0, 0]),
				make(Opcode::Pop, &vec![]),
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

	compiler.emit(Opcode::Mul, vec![]);

	compiler.enter_scope();
	assert_eq!(compiler.scope_index, 1, "scope_index wrong");

	compiler.emit(Opcode::Sub, vec![]);

	assert_eq!(compiler.current_scope().instructions.len(), 1, "instructions length wrong");

	let last = &compiler.current_scope().last_instruction;
	assert!(matches!(last, Some(EmittedInstruction { opcode: Opcode::Sub, .. })), "last_instruction wrong");

	assert!(Rc::ptr_eq(compiler.symbol_table.borrow().outer.as_ref().unwrap(), &global_symbol_table), "compiler did not enclose symbol_table");

	compiler.leave_scope();
	assert_eq!(compiler.scope_index, 0, "scope_index wrong");

	assert!(Rc::ptr_eq(&compiler.symbol_table, &global_symbol_table), "compiler did not restore global symbol table");
	assert_eq!(compiler.symbol_table.borrow().outer, None, "compiler modified global symbol table incorrectly");

	compiler.emit(Opcode::Add, vec![]);

	assert_eq!(compiler.current_scope().instructions.len(), 2, "instructions length wrong");

	let last = &compiler.current_scope().last_instruction;
	assert!(matches!(last, Some(EmittedInstruction { opcode: Opcode::Add, .. })), "last_instruction wrong");

	let previous = &compiler.current_scope().previous_instruction;
	assert!(matches!(previous, Some(EmittedInstruction { opcode: Opcode::Mul, .. })), "previous_instruction wrong");
}

#[test]
fn test_function_calls() {
	let tests = vec![
		CompilerTestCase {
			input: "fn() { 24 }();",
			expected_constants: vec![
				Object::Integer(24),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &vec![0]), // The literal "24"
					make(Opcode::ReturnValue, &vec![]),
				]),
					num_locals: 0,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![1, 0]), // The compiled function
				make(Opcode::Call, &vec![0]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "
            let noArg = fn() { 24 };
            noArg();
            ",
			expected_constants: vec![
				Object::Integer(24),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &vec![0]), // The literal "24"
					make(Opcode::ReturnValue, &vec![]),
				]),
					num_locals: 0,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![1, 0]), // The compiled function
				//TODO Add tip message for redundant variables
				make(Opcode::SetGlobal, &vec![0]),
				make(Opcode::GetGlobal, &vec![0]),
				make(Opcode::Call, &vec![0]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "
            let oneArg = fn(a) { a };
            oneArg(24);
            ",
			expected_constants: vec![
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 0,
					num_parameters: 1,
				}),
				Object::Integer(24),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![0, 0]),
				make(Opcode::SetGlobal, &vec![0]),
				make(Opcode::GetGlobal, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Call, &vec![1]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "
            let manyArg = fn(a, b, c) { a; b; c };
            manyArg(24, 25, 26);
            ",
			expected_constants: vec![
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::Pop, &vec![]),
						make(Opcode::GetLocal, &vec![1]),
						make(Opcode::Pop, &vec![]),
						make(Opcode::GetLocal, &vec![2]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 0,
					num_parameters: 3,
				}),
				Object::Integer(24),
				Object::Integer(25),
				Object::Integer(26),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![0, 0]),
				make(Opcode::SetGlobal, &vec![0]),
				make(Opcode::GetGlobal, &vec![0]),
				make(Opcode::Constant, &vec![1]),
				make(Opcode::Constant, &vec![2]),
				make(Opcode::Constant, &vec![3]),
				make(Opcode::Call, &vec![3]),
				make(Opcode::Pop, &vec![]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_let_statement_scopes() {
	let tests = vec![
		CompilerTestCase {
			input: "
			let num = 55;
			fn() { num }
			",
			expected_constants: vec![
				Object::Integer(55),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetGlobal, &vec![0]),
						make(Opcode::ReturnValue, &vec![]),
				]),
					num_locals: 0,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::SetGlobal, &vec![0]),
				make(Opcode::Closure, &vec![1, 0]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
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
						make(Opcode::Constant, &vec![0]),
						make(Opcode::SetLocal, &vec![0]),
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::ReturnValue, &vec![]),
				]),
					num_locals: 1,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![1, 0]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
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
						make(Opcode::Constant, &vec![0]),
						make(Opcode::SetLocal, &vec![0]),
						make(Opcode::Constant, &vec![1]),
						make(Opcode::SetLocal, &vec![1]),
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::GetLocal, &vec![1]),
						make(Opcode::Add, &vec![]),
						make(Opcode::ReturnValue, &vec![]),
				]),
					num_locals: 2,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![2, 0]),
				make(Opcode::Pop, &vec![]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_builtins() {
	let tests = vec![
		CompilerTestCase {
			input: "
            len([]);
            push([], 1);
            ",
			expected_constants: vec![Object::Integer(1)],
			expected_instructions: vec![
				make(Opcode::GetBuiltin, &vec![0]),
				make(Opcode::Array, &vec![0]),
				make(Opcode::Call, &vec![1]),
				make(Opcode::Pop, &vec![]),
				make(Opcode::GetBuiltin, &vec![5]),
				make(Opcode::Array, &vec![0]),
				make(Opcode::Constant, &vec![0]),
				make(Opcode::Call, &vec![2]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "fn() { len([]) }",
			expected_constants: vec![
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetBuiltin, &vec![0]),
						make(Opcode::Array, &vec![0]),
						make(Opcode::Call, &vec![1]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 0,
					num_parameters: 0,
				})
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![0, 0]),
				make(Opcode::Pop, &vec![]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_closures() {
	let tests = vec![
		CompilerTestCase {
			input: "
            fn(a) {
                fn(b) {
                    a + b
                }
            }
            ",
			expected_constants: vec![
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetFree, &vec![0]),
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::Add, &vec![]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 0,
					num_parameters: 1,
				}),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::Closure, &vec![0, 1]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 0,
					num_parameters: 1,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![1, 0]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
			input: "
            fn(a) {
                fn(b) {
                    fn(c) {
                        a + b + c
                    }
                }
            };
            ",
			expected_constants: vec![
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetFree, &vec![0]),
						make(Opcode::GetFree, &vec![1]),
						make(Opcode::Add, &vec![]),
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::Add, &vec![]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 0,
					num_parameters: 1,
				}),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetFree, &vec![0]),
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::Closure, &vec![0, 2]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 0,
					num_parameters: 1,
				}),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::Closure, &vec![1, 1]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 0,
					num_parameters: 1,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![2, 0]),
				make(Opcode::Pop, &vec![]),
			],
		},
		CompilerTestCase {
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
						make(Opcode::Constant, &vec![3]),
						make(Opcode::SetLocal, &vec![0]),
						make(Opcode::GetGlobal, &vec![0]),
						make(Opcode::GetFree, &vec![0]),
						make(Opcode::Add, &vec![]),
						make(Opcode::GetFree, &vec![1]),
						make(Opcode::Add, &vec![]),
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::Add, &vec![]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 1,
					num_parameters: 0,
				}),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &vec![2]),
						make(Opcode::SetLocal, &vec![0]),
						make(Opcode::GetFree, &vec![0]),
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::Closure, &vec![4, 2]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 1,
					num_parameters: 0,
				}),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Constant, &vec![1]),
						make(Opcode::SetLocal, &vec![0]),
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::Closure, &vec![5, 1]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 1,
					num_parameters: 0,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Constant, &vec![0]),
				make(Opcode::SetGlobal, &vec![0]),
				make(Opcode::Closure, &vec![6, 0]),
				make(Opcode::Pop, &vec![]),
			],
		},
	];

	run_compiler_tests(tests)
}

#[test]
fn test_recursive_functions() {
	let tests = vec![
		CompilerTestCase {
			input: "
            let countDown = fn(x) { countDown(x - 1); };
            countDown(1);
            ",
			expected_constants: vec![
				Object::Integer(1),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::CurrentClosure, &vec![]),
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::Constant, &vec![0]),
						make(Opcode::Sub, &vec![]),
						make(Opcode::Call, &vec![1]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 0,
					num_parameters: 1,
				}),
				Object::Integer(1),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![1, 0]),
				make(Opcode::SetGlobal, &vec![0]),
				make(Opcode::GetGlobal, &vec![0]),
				make(Opcode::Constant, &vec![2]),
				make(Opcode::Call, &vec![1]),
				make(Opcode::Pop, &vec![]),
			]
		},
		CompilerTestCase {
			input: "
            let wrapper = fn() {
                let countDown = fn(x) { countDown(x - 1); };
                countDown(1);
            };
            wrapper();
            ",
			expected_constants: vec![
				Object::Integer(1),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::CurrentClosure, &vec![]),
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::Constant, &vec![0]),
						make(Opcode::Sub, &vec![]),
						make(Opcode::Call, &vec![1]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 0,
					num_parameters: 1,
				}),
				Object::Integer(1),
				Object::Function(Function {
					instructions: concat_instructions(vec![
						make(Opcode::Closure, &vec![1, 0]),
						make(Opcode::SetLocal, &vec![0]),
						make(Opcode::GetLocal, &vec![0]),
						make(Opcode::Constant, &vec![2]),
						make(Opcode::Call, &vec![1]),
						make(Opcode::ReturnValue, &vec![]),
					]),
					num_locals: 1,
					num_parameters: 0,
				}),
			],
			expected_instructions: vec![
				make(Opcode::Closure, &vec![3, 0]),
				make(Opcode::SetGlobal, &vec![0]),
				make(Opcode::GetGlobal, &vec![0]),
				make(Opcode::Call, &vec![0]),
				make(Opcode::Pop, &vec![]),
			]
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

	assert_eq!(instruction_to_string(&actual), instruction_to_string(&concatted), "wrong instructions");
}

fn test_constants(expected: Vec<Object>, actual: Vec<Object>) {
	assert_eq!(actual.len(), expected.len(), "wrong number of constants");

	for (constant, actual) in expected.into_iter().zip(actual.into_iter()) {
		match constant {
			Object::Integer(v) => test_integer_object(v, actual),
			Object::Boolean(v) => test_boolean_object(v, actual),
			Object::String(v) => test_string_object(v, actual),
			Object::Array(v) => test_array_object(v, actual),
			Object::Hash(v) => test_hash_object(v, actual),
			Object::Function(v) => test_function_object(v, actual),
			_ => {}	//TODO Write tests for rest of objects
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
		assert_eq!(elements.len(), expected.len(), "wrong num of elements");

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
		assert_eq!(pairs.len(), expected.len(), "wrong num of pairs");

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

fn test_function_object(expected: Function, actual: Object) {
	if let Object::Function(function) = actual {
		test_instructions(vec![expected.instructions], function.instructions);
	} else {
		panic!("{:?} is not Function", actual);
	}
}