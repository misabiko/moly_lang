use moly_lib::ast::{IntExpr, PrefixOperator};
use moly_lib::lexer::Lexer;
use moly_lib::MolyError;
use moly_lib::parser::Parser;
use moly_lib::reporting::show_error;
use moly_lib::token::TokenType;
use moly_lib::type_checker::{TypeChecker, TypeCheckError};
use moly_lib::type_checker::typed_ast::{TypedStatementBlock, TypedExpression, TypedProgram, TypedStatement, TypedFunction};
use moly_lib::type_checker::type_env::TypeId;

mod type_env;

#[test]
fn test_boolean_expression() {
	let tests = vec![
		("true", true),
		("false", false),
	];

	for (input, expected_value) in tests {
		let stmt = type_check_single_statement(input).unwrap();

		if let TypedStatement::Expression { expr: TypedExpression::Boolean(value), has_semicolon: false } = stmt {
			assert_eq!(value, expected_value);
		} else {
			panic!("{:?} not TypedExpression(Boolean)", stmt)
		}
	}
}

#[test]
fn test_global_functions() {
	let tests = vec![
		("
fn globalFunc() {}

fn main() {
    globalFunc();
}",
		 TypedProgram(vec![
			 TypedStatement::Function(TypedFunction {
				 name: Some("globalFunc".into()),
				 parameters: vec![],
				 body: TypedStatementBlock {
					 statements: vec![],
					 return_type: TypeId::Void,
				 },
				 is_method: false,
			 }),
			 TypedStatement::Function(TypedFunction {
				 name: Some("main".into()),
				 parameters: vec![],
				 body: TypedStatementBlock {
					 statements: vec![
						 TypedStatement::Expression {
							 expr: TypedExpression::Call {
								 function: Box::new(TypedExpression::Identifier {
									 name: "globalFunc".into(),
									 type_id: TypeId::Function {
										 parameters: vec![],
										 return_type: Box::new(TypeId::Void),
										 is_method: false,
									 },
								 }),
								 return_type: TypeId::Void,
								 arguments: vec![],
							 },
							 has_semicolon: true,
						 }
					 ],
					 return_type: TypeId::Void,
				 },
				 is_method: false,
			 }),
		 ])),
	];

	for (input, expected_type) in tests {
		let p = type_check(input).unwrap();

		assert_eq!(p, expected_type);
	}
}

#[test]
fn test_function_parameter_parsing() {
	let tests = vec![
		("fn(x u8, y str, z i16) {};", vec![
			("x".into(), TypeId::U8),
			("y".into(), TypeId::String),
			("z".into(), TypeId::I16),
		]),
	];

	for (input, expected_params) in tests {
		let stmt = type_check_single_statement(input).unwrap();
		if let TypedStatement::Expression { expr: TypedExpression::Function(TypedFunction { parameters, .. }), has_semicolon: _ } = stmt {
			assert_eq!(parameters.len(), expected_params.len());

			for (param, expected_param) in parameters.iter().zip(expected_params.iter()) {
				assert_eq!(param, expected_param)
			}
		} else {
			panic!("{:?} is not Statement::Expression(Function)", stmt);
		}
	}
}

#[test]
fn test_if_expression() {
	let tests = vec![
		("if 2 < 4 { 4; }", Ok(TypeId::Void)),
		("if 2 < 4 { 4 }", Err(MolyError::TypeCheck(TypeCheckError::Generic("mismatched if types U8 vs None".into())))),
		("if 2 < 4 { 2 } else { 4 }", Ok(TypeId::U8)),
	];

	for (input, expected_type) in tests {
		let stmt = type_check_single_statement(input);
		match expected_type {
			Ok(expected_type) => {
				if let Ok(TypedStatement::Expression { expr: TypedExpression::If { type_id, .. }, has_semicolon: _ }) = stmt {
					assert_eq!(type_id, expected_type);
				} else {
					panic!("{:?} not TypedExpression(Boolean)", stmt)
				}
			}
			Err(expected_error) => assert_eq!(stmt.expect_err("stmt didn't return error"), expected_error),
		}
	}
}

#[test]
fn test_scoped_type_bindings() {
	let tests = vec![
		("
			let a = 10;
			let func = fn(a str) str {
				a
			};
			a;
		", TypedStatementBlock {
			statements: vec![
				TypedStatement::Let {
					name: "a".into(),
					value: TypedExpression::Integer(IntExpr::U8(10)),
					type_id: TypeId::U8,
				},
				TypedStatement::Let {
					name: "func".into(),
					value: TypedExpression::Function(TypedFunction {
						name: Some("func".into()),
						parameters: vec![("a".into(), TypeId::String)],
						body: TypedStatementBlock {
							statements: vec![
								TypedStatement::Expression {
									expr: TypedExpression::Identifier {
										name: "a".into(),
										type_id: TypeId::String,
									},
									has_semicolon: false,
								}
							],
							return_type: TypeId::String,
						},
						is_method: false,
					}),
					type_id: TypeId::Function {
						parameters: vec![TypeId::String],
						return_type: Box::new(TypeId::String),
						is_method: false,
					},
				},
				TypedStatement::Expression {
					expr: TypedExpression::Identifier {
						name: "a".into(),
						type_id: TypeId::U8,
					},
					has_semicolon: true,
				},
			],
			return_type: TypeId::Void,
		}),
	];

	for (input, expected_type) in tests {
		let p = type_check_block(input).unwrap();

		assert_eq!(p, expected_type);
	}
}

#[test]
fn test_prefix() {
	let tests = vec![
		("!5", MolyError::TypeCheck(TypeCheckError::PrefixTypeMismatch {
			operator: PrefixOperator::Bang,
			right_type: TypeId::U8,
		})),
		("-true", MolyError::TypeCheck(TypeCheckError::PrefixTypeMismatch {
			operator: PrefixOperator::Minus,
			right_type: TypeId::Bool,
		})),
	];

	for (input, expected_error) in tests {
		let stmt = type_check_single_statement(input);
		assert_eq!(stmt.expect_err("stmt didn't return error"), expected_error)
	}
}

#[test]
fn test_call_arg_num() {
	let tests = vec![
		("
			let func = fn() {};
			func(0)
		", MolyError::TypeCheck(TypeCheckError::CallArgCount {
			parameter_count: 0,
			argument_count: 1,
		})),
		("
			let func = fn(a u8) {};
			func();
		", MolyError::TypeCheck(TypeCheckError::CallArgCount {
			parameter_count: 1,
			argument_count: 0,
		})),
		("
			let func = fn(a u8) {};
			func(0, 0);
		", MolyError::TypeCheck(TypeCheckError::CallArgCount {
			parameter_count: 1,
			argument_count: 2,
		})),
	];

	for (input, expected_error) in tests {
		let program = type_check_block(input);
		assert_eq!(program.expect_err("type check didn't return error"), expected_error)
	}
}

#[test]
fn test_call_arg_type_mismatch() {
	let tests = vec![
		("
			let func = fn(a str) {};
			func(true)
		", MolyError::TypeCheck(TypeCheckError::CallArgTypeMismatch {
			parameter_types: vec![TypeId::String],
			argument_types: vec![TypeId::Bool],
		})),
		(r#"
			let func = fn(a str, b bool) {};
			func(true, "bleh");
		"#, MolyError::TypeCheck(TypeCheckError::CallArgTypeMismatch {
			parameter_types: vec![TypeId::String, TypeId::Bool],
			argument_types: vec![TypeId::Bool, TypeId::String],
		})),
	];

	for (input, expected_error) in tests {
		let program = type_check_block(input);
		assert_eq!(program.expect_err("type check didn't return error"), expected_error)
	}
}

#[test]
fn test_return_statements() {
	let tests = vec![
		("return true;", Ok(TypedStatementBlock {
			statements: vec![
				TypedStatement::Return(Some(TypedExpression::Boolean(true))),
			],
			return_type: TypeId::Bool,
		})),
		("{return true;}", Ok(TypedStatementBlock {
			statements: vec![
				TypedStatement::Expression {
					expr: TypedExpression::Block {
						block: TypedStatementBlock {
							statements: vec![
								TypedStatement::Return(Some(TypedExpression::Boolean(true))),
							],
							return_type: TypeId::Bool,
						},
						return_transparent: false,
					},
					has_semicolon: false,
				},
			],
			return_type: TypeId::Bool,
		})),
		//TODO Warn about redundant if true/false {}
		("
			let if_result = if true {
				return true;
			}else {
				0
			};
			false
		", Ok(TypedStatementBlock {
			statements: vec![
				TypedStatement::Let {
					name: "if_result".into(),
					value: TypedExpression::If {
						condition: Box::new(TypedExpression::Boolean(true)),
						type_id: TypeId::U8,
						consequence: TypedStatementBlock {
							statements: vec![
								TypedStatement::Return(Some(TypedExpression::Boolean(true)))
							],
							return_type: TypeId::Return(Box::new(TypeId::Bool)),
						},
						alternative: Some(TypedStatementBlock {
							statements: vec![TypedStatement::Expression {
								expr: TypedExpression::Integer(IntExpr::U8(0)),
								has_semicolon: false,
							}],
							return_type: TypeId::U8,
						}),
					},
					type_id: TypeId::U8,
				},
				TypedStatement::Expression {
					expr: TypedExpression::Boolean(false),
					has_semicolon: false,
				},
			],
			return_type: TypeId::Bool,
		})),
		("
			if true {
				return true;
			}else {
				return 0;
			}
		", Err(MolyError::TypeCheck(TypeCheckError::ReturnTypeMismatch {
			scope_return_type: TypeId::Bool,
			mismatched_type: TypeId::U8,
		}))),
		("
			fn() {
				if true {
					return true;
				}else {
					return 0;
				}
			}
		", Err(MolyError::TypeCheck(TypeCheckError::ReturnTypeMismatch {
			scope_return_type: TypeId::Bool,
			mismatched_type: TypeId::U8,
		}))),
	];

	for (input, expected_type) in tests {
		let p = type_check_block(input);

		assert_eq!(p, expected_type);
	}
}

#[test]
fn test_struct_construction() {
	let tests = vec![
		(
			r#"
				struct Person {
					name str,
					age u8,
				}
				Person { name: "Bob", age: 24 }
			"#,
			Ok(TypedStatementBlock {
				statements: vec![
					TypedStatement::Expression {
						expr: TypedExpression::Struct {
							name: "Person".into(),
							fields: vec![
								("name".into(), TypedExpression::String("Bob".into())),
								("age".into(), TypedExpression::Integer(IntExpr::U8(24))),
							],
							type_id: TypeId::CustomType(0),
						},
						has_semicolon: false,
					}
				],
				return_type: TypeId::CustomType(0),
			}),
		),
		(
			"
				struct Pair(i32, u32)
				Pair(10i32, 1u32)
			",
			Ok(TypedStatementBlock {
				statements: vec![
					TypedStatement::Expression {
						expr: TypedExpression::Call {
							function: Box::new(TypedExpression::Identifier {
								name: "Pair".into(),
								type_id: TypeId::CustomType(0),
							}),
							return_type: TypeId::CustomType(0),
							arguments: vec![
								TypedExpression::Integer(IntExpr::I32(10)),
								TypedExpression::Integer(IntExpr::U32(1)),
							],
						},
						has_semicolon: false,
					}
				],
				return_type: TypeId::CustomType(0),
			}),
		),
	];

	for (input, expected) in tests {
		let stmt = type_check_block(input);
		assert_eq!(stmt, expected)
	}
}

#[test]
fn test_method() {
	let tests = vec![
		(
			r#"struct Apple {}

				fn[a Apple] myFunc() {}

				fn main() {
					Apple{}.myFunc()
				}
			"#,
			Ok(TypedProgram(vec![
				TypedStatement::Function(TypedFunction {
					parameters: vec![("a".into(), TypeId::CustomType(0))],
					body: TypedStatementBlock {
						statements: vec![],
						return_type: TypeId::Void,
					},
					name: Some("myFunc".into()),
					is_method: true,
				}),
				TypedStatement::Function(TypedFunction {
					parameters: vec![],
					body: TypedStatementBlock {
						statements: vec![
							TypedStatement::Expression {
								expr: TypedExpression::Call {
									function: Box::new(TypedExpression::Field {
										left: Box::new(TypedExpression::Struct {
											name: "Apple".into(),
											fields: vec![],
											type_id: TypeId::CustomType(0),
										}),
										field: "myFunc".into(),
										left_type: TypeId::CustomType(0),
										field_type: TypeId::Function {
											parameters: vec![TypeId::CustomType(0)],
											return_type: Box::new(TypeId::Void),
											is_method: true,
										},
										binding_index: None,
									}),
									arguments: vec![],
									return_type: TypeId::Void,
								},
								has_semicolon: false,
							}
						],
						return_type: TypeId::Void,
					},
					name: Some("main".into()),
					is_method: false,
				}),
			])),
		),
	];

	for (input, expected) in tests {
		let stmt = type_check(input);
		match expected {
			Ok(expected) => if let Err(err) = stmt {
				eprintln!("{}", show_error(err.clone(), input.to_string()));
				panic!("{}", err);
			}else {
				assert_eq!(stmt, Ok(expected))
			}
			Err(expected) => assert_eq!(stmt, expected),
		}
	}
}

fn type_check(input: &str) -> Result<TypedProgram, MolyError> {
	let program = match Parser::new(Lexer::new(input)).parse_program() {
		Ok(p) => p,
		Err(err) => return Err(MolyError::Parse(err)),
	};

	let mut type_checker = TypeChecker::new();
	match type_checker.check(program, true) {
		Ok(program) => Ok(program),
		Err(err) => return Err(MolyError::TypeCheck(err)),
	}
}

fn type_check_block(input: &str) -> Result<TypedStatementBlock, MolyError> {
	let program = match Parser::new(Lexer::new(input)).parse_block_statement(TokenType::EOF) {
		Ok(p) => p,
		Err(err) => return Err(MolyError::Parse(err)),
	};

	let mut type_checker = TypeChecker::new();
	match type_checker.check_block(program, true, false) {
		Ok(program) => Ok(program),
		Err(err) => return Err(MolyError::TypeCheck(err)),
	}
}

fn type_check_single_statement(input: &str) -> Result<TypedStatement, MolyError> {
	let program = type_check_block(input)?;

	assert_eq!(program.statements.len(), 1, "program.statements does not contain 1 statement");

	Ok(program.statements.into_iter().next().unwrap())
}