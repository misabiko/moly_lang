use moly::ast::{IntExpr, PrefixOperator};
use moly::lexer::Lexer;
use moly::MolyError;
use moly::parser::Parser;
use moly::token::{IntType, TokenType};
use moly::type_checker::{TypeChecker, TypeCheckError};
use moly::type_checker::typed_ast::{TypedStatementBlock, TypedExpression, TypedProgram, TypedStatement, TypedFunction};
use moly::type_checker::type_env::{TypeBinding, TypeExpr};

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
					return_type: TypeExpr::Void,
				}
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
									type_expr: TypeExpr::FnLiteral {
										parameter_types: vec![],
										return_type: Box::new(TypeExpr::Void),
									},
								}),
								return_type: TypeExpr::Void,
								arguments: vec![],
							},
							has_semicolon: true,
						}
					],
					return_type: TypeExpr::Void,
				}
			})
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
			("x".into(), TypeExpr::Int(IntType::U8)),
			("y".into(), TypeExpr::String),
			("z".into(), TypeExpr::Int(IntType::I16)),
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
		("if 2 < 4 { 4; }", Ok(TypeExpr::Void)),
		("if 2 < 4 { 4 }", Err(MolyError::TypeCheck(TypeCheckError::Generic("mismatched if types Int(U8) vs None".into())))),
		("if 2 < 4 { 2 } else { 4 }", Ok(TypeExpr::Int(IntType::U8))),
	];

	for (input, expected_type) in tests {
		let stmt = type_check_single_statement(input);
		match expected_type {
			Ok(expected_type) => {
				if let Ok(TypedStatement::Expression { expr: TypedExpression::If {type_expr, ..}, has_semicolon: _ }) = stmt {
					assert_eq!(type_expr, expected_type);
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
					type_expr: TypeExpr::Int(IntType::U8),
				},
				TypedStatement::Let {
					name: "func".into(),
					value: TypedExpression::Function(TypedFunction {
						name: Some("func".into()),
						parameters: vec![("a".into(), TypeExpr::String)],
						body: TypedStatementBlock {
							statements: vec![
								TypedStatement::Expression {
									expr: TypedExpression::Identifier {
										name: "a".into(),
										type_expr: TypeExpr::String,
									},
									has_semicolon: false,
								}
							],
							return_type: TypeExpr::String,
						}
					}),
					type_expr: TypeExpr::FnLiteral {
						parameter_types: vec![TypeExpr::String],
						return_type: Box::new(TypeExpr::String),
					},
				},
				TypedStatement::Expression {
					expr: TypedExpression::Identifier {
						name: "a".into(),
						type_expr: TypeExpr::Int(IntType::U8),
					},
					has_semicolon: true,
				},
			],
			return_type: TypeExpr::Void,
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
			right_type: TypeExpr::Int(IntType::U8),
		})),
		("-true", MolyError::TypeCheck(TypeCheckError::PrefixTypeMismatch {
			operator: PrefixOperator::Minus,
			right_type: TypeExpr::Bool,
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
			argument_count: 0
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
			parameter_types: vec![TypeExpr::String],
			argument_types: vec![TypeExpr::Bool]
		})),
		(r#"
			let func = fn(a str, b bool) {};
			func(true, "bleh");
		"#, MolyError::TypeCheck(TypeCheckError::CallArgTypeMismatch {
			parameter_types: vec![TypeExpr::String, TypeExpr::Bool],
			argument_types: vec![TypeExpr::Bool, TypeExpr::String]
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
			return_type: TypeExpr::Bool,
		})),
		("{return true;}", Ok(TypedStatementBlock {
			statements: vec![
				TypedStatement::Expression {
					expr: TypedExpression::Block {
						block: TypedStatementBlock {
							statements: vec![
								TypedStatement::Return(Some(TypedExpression::Boolean(true))),
							],
							return_type: TypeExpr::Bool,
						},
						return_transparent: false
					},
					has_semicolon: false
				},
			],
			return_type: TypeExpr::Bool,
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
						type_expr: TypeExpr::Int(IntType::U8),
						consequence: TypedStatementBlock {
							statements: vec![
								TypedStatement::Return(Some(TypedExpression::Boolean(true)))
							],
							return_type: TypeExpr::Return(Box::new(TypeExpr::Bool)),
						},
						alternative: Some(TypedStatementBlock {
							statements: vec![TypedStatement::Expression {
								expr: TypedExpression::Integer(IntExpr::U8(0)),
								has_semicolon: false
							}],
							return_type: TypeExpr::Int(IntType::U8)
						})
					},
					type_expr: TypeExpr::Int(IntType::U8),
				},
				TypedStatement::Expression {
					expr: TypedExpression::Boolean(false),
					has_semicolon: false,
				}
			],
			return_type: TypeExpr::Bool,
		})),
		("
			if true {
				return true;
			}else {
				return 0;
			}
		", Err(MolyError::TypeCheck(TypeCheckError::ReturnTypeMismatch {
			scope_return_type: TypeExpr::Bool,
			mismatched_type: TypeExpr::Int(IntType::U8)
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
			scope_return_type: TypeExpr::Bool,
			mismatched_type: TypeExpr::Int(IntType::U8)
		}))),
	];

	for (input, expected_type) in tests {
		let p = type_check_block(input);

		assert_eq!(p, expected_type);
	}
}

#[test]
fn test_struct_construction() {
	let person_type = TypeExpr::Struct {
		name: "Person".into(),
		bindings: vec![
			TypeBinding {
				ident: "name".into(),
				type_expr: TypeExpr::String,
			},
			TypeBinding {
				ident: "age".into(),
				type_expr: TypeExpr::Int(IntType::U8),
			},
		],
	};
	let pair_type = TypeExpr::Struct {
		name: "Pair".into(),
		bindings: vec![
			TypeBinding {
				ident: "0".into(),
				type_expr: TypeExpr::Int(IntType::I32),
			},
			TypeBinding {
				ident: "1".into(),
				type_expr: TypeExpr::Int(IntType::U32),
			},
		],
	};

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
							type_expr: person_type.clone(),
						},
						has_semicolon: false,
					}
				],
				return_type: person_type,
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
								type_expr: pair_type.clone(),
							}),
							return_type: pair_type.clone(),
							arguments: vec![
								TypedExpression::Integer(IntExpr::I32(10)),
								TypedExpression::Integer(IntExpr::U32(1)),
							],
						},
						has_semicolon: false,
					}
				],
				return_type: pair_type,
			}),
		),
	];

	for (input, expected) in tests {
		let stmt = type_check_block(input);
		assert_eq!(stmt, expected)
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