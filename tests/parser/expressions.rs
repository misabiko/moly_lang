use moly::ast::{Expression, Function, InfixOperator, IntExpr, PrefixOperator, Statement, StatementBlock};
use moly::token::IntType;
use moly::type_checker::type_env::TypeExpr;

use crate::parse_single_statement;

#[test]
fn test_identifier_expression() {
	const INPUT: &str = "foobar;";

	let stmt = parse_single_statement(INPUT).unwrap();

	if let Statement::Expression { expr: Expression::Identifier(ident), has_semicolon: _ } = stmt {
		assert_eq!(ident, "foobar");
	} else {
		panic!("{:?} not Expression(Identifier)", stmt)
	}
}

#[test]
fn test_integer_literal_expression() {
	let tests = vec![
		("5;", IntExpr::U8(5)),
		("5u8;", IntExpr::U8(5)),
		("5u16;", IntExpr::U16(5)),
		("5u32;", IntExpr::U32(5)),
		("5u64;", IntExpr::U64(5)),
		("5i8;", IntExpr::I8(5)),
		("5i16;", IntExpr::I16(5)),
		("5i32;", IntExpr::I32(5)),
		("5i64;", IntExpr::I64(5)),
	];

	for (input, expected) in tests {
		let stmt = parse_single_statement(input).unwrap();

		if let Statement::Expression { expr: Expression::Integer(value), has_semicolon } = stmt {
			assert_eq!(value, expected);

			assert_eq!(has_semicolon, true, "missing semicolon");
		} else {
			panic!("{:?} not Expression(Integer)", stmt)
		}
	}
}

#[test]
fn test_boolean_expression() {
	let tests = vec![
		("true;", true),
		("false;", false),
	];

	for (input, expected_value) in tests {
		let stmt = parse_single_statement(input).unwrap();

		if let Statement::Expression { expr: Expression::Boolean(value), has_semicolon } = stmt {
			assert_eq!(value, expected_value);

			assert_eq!(has_semicolon, true, "missing semicolon");
		} else {
			panic!("{:?} not Expression(Boolean)", stmt)
		}
	}
}

#[test]
fn test_parsing_prefix_expressions() {
	let prefix_tests = [
		//Could throw parse error if using bang directly on integer
		("!5;", PrefixOperator::Bang, Expression::Integer(IntExpr::U8(5))),
		("-15;", PrefixOperator::Minus, Expression::Integer(IntExpr::U8(15))),
		("!true;", PrefixOperator::Bang, Expression::Boolean(true)),
		("!false;", PrefixOperator::Bang, Expression::Boolean(false)),
	];

	for (input, op, value) in prefix_tests {
		let stmt = parse_single_statement(input).unwrap();

		if let Statement::Expression { expr: Expression::Prefix { operator, right }, has_semicolon } = stmt {
			assert_eq!(operator, op, "wrong prefix operator");

			assert_eq!(right, Box::new(value), "wrong prefix operand");

			assert_eq!(has_semicolon, true, "missing semicolon");
		} else {
			panic!("{:?} is not Expression(Prefix)", stmt)
		}
	}
}

#[test]
fn test_parsing_infix_expressions() {
	let infix_tests = [
		("5 + 5;", Expression::Integer(IntExpr::U8(5)), InfixOperator::Plus, Expression::Integer(IntExpr::U8(5))),
		("5 - 5;", Expression::Integer(IntExpr::U8(5)), InfixOperator::Minus, Expression::Integer(IntExpr::U8(5))),
		("5 * 5;", Expression::Integer(IntExpr::U8(5)), InfixOperator::Mul, Expression::Integer(IntExpr::U8(5))),
		("5 / 5;", Expression::Integer(IntExpr::U8(5)), InfixOperator::Div, Expression::Integer(IntExpr::U8(5))),
		("5 > 5;", Expression::Integer(IntExpr::U8(5)), InfixOperator::GreaterThan, Expression::Integer(IntExpr::U8(5))),
		("5 < 5;", Expression::Integer(IntExpr::U8(5)), InfixOperator::LessThan, Expression::Integer(IntExpr::U8(5))),
		("5 == 5;", Expression::Integer(IntExpr::U8(5)), InfixOperator::Equal, Expression::Integer(IntExpr::U8(5))),
		("5 != 5;", Expression::Integer(IntExpr::U8(5)), InfixOperator::Unequal, Expression::Integer(IntExpr::U8(5))),
		("true == true", Expression::Boolean(true), InfixOperator::Equal, Expression::Boolean(true)),
		("true != false", Expression::Boolean(true), InfixOperator::Unequal, Expression::Boolean(false)),
		("false == false", Expression::Boolean(false), InfixOperator::Equal, Expression::Boolean(false)),
	];

	for (input, left_value, op, right_value) in infix_tests {
		let stmt = parse_single_statement(input).unwrap();

		if let Statement::Expression { expr, has_semicolon: _ } = stmt {
			assert_eq!(
				expr,
				Expression::Infix {
					left: Box::new(left_value),
					operator: op,
					right: Box::new(right_value),
				},
				"`{}` gave wrong infix expression",
				input
			);
		} else {
			panic!("{:?} is not Expression(Infix)", stmt);
		}
	}
}

#[test]
fn test_if_expression() {
	let tests = vec![
		("if x < y { x }", false),
		("if x < y { x } else { y }", true),
	];

	for (input, has_alternative) in tests {
		let stmt = parse_single_statement(input).unwrap();

		if let Statement::Expression { expr: Expression::If { condition, consequence, alternative }, has_semicolon: _ } = stmt {
			assert_eq!(
				condition,
				Box::new(Expression::Infix {
					left: Box::new(Expression::Identifier("x".into())),
					operator: InfixOperator::LessThan,
					right: Box::new(Expression::Identifier("y".into())),
				}),
				"wrong condition"
			);

			assert_eq!(consequence.0.len(), 1, "consequence is not 1 statement. ({:?})", consequence.0);

			let consequence_stmt = consequence.0.first().unwrap();

			assert_eq!(
				*consequence_stmt,
				Statement::Expression { expr: Expression::Identifier("x".into()), has_semicolon: false },
				"wrong if consequence"
			);

			if has_alternative {
				if let Some(alternative) = alternative {
					let alternative_stmt = alternative.0.first().unwrap();

					assert_eq!(
						*alternative_stmt,
						Statement::Expression { expr: Expression::Identifier("y".into()), has_semicolon: false },
						"wrong if alternative"
					);
				} else {
					panic!("missing alternative")
				}
			} else {
				assert!(alternative.is_none(), "alternative was not None. ({:?})", alternative);
			}
		} else {
			panic!("{:?} is not Expression(If)", stmt);
		}
	}
}

#[test]
fn test_function_literal_parsing() {
	let tests = vec![
		("fn(x u8, y u8) { x + y; }", TypeExpr::Void),
		("fn(x u8, y u8) u8 { x + y }", TypeExpr::Int(IntType::U8)),
	];

	for (input, expected_return) in tests {
		let stmt = parse_single_statement(input).unwrap();

		if let Statement::Expression { expr: Expression::Function(Function { name, parameters, body, return_type }), has_semicolon: _ } = stmt {
			assert_eq!(name, None, "shouldn't have a name");

			assert_eq!(parameters.len(), 2, "function parameters wrong, want 2. ({:?})", parameters);

			assert_eq!(parameters[0], ("x".into(), TypeExpr::Int(IntType::U8)));
			assert_eq!(parameters[1], ("y".into(), TypeExpr::Int(IntType::U8)));

			assert_eq!(return_type, expected_return, "wrong return type");

			assert_eq!(body.0.len(), 1, "body.statements doesn't have 1 statement. ({:?})", body.0);

			let body_stmt = body.0.first().unwrap();
			if let Statement::Expression { expr, has_semicolon: _ } = body_stmt {
				assert_eq!(
					*expr,
					Expression::Infix {
						left: Box::new(Expression::Identifier("x".into())),
						operator: InfixOperator::Plus,
						right: Box::new(Expression::Identifier("y".into())),
					},
					"wrong function body"
				)
			} else {
				panic!("{:?} not Statement::Expression(Infix)", body_stmt);
			}
		} else {
			panic!("{:?} is not Expression(Function)", stmt);
		}
	}
}

#[test]
fn test_function_parameter_parsing() {
	let tests = vec![
		("fn() {};", vec![]),
		("fn(x u8) {};", vec![
			("x".into(), TypeExpr::Int(IntType::U8))
		]),
		("fn(x u8, y str, z i16) {};", vec![
			("x".into(), TypeExpr::Int(IntType::U8)),
			("y".into(), TypeExpr::String),
			("z".into(), TypeExpr::Int(IntType::I16)),
		]),
	];

	for (input, expected_params) in tests {
		let stmt = parse_single_statement(input).unwrap();
		if let Statement::Expression { expr: Expression::Function(Function { parameters, .. }), has_semicolon: _ } = stmt {
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
fn test_call_expression_parsing() {
	let stmt = parse_single_statement("add(1, 2 * 3, 4 + 5);").unwrap();

	if let Statement::Expression { expr: Expression::Call { function, arguments }, has_semicolon: _ } = stmt {
		assert_eq!(
			*function,
			Expression::Identifier("add".into()),
			"wrong function name"
		);

		assert_eq!(
			*arguments,
			[
				Expression::Integer(IntExpr::U8(1)),
				Expression::Infix {
					left: Box::new(Expression::Integer(IntExpr::U8(2))),
					operator: InfixOperator::Mul,
					right: Box::new(Expression::Integer(IntExpr::U8(3))),
				},
				Expression::Infix {
					left: Box::new(Expression::Integer(IntExpr::U8(4))),
					operator: InfixOperator::Plus,
					right: Box::new(Expression::Integer(IntExpr::U8(5))),
				},
			],
			"wrong function arguments"
		);
	} else {
		panic!("{:?} is not Statement::Expression(Call)", stmt);
	}
}

#[test]
fn test_string_literal_expression() {
	let input = r#""hello world";"#;

	let stmt = parse_single_statement(input).unwrap();

	if let Statement::Expression { expr: Expression::String(value), has_semicolon: _ } = stmt {
		assert_eq!(value, "hello world")
	} else {
		panic!("{:?} is not Statement::Expression(String)", stmt)
	}
}

#[test]
fn test_parsing_array_literals() {
	const INPUT: &str = "[1, 2 * 2, 3 + 3]";

	let stmt = parse_single_statement(INPUT).unwrap();

	if let Statement::Expression { expr: Expression::Array(value), has_semicolon: _ } = stmt {
		assert_eq!(value.len(), 3, "wrong array length");

		assert_eq!(
			value,
			[
				Expression::Integer(IntExpr::U8(1)),
				Expression::Infix {
					left: Box::new(Expression::Integer(IntExpr::U8(2))),
					operator: InfixOperator::Mul,
					right: Box::new(Expression::Integer(IntExpr::U8(2))),
				},
				Expression::Infix {
					left: Box::new(Expression::Integer(IntExpr::U8(3))),
					operator: InfixOperator::Plus,
					right: Box::new(Expression::Integer(IntExpr::U8(3))),
				},
			],
			"wrong array elements"
		);
	} else {
		panic!("{:?} is not Statement::Expression(Array)", stmt);
	}
}

#[test]
fn test_parsing_index_expressions() {
	const INPUT: &str = "myArray[1 + 1]";

	let stmt = parse_single_statement(INPUT).unwrap();

	if let Statement::Expression { expr: Expression::Index { left, index }, has_semicolon: _ } = stmt {
		assert_eq!(
			left,
			Box::new(Expression::Identifier("myArray".into())),
			"wrong indexed array expression"
		);

		assert_eq!(
			index,
			Box::new(Expression::Infix {
				left: Box::new(Expression::Integer(IntExpr::U8(1))),
				operator: InfixOperator::Plus,
				right: Box::new(Expression::Integer(IntExpr::U8(1))),
			}),
			"wrong index expression"
		);
	} else {
		panic!("{:?} is not Statement::Expression(Index)", stmt)
	}
}

#[test]
fn test_function_literal_with_name() {
	let stmt = parse_single_statement("let myFunction = fn() { };").unwrap();

	if let Statement::Let { value: Expression::Function(Function { name, .. }), .. } = stmt {
		assert_eq!(name, Some("myFunction".into()), "function literal name wrong")
	} else {
		panic!("{:?} is not Statement::Let {{ value: Function }}", stmt)
	}
}

#[test]
fn test_block_expression() {
	let tests = vec![
		("{}", Statement::Expression {
			expr: Expression::Block {
				statements: StatementBlock(vec![]),
				return_transparent: false,
			},
			has_semicolon: false,
		}),
		("{ 0; }", Statement::Expression {
			expr: Expression::Block {
				statements: StatementBlock(vec![
					Statement::Expression {
						expr: Expression::Integer(IntExpr::U8(0)),
						has_semicolon: true
					}
				]),
				return_transparent: false,
			},
			has_semicolon: false
		}),
		("let x = { 0 }", Statement::Let {
			name: "x".into(),
			value: Expression::Block {
				statements: StatementBlock(vec![
					Statement::Expression {
						expr: Expression::Integer(IntExpr::U8(0)),
						has_semicolon: false
					}
				]),
				return_transparent: false,
			}
		}),
	];

	for (input, expected) in tests {
		let stmt = parse_single_statement(input).unwrap();

		assert_eq!(stmt, expected);
	}
}
