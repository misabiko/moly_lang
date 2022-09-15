use std::collections::HashMap;
use std::fmt::Write;
use moly_lang::{
	lexer::Lexer,
	ast::Statement,
	parser::Parser,
};
use moly_lang::ast::{Expression};

#[test]
fn test_let_statements() {
	//TODO Remove semicolon
	let tests = vec![
		("let x = 5;", "x", Expression::Integer(5)),
		("let y = true;", "y", Expression::Boolean(true)),
		//TODO ("let foobar = y;", "foobar", Expression::String("y")),
	];

	for (input, expected_identifier, expected_value) in tests {
		match parse_single_statement(input) {
			Statement::Let { name, value } => {
				test_let_statement(&name, expected_identifier);

				test_literal_expression(&value, &expected_value)
			},
			stmt => panic!("{:?} not Let", stmt),
		}
	}
}

#[test]
fn test_return_statements() {
	//TODO Remove semicolon
	let tests = vec![
		("return 5;", Expression::Integer(5)),
		("return true;", Expression::Boolean(true)),
		//TODO ("return foobar;", "foobar"),
	];

	for (input, expected_value) in tests {
		match parse_single_statement(input) {
			Statement::Return(Some(value)) => {

				test_literal_expression(&value, &expected_value)
			},
			Statement::Return(None) => panic!("missing returned value"),
			stmt => panic!("{:?} not Return", stmt),
		}
	}
}

#[test]
fn test_identifier_expression() {
	const INPUT: &str = "foobar;";

	let stmt = parse_single_statement(INPUT);

	if let Statement::Expression(Expression::Identifier(ident)) = stmt {
		assert_eq!(ident, "foobar");
	}else {
		panic!("{:?} not Expression(Identifier)", stmt)
	}
}

#[test]
fn test_integer_literal_expression() {
	const INPUT: &str = "5;";

	let stmt = parse_single_statement(INPUT);

	if let Statement::Expression(Expression::Integer(value)) = stmt {
		assert_eq!(value, 5);
	}else {
		panic!("{:?} not Expression(Integer)", stmt)
	}
}

#[test]
fn test_boolean_expression() {
	let tests = vec![
		("true;", true),
		("false;", false),
	];

	for (input, expected_value) in tests {
		let stmt = parse_single_statement(input);

		if let Statement::Expression(Expression::Boolean(value)) = stmt {
			assert_eq!(value, expected_value);
		}else {
			panic!("{:?} not Expression(Boolean)", stmt)
		}
	}
}

#[test]
fn test_parsing_prefix_expressions() {
	let prefix_tests = vec![
		("!5;", "!", Expression::Integer(5)),
		("-15;", "-", Expression::Integer(15)),
		("!true;", "!", Expression::Boolean(true)),
		("!false;", "!", Expression::Boolean(false)),
	];

	for (input, op, value) in prefix_tests {
		let stmt = parse_single_statement(input);

		if let Statement::Expression(Expression::Prefix { operator, right }) = stmt {
			assert_eq!(operator, op);

			//Could dereference and move "right" instead of passing box
			test_literal_expression(right.as_ref(), &value);
		}else {
			panic!("{:?} is not Expression(Prefix)", stmt)
		}
	}
}

#[test]
fn test_parsing_infix_expressions() {
	let infix_tests = vec![
		("5 + 5;", Expression::Integer(5), "+", Expression::Integer(5)),
		("5 - 5;", Expression::Integer(5), "-", Expression::Integer(5)),
		("5 * 5;", Expression::Integer(5), "*", Expression::Integer(5)),
		("5 / 5;", Expression::Integer(5), "/", Expression::Integer(5)),
		("5 > 5;", Expression::Integer(5), ">", Expression::Integer(5)),
		("5 < 5;", Expression::Integer(5), "<", Expression::Integer(5)),
		("5 == 5;", Expression::Integer(5), "==", Expression::Integer(5)),
		("5 != 5;", Expression::Integer(5), "!=", Expression::Integer(5)),
		("true == true", Expression::Boolean(true), "==", Expression::Boolean(true)),
		("true != false", Expression::Boolean(true), "!=", Expression::Boolean(false)),
		("false == false", Expression::Boolean(false), "==", Expression::Boolean(false)),
	];

	for (input, left_value, op, right_value) in infix_tests {
		let stmt = parse_single_statement(input);

		if let Statement::Expression(Expression::Infix { left, operator, right }) = stmt {
			test_literal_expression(&left, &left_value);

			assert_eq!(op, operator);

			test_literal_expression(&right, &right_value);
		} else {
			panic!("{:?} is not Expression(Infix)", stmt);
		}
	}
}

#[test]
fn test_operator_precedence_parsing() {
	let tests = vec![
		(
			"-a * b",
			"((-a) * b)",
		),
		(
			"!-a",
			"(!(-a))",
		),
		(
			"a + b + c",
			"((a + b) + c)",
		),
		(
			"a + b - c",
			"((a + b) - c)",
		),
		(
			"a * b * c",
			"((a * b) * c)",
		),
		(
			"a * b / c",
			"((a * b) / c)",
		),
		(
			"a + b / c",
			"(a + (b / c))",
		),
		(
			"a + b * c + d / e - f",
			"(((a + (b * c)) + (d / e)) - f)",
		),
		(
			"3 + 4; -5 * 5",
			"(3 + 4)((-5) * 5)",
		),
		(
			"5 > 4 == 3 < 4",
			"((5 > 4) == (3 < 4))",
		),
		(
			"5 < 4 != 3 > 4",
			"((5 < 4) != (3 > 4))",
		),
		(
			"3 + 4 * 5 == 3 * 1 + 4 * 5",
			"((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
		),
		(
			"true",
			"true",
		),
		(
			"false",
			"false",
		),
		(
			"3 > 5 == false",
			"((3 > 5) == false)",
		),
		(
			"3 < 5 == true",
			"((3 < 5) == true)",
		),
		(
			"1 + (2 + 3) + 4",
			"((1 + (2 + 3)) + 4)",
		),
		(
			"(5 + 5) * 2",
			"((5 + 5) * 2)",
		),
		(
			"2 / (5 + 5)",
			"(2 / (5 + 5))",
		),
		(
			"-(5 + 5)",
			"(-(5 + 5))",
		),
		(
			"!(true == true)",
			"(!(true == true))",
		),
		(
			"a + add(b * c) + d",
			"((a + add((b * c))) + d)",
		),
		(
			"add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
			"add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
		),
		(
			"add(a + b + c * d / f + g)",
			"add((((a + b) + ((c * d) / f)) + g))",
		),
		(
			"a * [1, 2, 3, 4][b * c] * d",
			"((a * ([1, 2, 3, 4][(b * c)])) * d)",
		),
		(
			"add(a * b[2], b[1], 2 * [1, 2][1])",
			"add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
		),
	];

	for (input, expected) in tests {
		let mut parser = Parser::new(Lexer::new(input));
		let program = parser.parse_program();
		check_parser_errors(&parser);

		assert_eq!(program.to_string(), expected);
	}
}

#[test]
fn test_if_expression() {
	let tests = vec![
		("if (x < y) { x }", false),
		("if (x < y) { x } else { y }", true),
	];

	for (input, has_alternative) in tests {
		let stmt = parse_single_statement(input);

		if let Statement::Expression(Expression::If { condition, consequence, alternative }) = stmt {
			test_infix_expression(
				&condition,
				Expression::Identifier("x".into()),
				"<".into(),
				Expression::Identifier("y".into())
			);

			assert_eq!(consequence.statements.len(), 1, "consequence is not 1 statement. ({:?})", consequence.statements);

			let consequence_stmt = consequence.statements.first().unwrap();

			if let Statement::Expression(ident) = consequence_stmt {
				test_identifier(ident, "x");
			}else {
				panic!("{:?} is not Statement::Expression", consequence_stmt)
			}

			if has_alternative {
				if let Some(alternative) = alternative {
					let alternative_stmt = alternative.statements.first().unwrap();

					if let Statement::Expression(ident) = alternative_stmt {
						test_identifier(ident, "y");
					} else {
						panic!("{:?} is not Statement::Expression", alternative_stmt)
					}
				}else {
					panic!("missing alternative")
				}
			} else {
				assert!(alternative.is_none(), "alternative was not None. ({:?})", alternative);
			}
		}else {
			panic!("{:?} is not Expression(If)", stmt);
		}
	}
}

#[test]
fn test_function_literal_parsing() {
	let tests = vec![
		"fn(x, y) { x + y; }"
	];

	for input in tests {
		let stmt = parse_single_statement(input);

		if let Statement::Expression(Expression::Function { parameters, body }) = stmt {
			assert_eq!(parameters.len(), 2, "function parameters wrong, want 2. ({:?})", parameters);

			test_literal_expression(&parameters[0], &Expression::Identifier("x".into()));
			test_literal_expression(&parameters[1], &Expression::Identifier("y".into()));

			assert_eq!(body.statements.len(), 1, "body.statements doesn't have 1 statement. ({:?})", body.statements);

			let body_stmt = body.statements.first().unwrap();
			if let Statement::Expression(exp) = body_stmt {
				test_infix_expression(&exp, Expression::Identifier("x".into()), "+", Expression::Identifier("y".into()));
			}else {
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
		("fn(x) {};", vec![
			Expression::Identifier("x".into())
		]),
		("fn(x, y, z) {};", vec![
			Expression::Identifier("x".into()),
			Expression::Identifier("y".into()),
			Expression::Identifier("z".into()),
		]),
	];

	for (input, expected_params) in tests {
		let stmt = parse_single_statement(input);
		if let Statement::Expression(Expression::Function { parameters, .. }) = stmt {
			assert_eq!(parameters.len(), expected_params.len());

			for (param, expected_param) in parameters.iter().zip(expected_params.iter()) {
				test_literal_expression(param, expected_param)
			}
		}else {
			panic!("{:?} is not Statement::Expression(Function)", stmt);
		}
	}
}

#[test]
fn test_call_expression_parsing() {
	let tests = vec![
		"add(1, 2 * 3, 4 + 5);"
	];

	for input in tests {
		let stmt = parse_single_statement(input);

		if let Statement::Expression(Expression::Call { function, arguments }) = stmt {
			test_identifier(&function, "add");

			assert_eq!(arguments.len(), 3, "wrong number of arguments");

			test_literal_expression(&arguments[0], &Expression::Integer(1));
			test_infix_expression(&arguments[1], Expression::Integer(2), "*", Expression::Integer(3));
			test_infix_expression(&arguments[2], Expression::Integer(4), "+", Expression::Integer(5))
		} else {
			panic!("{:?} is not Statement::Expression(Call)", stmt);
		}
	}
}

#[test]
fn test_string_literal_expression() {
	let input = r#""hello world";"#;

	let stmt = parse_single_statement(input);

	if let Statement::Expression(Expression::String(value)) = stmt {
		assert_eq!(value, "hello world")
	}else {
		panic!("{:?} is not Statement::Expression(String)", stmt)
	}
}

#[test]
fn test_parsing_array_literals() {
	const INPUT: &str = "[1, 2 * 2, 3 + 3]";

	let stmt = parse_single_statement(INPUT);

	if let Statement::Expression(Expression::Array(value)) = stmt {
		assert_eq!(value.len(), 3, "wrong array length");

		test_integer_literal(&value[0], 1);
		test_infix_expression(&value[1], Expression::Integer(2), "*", Expression::Integer(2));
		test_infix_expression(&value[2], Expression::Integer(3), "+", Expression::Integer(3));
	}else {
		panic!("{:?} is not Statement::Expression(Array)", stmt);
	}
}

#[test]
fn test_parsing_index_expressions() {
	const INPUT: &str = "myArray[1 + 1]";

	let stmt = parse_single_statement(INPUT);

	if let Statement::Expression(Expression::Index { left, index }) = stmt {
		test_identifier(&left, "myArray");

		test_infix_expression(&index, Expression::Integer(1), "+", Expression::Integer(1));
	}else {
		panic!("{:?} is not Statement::Expression(Index)", stmt)
	}
}

#[test]
fn test_parsing_hash_literals_string_keys() {
	const INPUT: &str = r#"{"one": 1, "two": 2, "three": 3}"#;

	let stmt = parse_single_statement(INPUT);

	if let Statement::Expression(Expression::Hash(pairs)) = stmt {
		assert_eq!(pairs.len(), 3);

		let expected = HashMap::from([
			("one", Expression::Integer(1)),
			("two", Expression::Integer(2)),
			("three", Expression::Integer(3)),
		]);

		for (key, value) in pairs {
			if let Expression::String(string) = key {
				test_literal_expression(&value, &expected[string.as_str()])
			}else {
				panic!("hash key {:?} is not HashingExpression::String", key)
			}
		}
	} else {
		panic!("{:?} is not Statement::Expression(Hash)", stmt)
	}
}

//TODO Add tests for hash integers and bools

#[test]
fn test_parsing_empty_hash_literal() {
	let stmt = parse_single_statement("{}");

	if let Statement::Expression(Expression::Hash(pairs)) = stmt {
		assert!(pairs.is_empty())
	} else {
		panic!("{:?} is not Statement::Expression(Hash)", stmt)
	}
}

#[test]
fn test_parsing_hash_literals_with_expressions() {
	let stmt = parse_single_statement(r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#);

	if let Statement::Expression(Expression::Hash(pairs)) = stmt {
		assert_eq!(pairs.len(), 3);

		let mut expected: HashMap<&'static str, Box<dyn Fn(Expression) -> ()>> = HashMap::new();
		expected.insert("one", Box::new(|e: Expression|
			test_infix_expression(&e, Expression::Integer(0), "+", Expression::Integer(1))
		));
		expected.insert("two", Box::new(|e: Expression|
			test_infix_expression(&e, Expression::Integer(10), "-", Expression::Integer(8))
		));
		expected.insert("three", Box::new(|e: Expression|
			test_infix_expression(&e, Expression::Integer(15), "/", Expression::Integer(5))
		));

		for (key, value) in pairs {
			if let Expression::String(string) = key {
				if let Some(test_func) = expected.get(string.as_str()) {
					test_func(value)
				}else {
					panic!("no test function for key {:?} found", string)
				}
			}else {
				panic!("hash key {:?} is not HashingExpression::String", key)
			}
		}
	} else {
		panic!("{:?} is not Statement::Expression(Hash)", stmt)
	}
}

fn check_parser_errors(parser: &Parser) {
	if parser.errors.is_empty() {
		return
	}

	let mut error_msg = format!("parser has {} errors:\n", parser.errors.len());
	for msg in &parser.errors {
		writeln!(error_msg, "parser error: {}", msg).unwrap();
	}
	panic!("{}", error_msg)
}

fn parse_single_statement(input: &str) -> Statement {
	let mut parser = Parser::new(Lexer::new(input));
	let program = parser.parse_program();
	check_parser_errors(&parser);

	assert_eq!(program.statements.len(), 1, "program.statements does not contain 1 statement");

	program.statements.into_iter().next().unwrap()
}

fn test_let_statement(name: &Expression, ident_name: &str) {
	//assert_eq!(stmt.token_literal, "let")

	// let Statement::Let {name, ..} = stmt {
		if let Expression::Identifier(n) = name {
			assert_eq!(n, ident_name);
		}else {
			panic!("{:?} not Expression::Identifier", name);
		}
	/*} else {
		panic!("{:?} not Statement::Let", stmt);
	}*/
}

fn test_identifier(exp: &Expression, value: &str) {
	if let Expression::Identifier(ident) = exp {
		assert_eq!(ident, &value);
	}else {
		panic!("{:?} not Identifier", exp);
	}
}

fn test_literal_expression(exp: &Expression, expected: &Expression) {
	match expected {
		Expression::Identifier(name) => test_identifier(exp, name),
		Expression::Integer(value) => test_integer_literal(exp, *value),
		Expression::Boolean(value) => test_boolean_literal(exp, *value),
		_ => panic!("type of exp ({:?}) not handled", exp),
	}
}

fn test_integer_literal(il: &Expression, value: i64) {
	if let Expression::Integer(v) = il {
		assert_eq!(v, &value);
	}else {
		panic!("{:?} not Integer", il);
	}
}

fn test_boolean_literal(exp: &Expression, value: bool) {
	if let Expression::Boolean(v) = exp {
		assert_eq!(v, &value);
	}else {
		panic!("{:?} not Boolean", exp);
	}
}

fn test_infix_expression(exp: &Expression, left_value: Expression, op: &str, right_value: Expression) {
	if let Expression::Infix { left, operator, right } = exp {
		test_literal_expression(&left, &left_value);

		assert_eq!(operator, &op);

		test_literal_expression(&right, &right_value);
	}else {
		panic!("{:?} is not Infix", exp);
	}
}