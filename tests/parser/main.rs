use moly::{
	ast::Statement,
	lexer::Lexer,
	parser::Parser,
};

mod statements;
mod expressions;

//TODO Parse DeclarativeProgram

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
			"(3 + 4);((-5) * 5)",
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
		let program = match parser.parse_program() {
			Ok(p) => p,
			Err(err) => panic!("parse error: {}", err),
		};

		assert_eq!(program.to_string(), expected);
	}
}

fn parse_single_statement(input: &str) -> Statement {
	let mut parser = Parser::new(Lexer::new(input));
	let program = match parser.parse_program() {
		Ok(p) => p,
		Err(err) => panic!("parse error: {}", err),
	};

	assert_eq!(program.0.len(), 1, "program.statements does not contain 1 statement");

	program.0.into_iter().next().unwrap()
}
