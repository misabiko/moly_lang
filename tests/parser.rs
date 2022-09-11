use std::fmt::Write;
use moly_lang::{
	lexer::Lexer,
	ast::Statement,
	parser::Parser,
};
use moly_lang::ast::Expression;

#[test]
fn test_let_statements() {
	//TODO Remove semicolon
	const INPUT: &str = "
let x = 5;
let y = 10;
let foobar = 838383;
";

	let mut parser = Parser::new(Lexer::new(INPUT));

	let program = parser.parse_program();
	check_parser_errors(&parser);

	assert_eq!(program.statements.len(), 3);

	let identifiers = vec![
		"x",
		"y",
		"foobar"
	];

	for (expected, stmt) in identifiers.into_iter().zip(program.statements.iter()) {
		test_let_statement(stmt, expected);
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
	assert!(false, "{}", error_msg)
}

fn test_let_statement(stmt: &Statement, ident_name: &str) {
	//assert_eq!(stmt.token_literal, "let")

	if let Statement::Let {name, ..} = stmt {
		if let Expression::Identifier(n) = name {
			assert_eq!(n, ident_name);
		}else {
			assert!(false, "{:?} not Expression::Identifier", name);
		}
	} else {
		assert!(false, "{:?} not Statement::Let", stmt);
	}
}