use moly::{
	ast::Statement,
	lexer::Lexer,
	parser::Parser,
};
use moly::ast::{Expression, Function, Program, StatementBlock};
use moly::parser::ParserError;
use moly::token::{Token, TokenLiteral, TokenType};
use moly::type_checker::type_env::TypeExpr;

mod statements;
mod expressions;

#[test]
fn test_program_parsing() {
	let tests = vec![
		("let x = 5;", Err(ParserError::InvalidGlobalToken(Token {
			token_type: TokenType::Let,
			literal: TokenLiteral::Static("let"),
			after_whitespace: false
		}))),
		 ("fn(){}", Err(ParserError::MissingGlobalFunctionName)),
		 ("fn myFunc(){}", Ok(StatementBlock(vec![
			Statement::Function(Function {
				name: Some("myFunc".into()),
				parameters: vec![],
				return_type: TypeExpr::Void,
				body: StatementBlock(vec![]),
			})
		]))),
		("fn main(){}", Ok(StatementBlock(vec![
			Statement::Function(Function {
				name: Some("main".into()),
				parameters: vec![],
				return_type: TypeExpr::Void,
				body: StatementBlock(vec![]),
			})
		]))),
		("
fn globalFunc() {}

fn main() {
    globalFunc();
}",
		 Ok(StatementBlock(vec![
			Statement::Function(Function {
				name: Some("globalFunc".into()),
				parameters: vec![],
				return_type: TypeExpr::Void,
				body: StatementBlock(vec![]),
			}),
			Statement::Function(Function {
				name: Some("main".into()),
				parameters: vec![],
				return_type: TypeExpr::Void,
				body: StatementBlock(vec![
					Statement::Expression {
						expr: Expression::Call {
							function: Box::new(Expression::Identifier("globalFunc".into())),
							arguments: vec![],
						},
						has_semicolon: true,
					}
				]),
			}),
		]))),
	];

	for (input, expected) in tests {
		let mut parser = Parser::new(Lexer::new(input));

		assert_eq!(parser.parse_program(), expected);
	}
}

#[test]
fn test_comment_parsing() {
	let tests = vec![
		("fn main(){
			// my comment
		}", Ok(StatementBlock(vec![
			Statement::Function(Function {
				name: Some("main".into()),
				parameters: vec![],
				return_type: TypeExpr::Void,
				body: StatementBlock(vec![]),
			})
		]))),
		("/*pre fn*/fn /*pre main*/main(){/*
		*/}/**/", Ok(StatementBlock(vec![
			Statement::Function(Function {
				name: Some("main".into()),
				parameters: vec![],
				return_type: TypeExpr::Void,
				body: StatementBlock(vec![]),
			})
		]))),
	];

	for (input, expected) in tests {
		let mut parser = Parser::new(Lexer::new(input));

		assert_eq!(parser.parse_program(), expected);
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
		let program = match parser.parse_block_statement(TokenType::EOF) {
			Ok(p) => p,
			Err(err) => panic!("parse error: {}", err),
		};

		assert_eq!(program.to_string(), expected);
	}
}

fn parse(input: &str) -> Result<Program, ParserError> {
	let mut parser = Parser::new(Lexer::new(input));
	parser.parse_block_statement(TokenType::EOF)
}

fn parse_single_statement(input: &str) -> Result<Statement, ParserError> {
	let program = parse(input)?;

	assert_eq!(program.0.len(), 1, "program.statements does not contain 1 statement");

	Ok(program.0.into_iter().next().unwrap())
}
