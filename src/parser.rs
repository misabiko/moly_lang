use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

pub struct Parser {
	pub lexer: Lexer,
	pub errors: Vec<String>,

	pub cur_token: Token,
	pub peek_token: Token,
}

impl Parser {
	pub fn new(lexer: Lexer) -> Self {
		let default_token = Token {token_type: TokenType::Illegal, literal: None};
		let mut parser = Parser {
			lexer,
			errors: vec![],

			cur_token: default_token.clone(),
			peek_token: default_token,
		};

		parser.next_token();
		parser.next_token();

		parser
	}

	fn peek_error(&mut self, t: TokenType) {
		self.errors.push(
			format!(
				"expected next token to be {}, got {} instead",
				t, self.peek_token.token_type
			))
	}

	pub fn next_token(&mut self) {
		self.cur_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
	}

	pub fn parse_program(&mut self) -> Program {
		let mut statements = vec![];

		while self.cur_token.token_type != TokenType::EOF {
			if let Some(stmt) = self.parse_statement() {
				statements.push(stmt);
			}

			self.next_token();
		}

		Program { statements }
	}

	fn parse_statement(&mut self) -> Option<Statement> {
		match self.cur_token.token_type {
			TokenType::Let => self.parse_let_statement(),
			_ => None,
		}
	}

	fn parse_let_statement(&mut self) -> Option<Statement> {
		if !self.expect_peek(TokenType::Ident) {
			return None;
		}

		let name = Expression::Identifier(self.cur_token.literal.clone().expect("Current token doesn't have a value"));

		if !self.expect_peek(TokenType::Assign) {
			return None;
		}

		// TODO: We're skipping the expressions until we
		// encounter a semicolon
		while !self.cur_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		Some(Statement::Let {
			name,
			value: Expression::Identifier("".to_owned()),
		})
	}

	fn cur_token_is(&self, t: TokenType) -> bool {
		self.cur_token.token_type == t
	}

	fn peek_token_is(&self, t: TokenType) -> bool {
		self.peek_token.token_type == t
	}

	fn expect_peek(&mut self, t: TokenType) -> bool {
		if self.peek_token_is(t) {
			self.next_token();
			true
		} else {
			self.peek_error(t);
			false
		}
	}
}