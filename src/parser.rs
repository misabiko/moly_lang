use crate::ast::{BlockStatement, Expression, Program, Statement};
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

	fn no_prefix_parse_fn_error(&mut self, token_type: TokenType) {
		self.errors.push(
			format!(
				"no prefix parse function for {} found",
				token_type
			)
		)
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
			TokenType::Return => self.parse_return_statement(),
			_ => self.parse_expression_statement(),
		}
	}

	fn parse_let_statement(&mut self) -> Option<Statement> {
		if !self.expect_peek(TokenType::Ident) {
			return None;
		}

		let stmt_name = self.cur_token.literal.clone().expect("Current token doesn't have a value");

		if !self.expect_peek(TokenType::Assign) {
			return None;
		}

		self.next_token();

		let mut value = self.parse_expression(Precedence::Lowest)?;

		if let Expression::Function { name, .. } = &mut value {
			*name = Some(stmt_name.clone());
		}

		if self.peek_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		Some(Statement::Let {
			name: Expression::Identifier(stmt_name),
			value,
		})
	}

	fn parse_return_statement(&mut self) -> Option<Statement> {
		self.next_token();

		let return_value = if !self.cur_token_is(TokenType::Semicolon) {
			self.parse_expression(Precedence::Lowest)
		}else {
			None
		};

		if self.peek_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		Some(Statement::Return (return_value))
	}

	fn parse_expression_statement(&mut self) -> Option<Statement> {
		let expression = self.parse_expression(Precedence::Lowest);

		if self.peek_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		Some(Statement::Expression(expression?))
	}

	fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
		let mut left_exp = match self.cur_token.token_type {
			TokenType::Ident => Some(self.parse_identifier()),
			TokenType::Int => self.parse_integer_literal(),
			TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
			TokenType::True | TokenType::False => Some(self.parse_boolean()),
			TokenType::LParen => self.parse_grouped_expression(),
			TokenType::If => self.parse_if_expression(),
			TokenType::Function => self.parse_function_literal(),
			TokenType::String => Some(self.parse_string_literal()),
			TokenType::LBracket => self.parse_array_literal(),
			TokenType::LBrace => self.parse_hash_literal(),
			t => {
				self.no_prefix_parse_fn_error(t);
				return None
			}
		}?;

		while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
			let infix = match self.peek_token.token_type {
				TokenType::Plus |
				TokenType::Minus |
				TokenType::Slash |
				TokenType::Asterisk |
				TokenType::Eq |
				TokenType::NotEq |
				TokenType::LT |
				TokenType::GT => Parser::parse_infix_expression,
				TokenType::LParen => Parser::parse_call_expression,
				TokenType::LBracket => Parser::parse_index_expression,
				_ => return Some(left_exp)
			};

			self.next_token();

			left_exp = infix(self, left_exp)?;
		}

		Some(left_exp)
	}

	fn parse_identifier(&self) -> Expression {
		Expression::Identifier(self.cur_token.literal.clone().unwrap())
	}

	fn parse_integer_literal(&mut self) -> Option<Expression> {
		match self.cur_token.literal.clone().unwrap().parse::<i64>() {
			Ok(value) => Some(Expression::Integer(value)),
			Err(err) => {
				self.errors.push(format!("could not parse {:?} as integer ({:?})", self.cur_token.literal, err));
				None
			}
		}
	}

	fn parse_boolean(&self) -> Expression {
		let value = match self.cur_token.token_type {
			TokenType::True => true,
			TokenType::False => false,
			_ => panic!("{:?} isn't a boolean token", self.cur_token)
		};

		Expression::Boolean(value)
	}

	fn parse_prefix_expression(&mut self) -> Option<Expression> {
		//TODO Try std::mem::take
		let operator = self.cur_token.literal.clone().unwrap();

		self.next_token();

		//TODO Throw parse error instead of option
		let right = self.parse_expression(Precedence::Prefix)?;

		Some(Expression::Prefix {
			operator,
			right: Box::new(right),
		})
	}

	fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
		//TODO Try std::mem::take
		let operator = self.cur_token.literal.clone().unwrap();

		let precedence = self.cur_precedence();
		self.next_token();
		//TODO Throw parse error instead of option
		let right = self.parse_expression(precedence)?;

		Some(Expression::Infix {
			left: Box::new(left),
			operator,
			right: Box::new(right),
		})
	}

	fn parse_grouped_expression(&mut self) -> Option<Expression> {
		self.next_token();

		let exp = self.parse_expression(Precedence::Lowest);

		if !self.expect_peek(TokenType::RParen) {
			return None;
		}

		return exp;
	}

	fn parse_if_expression(&mut self) -> Option<Expression> {
		//TODO Remove parentheses from if
		if !self.expect_peek(TokenType::LParen) {
			return None
		}

		self.next_token();
		let condition = self.parse_expression(Precedence::Lowest)?;

		if !self.expect_peek(TokenType::RParen) {
			return None
		}

		if !self.expect_peek(TokenType::LBrace) {
			return None
		}

		let consequence = self.parse_block_statement();

		let alternative = if self.peek_token_is(TokenType::Else) {
			self.next_token();

			if !self.expect_peek(TokenType::LBrace) {
				return None
			}

			Some(self.parse_block_statement())
		}else {
			None
		};

		Some(Expression::If {
			condition: Box::new(condition),
			consequence,
			alternative,
		})
	}

	fn parse_block_statement(&mut self) -> BlockStatement {
		self.next_token();

		let mut statements = vec![];

		//TODO Parse error on EOF
		while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::EOF) {
			if let Some(stmt) = self.parse_statement() {
				statements.push(stmt);
			}

			self.next_token();
		}

		BlockStatement { statements }
	}

	fn parse_function_literal(&mut self) -> Option<Expression> {
		if !self.expect_peek(TokenType::LParen) {
			return None
		}

		let parameters = self.parse_function_parameters()?;

		if !self.expect_peek(TokenType::LBrace) {
			return None
		}

		let body = self.parse_block_statement();

		Some(Expression::Function {
			parameters,
			body,
			name: None,
		})
	}

	fn parse_function_parameters(&mut self) -> Option<Vec<String>> {
		let mut identifiers = vec![];

		if self.peek_token_is(TokenType::RParen) {
			self.next_token();
			return Some(identifiers)
		}

		self.next_token();

		identifiers.push(self.cur_token.literal.clone().unwrap());

		while self.peek_token_is(TokenType::Comma) {
			self.next_token();
			self.next_token();

			identifiers.push(self.cur_token.literal.clone().unwrap());
		}

		if !self.expect_peek(TokenType::RParen) {
			return None;
		}

		Some(identifiers)
	}

	fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
		Some(Expression::Call {
			function: Box::new(function),
			arguments: self.parse_expression_list(TokenType::RParen)?,
		})
	}

	fn parse_string_literal(&self) -> Expression {
		Expression::String(self.cur_token.literal.clone().unwrap())
	}

	fn parse_array_literal(&mut self) -> Option<Expression> {
		Some(Expression::Array(self.parse_expression_list(TokenType::RBracket)?))
	}

	fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<Expression>> {
		let mut list = vec![];

		if self.peek_token_is(end) {
			self.next_token();
			return Some(list)
		}

		self.next_token();
		list.push(self.parse_expression(Precedence::Lowest)?);

		while self.peek_token_is(TokenType::Comma) {
			self.next_token();
			self.next_token();
			list.push(self.parse_expression(Precedence::Lowest)?);
		}

		if !self.expect_peek(end) {
			return None
		}

		Some(list)
	}

	fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
		self.next_token();
		let index = self.parse_expression(Precedence::Lowest)?;

		if !self.expect_peek(TokenType::RBracket) {
			return None
		}

		Some(Expression::Index {
			left: Box::new(left),
			index: Box::new(index),
		})
	}

	fn parse_hash_literal(&mut self) -> Option<Expression> {
		let mut pairs = vec![];

		//We could skip a peek_token_is with a manual loop, but probably not worth it
		while !self.peek_token_is(TokenType::RBrace) {
			self.next_token();

			let key = self.parse_expression(Precedence::Lowest)?;
			/*let key = if let Ok(key) = HashingExpression::try_from(self.parse_expression(Precedence::Lowest)?) {
				key
			}else {
				return None;
			};*/

			if !self.expect_peek(TokenType::Colon) {
				return None
			}

			self.next_token();
			let value = self.parse_expression(Precedence::Lowest)?;

			pairs.push((key, value));

			if !self.peek_token_is(TokenType::RBrace) && !self.expect_peek(TokenType::Comma) {
				return None
			}
		}

		if !self.expect_peek(TokenType::RBrace) {
			return None
		}

		Some(Expression::Hash(pairs))
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

	fn peek_precedence(&self) -> Precedence {
		precedences(self.peek_token.token_type).unwrap_or(Precedence::Lowest)
	}

	fn cur_precedence(&self) -> Precedence {
		precedences(self.cur_token.token_type).unwrap_or(Precedence::Lowest)
	}
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Copy, Clone)]
enum Precedence {
	Lowest,
	Equals,
	LessGreater,
	Sum,
	Product,
	Prefix,
	Call,
	Index,
}

const fn precedences(token_type: TokenType) -> Option<Precedence> {
	match token_type {
		TokenType::Eq |
		TokenType::NotEq => Some(Precedence::Equals),
		TokenType::LT |
		TokenType::GT => Some(Precedence::LessGreater),
		TokenType::Plus |
		TokenType::Minus => Some(Precedence::Sum),
		TokenType::Slash |
		TokenType::Asterisk => Some(Precedence::Product),
		TokenType::LParen => Some(Precedence::Call),
		TokenType::LBracket => Some(Precedence::Index),
		_ => None,
	}
}