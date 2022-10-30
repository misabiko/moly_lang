use crate::token::{lookup_ident, Token, TokenLiteral, TokenType};

pub struct Lexer {
	//TODO Could be Chars
	pub input: String,
	/// current position in input (points to current char)
	position: usize,
	/// current reading position in input (after current char)
	read_position: usize,
	/// current char under examination
	ch: Option<char>,
	pub line: usize,
}

impl Lexer {
	pub fn new(input: &str) -> Self {
		let mut lexer = Self {
			input: input.to_owned(),
			position: 0,
			read_position: 0,
			ch: None,
			line: 0,
		};

		lexer.read_char();

		return lexer;
	}

	pub fn read_char(&mut self) {
		if let Some('\n') = self.ch {
			self.line += 1;
		}

		self.ch = self.input.chars().nth(self.read_position);

		self.position = self.read_position;
		self.read_position += 1;
	}

	pub fn peek_char(&self) -> Option<char> {
		self.input.chars().nth(self.read_position)
	}

	pub fn next_token(&mut self) -> Token {
		let mut token = self.next_token_with_comments();
		loop {
			if let TokenType::LineComment | TokenType::MultilineComment = token.token_type {
				token = self.next_token_with_comments();
			} else {
				return token;
			}
		}
	}

	pub fn next_token_with_comments(&mut self) -> Token {
		let after_whitespace = self.skip_whitespace();

		let position = self.position;
		let line = self.line;

		let (token_type, literal) = match self.ch {
			Some(ch) => match ch {
				'=' => match self.peek_char() {
					Some('=') => {
						self.read_char();
						(TokenType::Eq, TokenLiteral::Static("=="))
					}
					Some('>') => {
						self.read_char();
						(TokenType::BigRightArrow, TokenLiteral::Static("=>"))
					}
					_ => (TokenType::Assign, TokenLiteral::Static("="))
				},
				//TODO += -= /= *= %=
				'+' => (TokenType::Plus, TokenLiteral::Static("+")),
				'-' => (TokenType::Minus, TokenLiteral::Static("-")),
				'!' => if self.peek_char() == Some('=') {
					self.read_char();
					(TokenType::NotEq, TokenLiteral::Static("!="))
				} else {
					(TokenType::Bang, TokenLiteral::Static("!"))
				},
				'*' => (TokenType::Asterisk, TokenLiteral::Static("*")),
				'/' => match self.peek_char() {
					Some('=') => {
						self.read_char();
						(TokenType::Eq, TokenLiteral::Static("=="))
					}
					Some('/') => {
						self.read_char();
						(TokenType::LineComment, TokenLiteral::String(self.read_line_comment()))
					}
					Some('*') => {
						self.read_char();
						(TokenType::MultilineComment, TokenLiteral::String(self.read_multiline_comment()))
					}
					_ => (TokenType::Slash, TokenLiteral::Static("/"))
				},
				//TODO %
				//TODO <= >=
				'<' => (TokenType::LT, TokenLiteral::Static("<")),
				'>' => (TokenType::GT, TokenLiteral::Static(">")),
				'&' => if self.peek_char() == Some('&') {
					self.read_char();
					(TokenType::And, TokenLiteral::Static("&&"))
				} else {
					(TokenType::Illegal, TokenLiteral::Static(""))
				},
				',' => (TokenType::Comma, TokenLiteral::Static(",")),
				';' => (TokenType::Semicolon, TokenLiteral::Static(";")),
				':' => (TokenType::Colon, TokenLiteral::Static(":")),
				'|' => if self.peek_char() == Some('|') {
					self.read_char();
					(TokenType::Or, TokenLiteral::Static("||"))
				} else {
					(TokenType::VBar, TokenLiteral::Static("|"))
				},
				'.' => (TokenType::Dot, TokenLiteral::Static(".")),
				'(' => (TokenType::LParen, TokenLiteral::Static("(")),
				')' => (TokenType::RParen, TokenLiteral::Static(")")),
				'{' => (TokenType::LBrace, TokenLiteral::Static("{")),
				'}' => (TokenType::RBrace, TokenLiteral::Static("}")),
				'[' => (TokenType::LBracket, TokenLiteral::Static("[")),
				']' => (TokenType::RBracket, TokenLiteral::Static("]")),
				'"' => (TokenType::String, TokenLiteral::String(self.read_string())),
				_ => if is_letter(self.ch) {
					//Returning early to skip the read_char() at the end
					let (token_type, literal) = lookup_ident(self.read_identifier());
					return Token {
						token_type,
						literal,
						position,
						line,
						after_whitespace,
					};
				} else if is_digit(self.ch) {
					return self.read_number(after_whitespace);
				} else {
					(TokenType::Illegal, TokenLiteral::Static(""))
				}
			}
			None => (TokenType::EOF, TokenLiteral::Static("")),
		};

		self.read_char();

		Token {
			token_type,
			literal,
			position,
			line,
			after_whitespace,
		}
	}

	fn skip_whitespace(&mut self) -> bool {
		let mut after_whitespace = false;
		while matches!(self.ch, Some(' ' | '\t' | '\n' | '\r')) {
			self.read_char();
			after_whitespace = true;
		}

		after_whitespace
	}

	fn read_identifier(&mut self) -> String {
		let position = self.position;
		while is_letter(self.ch) || is_digit(self.ch) {
			self.read_char();
		}
		self.input.chars()
			.skip(position)
			.take(self.position - position)
			.collect()
	}

	fn read_number(&mut self, after_whitespace: bool) -> Token {
		let position = self.position;
		let line = self.line;
		let number = self.read_integer();

		if self.ch == Some('.') && is_digit(self.peek_char()) {
			self.read_char();
			let decimals = self.read_integer();

			Token {
				token_type: TokenType::Float,
				literal: TokenLiteral::Float(number, decimals),
				position,
				line,
				after_whitespace,
			}
		} else {
			Token {
				token_type: TokenType::Int,
				literal: TokenLiteral::Integer(number),
				position,
				line,
				after_whitespace,
			}
		}
	}

	fn read_integer(&mut self) -> usize {
		let position = self.position;
		while is_digit(self.ch) {
			self.read_char();
		}
		self.input.chars()
			.skip(position)
			.take(self.position - position)
			.collect::<String>()
			.parse()
			.expect("failed to parse integer")
	}

	fn read_string(&mut self) -> String {
		let position = self.position + 1;
		loop {
			self.read_char();
			if let Some('"') | None = self.ch {
				//TODO Throw on EOF mid string
				break;
			}
		}
		self.input.chars()
			.skip(position)
			.take(self.position - position)
			.collect()
	}

	fn read_line_comment(&mut self) -> String {
		let position = self.position + 1;
		loop {
			if let Some('\n') | None = self.peek_char() {
				break;
			}
			self.read_char();
		}

		if position > self.position {
			"".into()
		} else {
			self.input.chars()
				.skip(position)
				.take(self.position - position + 1)
				.collect()
		}
	}

	fn read_multiline_comment(&mut self) -> String {
		let position = self.position + 1;
		let mut nest_levels = 0;

		loop {
			self.read_char();
			match (self.ch, self.peek_char()) {
				//TODO Throw on EOF mid multiline comment
				(None, _) => {}
				(Some('/'), Some('*')) => {
					nest_levels += 1;
					self.read_char();
				}
				(Some('*'), Some('/')) => {
					self.read_char();
					if nest_levels == 0 {
						break;
					} else {
						nest_levels -= 1;
					}
				}
				_ => {}
			}
		}
		self.input.chars()
			.skip(position)
			.take(self.position - position - 1)
			.collect()
	}
}

//TODO Add more cases for emojis and other stuff in identifiers
fn is_letter(ch: Option<char>) -> bool {
	matches!(ch, Some(
		'a' ..= 'z' |
		'A' ..= 'Z' |
		'_'
	))
}

//TODO Support _
fn is_digit(ch: Option<char>) -> bool {
	matches!(ch, Some('0' ..= '9'))
}