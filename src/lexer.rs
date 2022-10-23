use crate::token::{lookup_ident, Token, TokenLiteral, TokenType};

pub struct Lexer {
	//TODO Could be Chars
	input: String,
	/// current position in input (points to current char)
	position: usize,
	/// current reading position in input (after current char)
	read_position: usize,
	/// current char under examination
	ch: Option<char>,
}

impl Lexer {
	pub fn new(input: &str) -> Self {
		let mut lexer = Self {
			input: input.to_owned(),
			position: 0,
			read_position: 0,
			ch: None,
		};

		lexer.read_char();

		return lexer;
	}

	//TODO read runes instead of ascii chars
	pub fn read_char(&mut self) {
		if self.read_position >= self.input.len() {
			self.ch = None;
		} else {
			self.ch = self.input.chars().nth(self.read_position);
		}

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
			}else {
				return token
			}
		}
	}

	pub fn next_token_with_comments(&mut self) -> Token {
		let after_whitespace = self.skip_whitespace();

		let token = match self.ch {
			Some(ch) => match ch {
				'=' => if self.peek_char() == Some('=') {
					self.read_char();
					Token { token_type: TokenType::Eq, literal: TokenLiteral::Static("=="), after_whitespace }
				} else {
					Token { token_type: TokenType::Assign, literal: TokenLiteral::Static("="), after_whitespace }
				},
				//TODO += -= /= *= %=
				'+' => Token { token_type: TokenType::Plus, literal: TokenLiteral::Static("+"), after_whitespace },
				'-' => Token { token_type: TokenType::Minus, literal: TokenLiteral::Static("-"), after_whitespace },
				'!' => if self.peek_char() == Some('=') {
					self.read_char();
					Token { token_type: TokenType::NotEq, literal: TokenLiteral::Static("!="), after_whitespace }
				} else {
					Token { token_type: TokenType::Bang, literal: TokenLiteral::Static("!"), after_whitespace }
				},
				'*' => Token { token_type: TokenType::Asterisk, literal: TokenLiteral::Static("*"), after_whitespace },
				'/' => match self.peek_char() {
					Some('=') => {
						self.read_char();
						Token { token_type: TokenType::Eq, literal: TokenLiteral::Static("=="), after_whitespace }
					}
					Some('/') => {
						self.read_char();
						Token { token_type: TokenType::LineComment, literal: TokenLiteral::String(self.read_line_comment()), after_whitespace }
					}
					Some('*') => {
						self.read_char();
						Token { token_type: TokenType::MultilineComment, literal: TokenLiteral::String(self.read_multiline_comment()), after_whitespace }
					}
					_ => Token { token_type: TokenType::Slash, literal: TokenLiteral::Static("/"), after_whitespace }
				},
				//TODO %
				//TODO <= >=
				'<' => Token { token_type: TokenType::LT, literal: TokenLiteral::Static("<"), after_whitespace },
				'>' => Token { token_type: TokenType::GT, literal: TokenLiteral::Static(">"), after_whitespace },
				',' => Token { token_type: TokenType::Comma, literal: TokenLiteral::Static(","), after_whitespace },
				';' => Token { token_type: TokenType::Semicolon, literal: TokenLiteral::Static(";"), after_whitespace },
				':' => Token { token_type: TokenType::Colon, literal: TokenLiteral::Static(":"), after_whitespace },
				'(' => Token { token_type: TokenType::LParen, literal: TokenLiteral::Static("("), after_whitespace },
				')' => Token { token_type: TokenType::RParen, literal: TokenLiteral::Static(")"), after_whitespace },
				'{' => Token { token_type: TokenType::LBrace, literal: TokenLiteral::Static("{"), after_whitespace },
				'}' => Token { token_type: TokenType::RBrace, literal: TokenLiteral::Static("}"), after_whitespace },
				'[' => Token { token_type: TokenType::LBracket, literal: TokenLiteral::Static("["), after_whitespace },
				']' => Token { token_type: TokenType::RBracket, literal: TokenLiteral::Static("]"), after_whitespace },
				'"' => Token { token_type: TokenType::String, literal: TokenLiteral::String(self.read_string()), after_whitespace },
				_ => if is_letter(self.ch) {
					//Returning early to skip the read_char() at the end
					return lookup_ident(self.read_identifier(), after_whitespace);
				} else if is_digit(self.ch) {
					return Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(self.read_number()), after_whitespace };
				} else {
					Token { token_type: TokenType::Illegal, literal: TokenLiteral::Static(""), after_whitespace }
				}
			}
			None => Token { token_type: TokenType::EOF, literal: TokenLiteral::Static(""), after_whitespace },
		};

		self.read_char();

		token
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

	fn read_number(&mut self) -> usize {
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
		}else {
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
				},
				(Some('*'), Some('/')) => {
					self.read_char();
					if nest_levels == 0 {
						break;
					}else {
						nest_levels -= 1;
					}
				},
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