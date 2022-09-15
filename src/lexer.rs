use crate::token::{lookup_ident, Token, TokenType};

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

		return lexer
	}

	//TODO read runes instead of ascii chars
	pub fn read_char(&mut self) {
		if self.read_position >= self.input.len() {
			self.ch = None;
		}else {
			self.ch = self.input.chars().nth(self.read_position);
		}

		self.position = self.read_position;
		self.read_position += 1;
	}

	pub fn peek_char(&self) -> Option<char> {
		self.input.chars().nth(self.read_position)
	}

	pub fn next_token(&mut self) -> Token {
		self.skip_whitespace();

		let token = match self.ch {
			Some(ch) => match ch {
				//TODO Order with TokenType
				'=' => if self.peek_char() == Some('=') {
					self.read_char();
					new_token(TokenType::Eq, Some("==".to_owned()))
				} else {
					new_token(TokenType::Assign, Some(ch.to_string()))
				},
				'+' => new_token(TokenType::Plus, Some(ch.to_string())),
				'-' => new_token(TokenType::Minus, Some(ch.to_string())),
				'!' => if self.peek_char() == Some('=') {
					self.read_char();
					new_token(TokenType::NotEq, Some("!=".to_owned()))
				} else {
					new_token(TokenType::Bang, Some(ch.to_string()))
				},
				'/' => new_token(TokenType::Slash, Some(ch.to_string())),
				'*' => new_token(TokenType::Asterisk, Some(ch.to_string())),
				'<' => new_token(TokenType::LT, Some(ch.to_string())),
				'>' => new_token(TokenType::GT, Some(ch.to_string())),
				';' => new_token(TokenType::Semicolon, Some(ch.to_string())),
				':' => new_token(TokenType::Colon, Some(ch.to_string())),
				'(' => new_token(TokenType::LParen, Some(ch.to_string())),
				')' => new_token(TokenType::RParen, Some(ch.to_string())),
				',' => new_token(TokenType::Comma, Some(ch.to_string())),
				'{' => new_token(TokenType::LBrace, Some(ch.to_string())),
				'}' => new_token(TokenType::RBrace, Some(ch.to_string())),
				'[' => new_token(TokenType::LBracket, Some(ch.to_string())),
				']' => new_token(TokenType::RBracket, Some(ch.to_string())),
				'"' => new_token(TokenType::String, Some(self.read_string())),
				_ => if is_letter(self.ch) {
					let literal = self.read_identifier();
					//Returning early to skip the read_char() at the end
					//TODO return the whole if
					return new_token(lookup_ident(literal.as_str()), Some(literal))
				} else if is_digit(self.ch) {
					return new_token(TokenType::Int, Some(self.read_number()))
				} else {
					new_token(TokenType::Illegal, None)
				}
			}
			None => new_token(TokenType::EOF, None),
		};

		self.read_char();

		token
	}

	fn skip_whitespace(&mut self) {
		while matches!(self.ch, Some(' ' | '\t' | '\n' | '\r')) {
			self.read_char()
		}
	}

	fn read_identifier(&mut self) -> String {
		let position = self.position;
		while is_letter(self.ch) {
			self.read_char();
		}
		self.input.chars()
			.skip(position)
			.take(self.position - position)
			.collect()
	}

	fn read_number(&mut self) -> String {
		let position = self.position;
		while is_digit(self.ch) {
			self.read_char();
		}
		self.input.chars()
			.skip(position)
			.take(self.position - position)
			.collect::<String>()
	}

	fn read_string(&mut self) -> String {
		let position = self.position + 1;
		loop {
			self.read_char();
			if let Some('"') | None = self.ch {
				break;
			}
		}
		self.input.chars()
			.skip(position)
			.take(self.position - position)
			.collect()
	}
}

//TODO Dissolve new_token?
fn new_token(token_type: TokenType, literal: Option<String>) -> Token {
	Token {
		token_type,
		literal,
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