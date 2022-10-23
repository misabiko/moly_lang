use core::fmt;
use core::fmt::Formatter;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
	pub token_type: TokenType,
	pub literal: TokenLiteral,
	pub after_whitespace: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenLiteral {
	Static(&'static str),
	String(String),
	//Minus is parsed as a separate token, so we store positive only
	Integer(usize),
}

impl TokenLiteral {
	pub fn get_static(&self) -> Option<&'static str> {
		if let TokenLiteral::Static(literal) = self {
			Some(literal)
		}else {
			None
		}
	}

	pub fn get_string(&self) -> Option<&String> {
		if let TokenLiteral::String(literal) = self {
			Some(literal)
		}else {
			None
		}
	}

	pub fn get_integer(&self) -> Option<&usize> {
		if let TokenLiteral::Integer(literal) = self {
			Some(literal)
		}else {
			None
		}
	}
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum TokenType {
	Illegal,
	EOF,

	// Identifiers + literals
	Ident, // add, foobar, x, y, ...
	Int,  // 1343456

	// Operators
	Assign,
	Plus,
	Minus,
	Bang,
	Asterisk,
	Slash,

	LT,
	GT,
	Eq,
	NotEq,

	// Delimiters
	Comma,
	Semicolon,
	Colon,
	VBar,

	LParen,
	RParen,
	LBrace,
	RBrace,
	LBracket,
	RBracket,
	BigRightArrow,

	// Keywords
	Function,
	Let,
	Struct,
	Enum,
	True,
	False,
	If,
	Else,
	Return,
	IntegerType(IntType),
	Bool,
	Str,

	String,
	LineComment,
	MultilineComment,
}

impl fmt::Display for TokenType {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			TokenType::Illegal => "ILLEGAL",
			TokenType::EOF => "EOF",

			TokenType::Ident => "IDENT",
			TokenType::Int => "INT",

			TokenType::Assign => "=",
			TokenType::Plus => "+",
			TokenType::Minus => "-",
			TokenType::Bang => "!",
			TokenType::Asterisk => "*",
			TokenType::Slash => "/",

			TokenType::LT => "<",
			TokenType::GT => ">",
			TokenType::Eq => "==",
			TokenType::NotEq => "!=",

			TokenType::Comma => ",",
			TokenType::Semicolon => ";",
			TokenType::Colon => ":",
			TokenType::VBar => "|",

			TokenType::LParen => "(",
			TokenType::RParen => ")",
			TokenType::LBrace => "{",
			TokenType::RBrace => "}",
			TokenType::LBracket => "[",
			TokenType::RBracket => "]",
			TokenType::BigRightArrow => "=>",

			TokenType::Function => "FUNCTION",
			TokenType::Let => "LET",
			TokenType::Struct => "STRUCT",
			TokenType::Enum => "ENUM",
			TokenType::True => "TRUE",
			TokenType::False => "FALSE",
			TokenType::If => "IF",
			TokenType::Else => "ELSE",
			TokenType::Return => "RETURN",
			TokenType::IntegerType(IntType::U8) => "U8",
			TokenType::IntegerType(IntType::U16) => "U16",
			TokenType::IntegerType(IntType::U32) => "U32",
			TokenType::IntegerType(IntType::U64) => "U64",
			TokenType::IntegerType(IntType::I8) => "I8",
			TokenType::IntegerType(IntType::I16) => "I16",
			TokenType::IntegerType(IntType::I32) => "I32",
			TokenType::IntegerType(IntType::I64) => "I64",
			TokenType::Bool => "BOOL",
			TokenType::Str => "STR",

			TokenType::String => "STRING",
			TokenType::LineComment => "LINECOMMENT",
			TokenType::MultilineComment => "MULTILINECOMMENT",
		})
	}
}

pub fn lookup_ident(keyword: String, after_whitespace: bool) -> Token {
	match keyword.as_str() {
		"fn" => Token {
			token_type: TokenType::Function,
			literal: TokenLiteral::Static("fn"),
			after_whitespace,
		},
		"let" => Token {
			token_type: TokenType::Let,
			literal: TokenLiteral::Static("let"),
			after_whitespace,
		},
		"struct" => Token {
			token_type: TokenType::Struct,
			literal: TokenLiteral::Static("struct"),
			after_whitespace,
		},
		"enum" => Token {
			token_type: TokenType::Enum,
			literal: TokenLiteral::Static("enum"),
			after_whitespace,
		},
		"true" => Token {
			token_type: TokenType::True,
			literal: TokenLiteral::Static("true"),
			after_whitespace,
		},
		"false" => Token {
			token_type: TokenType::False,
			literal: TokenLiteral::Static("false"),
			after_whitespace,
		},
		"if" => Token {
			token_type: TokenType::If,
			literal: TokenLiteral::Static("if"),
			after_whitespace,
		},
		"else" => Token {
			token_type: TokenType::Else,
			literal: TokenLiteral::Static("else"),
			after_whitespace,
		},
		"return" => Token {
			token_type: TokenType::Return,
			literal: TokenLiteral::Static("return"),
			after_whitespace,
		},
		"u8" => Token {
			token_type: TokenType::IntegerType(IntType::U8),
			literal: TokenLiteral::Static("u8"),
			after_whitespace,
		},
		"u16" => Token {
			token_type: TokenType::IntegerType(IntType::U16),
			literal: TokenLiteral::Static("u16"),
			after_whitespace,
		},
		"u32" => Token {
			token_type: TokenType::IntegerType(IntType::U32),
			literal: TokenLiteral::Static("u32"),
			after_whitespace,
		},
		"u64" => Token {
			token_type: TokenType::IntegerType(IntType::U64),
			literal: TokenLiteral::Static("u64"),
			after_whitespace,
		},
		"i8" => Token {
			token_type: TokenType::IntegerType(IntType::I8),
			literal: TokenLiteral::Static("i8"),
			after_whitespace,
		},
		"i16" => Token {
			token_type: TokenType::IntegerType(IntType::I16),
			literal: TokenLiteral::Static("i16"),
			after_whitespace,
		},
		"i32" => Token {
			token_type: TokenType::IntegerType(IntType::I32),
			literal: TokenLiteral::Static("i32"),
			after_whitespace,
		},
		"i64" => Token {
			token_type: TokenType::IntegerType(IntType::I64),
			literal: TokenLiteral::Static("i64"),
			after_whitespace,
		},
		"bool" => Token {
			token_type: TokenType::Bool,
			literal: TokenLiteral::Static("bool"),
			after_whitespace,
		},
		"str" => Token {
			token_type: TokenType::Str,
			literal: TokenLiteral::Static("str"),
			after_whitespace,
		},
		_ => Token {
			token_type: TokenType::Ident,
			literal: TokenLiteral::String(keyword),
			after_whitespace,
		},
	}
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum IntType {
	U8,
	U16,
	U32,
	U64,
	I8,
	I16,
	I32,
	I64,
}