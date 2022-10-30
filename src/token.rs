use core::fmt;
use core::fmt::Formatter;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
	pub token_type: TokenType,
	pub literal: TokenLiteral,
	pub position: usize,
	pub line: usize,
	pub after_whitespace: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenLiteral {
	Static(&'static str),
	String(String),
	//Minus is parsed as a separate token, so we store positive only
	Integer(usize),
	Float(usize, usize),
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

impl fmt::Display for TokenLiteral {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			TokenLiteral::Static(t) => t.fmt(f),
			TokenLiteral::String(t) => t.fmt(f),
			TokenLiteral::Integer(t) => t.fmt(f),
			TokenLiteral::Float(i, d) => write!(f, "{}.{}", i, d),
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
	//Could move to parser
	Float,

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
	And,
	Or,

	// Delimiters
	Comma,
	Semicolon,
	Colon,
	VBar,
	Dot,

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
	Unless,
	While,
	Until,
	For,

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
			TokenType::Float => "FLOAT",

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
			TokenType::And => "&&",
			TokenType::Or => "||",

			TokenType::Comma => ",",
			TokenType::Semicolon => ";",
			TokenType::Colon => ":",
			TokenType::VBar => "|",
			TokenType::Dot => ".",

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
			TokenType::Unless => "UNLESS",
			TokenType::While => "WHILE",
			TokenType::Until => "UNTIL",
			TokenType::For => "FOR",

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

pub fn lookup_ident(keyword: String) -> (TokenType, TokenLiteral) {
	match keyword.as_str() {
		"fn" => (
			TokenType::Function,
			TokenLiteral::Static("fn")
		),
		"let" => (
			TokenType::Let,
			TokenLiteral::Static("let")
		),
		"struct" => (
			TokenType::Struct,
			TokenLiteral::Static("struct")
		),
		"enum" => (
			TokenType::Enum,
			TokenLiteral::Static("enum")
		),
		"true" => (
			TokenType::True,
			TokenLiteral::Static("true")
		),
		"false" => (
			TokenType::False,
			TokenLiteral::Static("false")
		),
		"if" => (
			TokenType::If,
			TokenLiteral::Static("if")
		),
		"else" => (
			TokenType::Else,
			TokenLiteral::Static("else")
		),
		"unless" => (
			TokenType::Unless,
			TokenLiteral::Static("unless")
		),
		"while" => (
			TokenType::While,
			TokenLiteral::Static("while")
		),
		"until" => (
			TokenType::Until,
			TokenLiteral::Static("until")
		),
		"for" => (
			TokenType::For,
			TokenLiteral::Static("for")
		),
		"return" => (
			TokenType::Return,
			TokenLiteral::Static("return")
		),
		"u8" => (
			TokenType::IntegerType(IntType::U8),
			TokenLiteral::Static("u8")
		),
		"u16" => (
			TokenType::IntegerType(IntType::U16),
			TokenLiteral::Static("u16")
		),
		"u32" => (
			TokenType::IntegerType(IntType::U32),
			TokenLiteral::Static("u32")
		),
		"u64" => (
			TokenType::IntegerType(IntType::U64),
			TokenLiteral::Static("u64")
		),
		"i8" => (
			TokenType::IntegerType(IntType::I8),
			TokenLiteral::Static("i8")
		),
		"i16" => (
			TokenType::IntegerType(IntType::I16),
			TokenLiteral::Static("i16")
		),
		"i32" => (
			TokenType::IntegerType(IntType::I32),
			TokenLiteral::Static("i32")
		),
		"i64" => (
			TokenType::IntegerType(IntType::I64),
			TokenLiteral::Static("i64")
		),
		"bool" => (
			TokenType::Bool,
			TokenLiteral::Static("bool")
		),
		"str" => (
			TokenType::Str,
			TokenLiteral::Static("str")
		),
		_ => (TokenType::Ident,
			  TokenLiteral::String(keyword)
		),
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