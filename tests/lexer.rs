use moly_lang::{
	lexer::Lexer,
	token::TokenType,
};
use moly_lang::token::TokenLiteral;

#[test]
fn test_next_token() {
	//TODO Remove if parentheses
	const INPUT: &str = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if 5 < 10 {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
"foobar"
"foo bar"
[1, 2];
{"foo": "bar"}
"#;

	let tests = vec![
		(TokenType::Let, TokenLiteral::Static("let")),
		(TokenType::Ident, TokenLiteral::String("five".into())),
		(TokenType::Assign, TokenLiteral::Static("=")),
		(TokenType::Int, TokenLiteral::Integer(5)),
		(TokenType::Semicolon, TokenLiteral::Static(";")),
		(TokenType::Let, TokenLiteral::Static("let")),
		(TokenType::Ident, TokenLiteral::String("ten".into())),
		(TokenType::Assign, TokenLiteral::Static("=")),
		(TokenType::Int, TokenLiteral::Integer(10)),
		(TokenType::Semicolon, TokenLiteral::Static(";")),
		(TokenType::Let, TokenLiteral::Static("let")),
		(TokenType::Ident, TokenLiteral::String("add".into())),
		(TokenType::Assign, TokenLiteral::Static("=")),
		(TokenType::Function, TokenLiteral::Static("fn")),
		(TokenType::LParen, TokenLiteral::Static("(")),
		(TokenType::Ident, TokenLiteral::String("x".into())),
		(TokenType::Comma, TokenLiteral::Static(",")),
		(TokenType::Ident, TokenLiteral::String("y".into())),
		(TokenType::RParen, TokenLiteral::Static(")")),
		(TokenType::LBrace, TokenLiteral::Static("{")),
		(TokenType::Ident, TokenLiteral::String("x".into())),
		(TokenType::Plus, TokenLiteral::Static("+")),
		(TokenType::Ident, TokenLiteral::String("y".into())),
		(TokenType::Semicolon, TokenLiteral::Static(";")),
		(TokenType::RBrace, TokenLiteral::Static("}")),
		(TokenType::Semicolon, TokenLiteral::Static(";")),
		(TokenType::Let, TokenLiteral::Static("let")),
		(TokenType::Ident, TokenLiteral::String("result".into())),
		(TokenType::Assign, TokenLiteral::Static("=")),
		(TokenType::Ident, TokenLiteral::String("add".into())),
		(TokenType::LParen, TokenLiteral::Static("(")),
		(TokenType::Ident, TokenLiteral::String("five".into())),
		(TokenType::Comma, TokenLiteral::Static(",")),
		(TokenType::Ident, TokenLiteral::String("ten".into())),
		(TokenType::RParen, TokenLiteral::Static(")")),
		(TokenType::Semicolon, TokenLiteral::Static(";")),
		(TokenType::Bang, TokenLiteral::Static("!")),
		(TokenType::Minus, TokenLiteral::Static("-")),
		(TokenType::Slash, TokenLiteral::Static("/")),
		(TokenType::Asterisk, TokenLiteral::Static("*")),
		(TokenType::Int, TokenLiteral::Integer(5)),
		(TokenType::Semicolon, TokenLiteral::Static(";")),
		(TokenType::Int, TokenLiteral::Integer(5)),
		(TokenType::LT, TokenLiteral::Static("<")),
		(TokenType::Int, TokenLiteral::Integer(10)),
		(TokenType::GT, TokenLiteral::Static(">")),
		(TokenType::Int, TokenLiteral::Integer(5)),
		(TokenType::Semicolon, TokenLiteral::Static(";")),
		(TokenType::If, TokenLiteral::Static("if")),
		(TokenType::Int, TokenLiteral::Integer(5)),
		(TokenType::LT, TokenLiteral::Static("<")),
		(TokenType::Int, TokenLiteral::Integer(10)),
		(TokenType::LBrace, TokenLiteral::Static("{")),
		(TokenType::Return, TokenLiteral::Static("return")),
		(TokenType::True, TokenLiteral::Static("true")),
		(TokenType::Semicolon, TokenLiteral::Static(";")),
		(TokenType::RBrace, TokenLiteral::Static("}")),
		(TokenType::Else, TokenLiteral::Static("else")),
		(TokenType::LBrace, TokenLiteral::Static("{")),
		(TokenType::Return, TokenLiteral::Static("return")),
		(TokenType::False, TokenLiteral::Static("false")),
		(TokenType::Semicolon, TokenLiteral::Static(";")),
		(TokenType::RBrace, TokenLiteral::Static("}")),
		(TokenType::Int, TokenLiteral::Integer(10)),
		(TokenType::Eq, TokenLiteral::Static("==")),
		(TokenType::Int, TokenLiteral::Integer(10)),
		(TokenType::Semicolon, TokenLiteral::Static(";")),
		(TokenType::Int, TokenLiteral::Integer(10)),
		(TokenType::NotEq, TokenLiteral::Static("!=")),
		(TokenType::Int, TokenLiteral::Integer(9)),
		(TokenType::Semicolon, TokenLiteral::Static(";")),
		(TokenType::String, TokenLiteral::String("foobar".into())),
		(TokenType::String, TokenLiteral::String("foo bar".into())),
		(TokenType::LBracket, TokenLiteral::Static("[")),
		(TokenType::Int, TokenLiteral::Integer(1)),
		(TokenType::Comma, TokenLiteral::Static(",")),
		(TokenType::Int, TokenLiteral::Integer(2)),
		(TokenType::RBracket, TokenLiteral::Static("]")),
		(TokenType::Semicolon, TokenLiteral::Static(";")),
		(TokenType::LBrace, TokenLiteral::Static("{")),
		(TokenType::String, TokenLiteral::String("foo".into())),
		(TokenType::Colon, TokenLiteral::Static(":")),
		(TokenType::String, TokenLiteral::String("bar".into())),
		(TokenType::RBrace, TokenLiteral::Static("}")),
		(TokenType::EOF, TokenLiteral::Static("")),
	];

	let mut lexer = Lexer::new(INPUT);

	for (expected_type, expected_literal) in tests {
		let token = lexer.next_token();

		assert_eq!(token.token_type, expected_type);

		assert_eq!(token.literal, expected_literal);
	}
	let result = 2 + 2;
	assert_eq!(result, 4);
}