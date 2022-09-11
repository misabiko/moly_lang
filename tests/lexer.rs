use moly_lang::{
	lexer::Lexer,
	token::TokenType,
};

struct TestStruct {
	expected_type: TokenType,
	expected_literal: Option<String>,
}

#[test]
fn test_next_token() {
	const INPUT: &str = "let five = 5
let ten = 10

let add = fn(x, y) {
  x + y
}

let result = add(five, ten)

!-/*5;
5 < 10 > 5

if 5 < 10 {
    return true
} else {
    return false
}

10 == 10
10 != 9
";

	let tests = vec![
		(TokenType::Let, Some("let")),
		(TokenType::Ident, Some("five")),
		(TokenType::Assing, Some("=")),
		(TokenType::Int, Some("5")),
		(TokenType::Let, Some("let")),
		(TokenType::Ident, Some("ten")),
		(TokenType::Assing, Some("=")),
		(TokenType::Int, Some("10")),
		(TokenType::Let, Some("let")),
		(TokenType::Ident, Some("add")),
		(TokenType::Assing, Some("=")),
		(TokenType::Function, Some("fn")),
		(TokenType::LParen, Some("(")),
		(TokenType::Ident, Some("x")),
		(TokenType::Comma, Some(",")),
		(TokenType::Ident, Some("y")),
		(TokenType::RParen, Some(")")),
		(TokenType::LBrace, Some("{")),
		(TokenType::Ident, Some("x")),
		(TokenType::Plus, Some("+")),
		(TokenType::Ident, Some("y")),
		(TokenType::RBrace, Some("}")),
		(TokenType::Let, Some("let")),
		(TokenType::Ident, Some("result")),
		(TokenType::Assing, Some("=")),
		(TokenType::Ident, Some("add")),
		(TokenType::LParen, Some("(")),
		(TokenType::Ident, Some("five")),
		(TokenType::Comma, Some(",")),
		(TokenType::Ident, Some("ten")),
		(TokenType::RParen, Some(")")),
		(TokenType::Bang, Some("!")),
		(TokenType::Minus, Some("-")),
		(TokenType::Slash, Some("/")),
		(TokenType::Asterisk, Some("*")),
		(TokenType::Int, Some("5")),
		(TokenType::Semicolon, Some(";")),
		(TokenType::Int, Some("5")),
		(TokenType::LT, Some("<")),
		(TokenType::Int, Some("10")),
		(TokenType::GT, Some(">")),
		(TokenType::Int, Some("5")),
		(TokenType::If, Some("if")),
		(TokenType::Int, Some("5")),
		(TokenType::LT, Some("<")),
		(TokenType::Int, Some("10")),
		(TokenType::LBrace, Some("{")),
		(TokenType::Return, Some("return")),
		(TokenType::True, Some("true")),
		(TokenType::RBrace, Some("}")),
		(TokenType::Else, Some("else")),
		(TokenType::LBrace, Some("{")),
		(TokenType::Return, Some("return")),
		(TokenType::False, Some("false")),
		(TokenType::RBrace, Some("}")),
		(TokenType::Int, Some("10")),
		(TokenType::Eq, Some("==")),
		(TokenType::Int, Some("10")),
		(TokenType::Int, Some("10")),
		(TokenType::NotEq, Some("!=")),
		(TokenType::Int, Some("9")),
		(TokenType::EOF, None),
	].into_iter()
		.map(|(t, l)| TestStruct{
			expected_type: t,
			expected_literal: l.map(|l| l.to_owned())
		});

	let mut lexer = Lexer::new(INPUT);

	for test in tests {
		let token = lexer.next_token();

		assert_eq!(token.token_type, test.expected_type);

		assert_eq!(token.literal, test.expected_literal);
	}
	let result = 2 + 2;
	assert_eq!(result, 4);
}