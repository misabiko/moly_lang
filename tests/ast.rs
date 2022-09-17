use moly_lang::ast::{Expression, Program, Statement};

#[test]
fn test_string() {
	let program = Program {
		statements: vec![
			Statement::Let {
				name: "myVar".into(),
				value: Expression::Identifier("anotherVar".into())
			}
		]
	};

	assert_eq!(program.to_string(), "let myVar = anotherVar;");
}