use moly::ast::{Expression, Statement, StatementBlock};

#[test]
fn test_string() {
	let program = StatementBlock(vec![
			Statement::Let {
				name: "myVar".into(),
				value: Expression::Identifier("anotherVar".into())
			}
		]
	);

	assert_eq!(program.to_string(), "let myVar = anotherVar;");
}