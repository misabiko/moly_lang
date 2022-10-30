use moly_lib::ast::{Expression, IntExpr};
use moly_lib::lexer::Lexer;
use moly_lib::MolyError;
use moly_lib::parser::Parser;
use moly_lib::reporting::show_error;
use moly_lib::token::TokenType;
use moly_lib::type_checker::type_env::TypeId;
use moly_lib::type_checker::TypeChecker;
use moly_lib::type_checker::typed_ast::TypedExpression;

#[test]
fn test_anon_trait_check_expr() {
	let mut type_checker = TypeChecker::new();

	let result = type_checker.check_expression(Expression::Field {
		left: Box::new(Expression::Array(
			vec![
				Expression::Integer(IntExpr::U8(1)),
			],
		)),
		field: "push".into(),
	});

	let array_type = TypeId::Array(Box::new(TypeId::U8));
	let push_type = TypeId::Function {
		parameters: vec![
			array_type.clone(),
			TypeId::U8,
		],
		return_type: Box::new(array_type),
		is_method: true
	};
	assert_eq!(result, Ok((
		TypedExpression::Field {
			left: Box::new(TypedExpression::Array {
				elements: vec![TypedExpression::Integer(IntExpr::U8(1))],
				type_id: TypeId::U8
			}),
			field: "push".to_string(),
			left_type: TypeId::Array(Box::new(TypeId::U8)),
			field_type: push_type.clone(),
			binding_index: None
		},
		push_type,
	)));
}

#[test]
fn test_anon_trait_params() {
	let input = "[1].push(2)";

	let program = Parser::new(Lexer::new(input)).parse_block_statement(TokenType::EOF);
	let program = match program {
		Ok(p) => p,
		Err(err) => {
			eprintln!("{}", show_error(MolyError::Parse(err.clone()), input.into()));
			panic!("test {}: parse error: {}", 0, err)
		}
	};

	/*let program = StatementBlock(
		vec![
			Statement::Expression {
				expr: Expression::Call {
					function: Box::new(Expression::Field {
						left: Box::new(Expression::Array(
							vec![
								Expression::Integer(IntExpr::U8(1)),
							],
						)),
						field: "push".into(),
					}),
					arguments: vec![
						Expression::Integer(IntExpr::U8(2)),
					],
				},
				has_semicolon: false,
			},
		],
	);*/

	let mut type_checker = TypeChecker::new();

	/*let result = type_checker.check_block(program, true, false);

	let array_type = TypeId::Array(Box::new(TypeId::U8));
	let push_type = TypeId::Function {
		parameters: vec![
			array_type.clone(),
			TypeId::U8,
		],
		return_type: Box::new(array_type),
		is_method: true
	};
	assert_eq!(result, Ok((
		TypedExpression::Field {
			left: Box::new(TypedExpression::Array {
				elements: vec![TypedExpression::Integer(IntExpr::U8(1))],
				type_id: TypeId::U8
			}),
			field: "push".to_string(),
			left_type: TypeId::Array(Box::new(TypeId::U8)),
			field_type: push_type.clone(),
			binding_index: None
		},
		push_type,
	)));*/

	match type_checker.check_block(program, true, false) {
		Ok(program) => program,
		Err(err) => {
			eprintln!("{}", show_error(MolyError::TypeCheck(err.clone()), input.into()));
			panic!("test {}: type checking error: {:?}", 0, err)
		}
	};
}