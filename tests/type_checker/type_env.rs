use moly_lib::type_checker::type_env::TypeId;
use moly_lib::type_checker::TypeChecker;

#[test]
fn test_get_method() {
	// let input = "[1].push(2)";

	let type_checker = TypeChecker::new();
	let method = type_checker.type_env.get_method(
		&"push".to_string(),
		&TypeId::Array(Box::new(TypeId::U8))
	);

	let concrete_array = TypeId::Array(Box::new(TypeId::U8));
	assert_eq!(method, Some(TypeId::Function {
		parameters: vec![
			concrete_array.clone(),
			TypeId::U8
		],
		return_type: Box::new(concrete_array),
		is_method: true
	}));
}
