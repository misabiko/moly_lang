use std::collections::HashMap;
use moly_lang::compiler::symbol_table::{GLOBAL_SCOPE, Symbol, SymbolTable};

#[test]
fn test_define() {
	let expected: HashMap<&'static str, Symbol> = vec![
		("a", Symbol { name: "a".into(), scope: GLOBAL_SCOPE, index: 0}),
		("b", Symbol { name: "b".into(), scope: GLOBAL_SCOPE, index: 1}),
	].into_iter().collect();

	let mut global = SymbolTable::new();

	let a = global.define("a");
	assert_eq!(a, &expected["a"]);

	let b = global.define("b");
	assert_eq!(b, &expected["b"]);
}

#[test]
fn test_resolve_global() {
	let mut global = SymbolTable::new();
	global.define("a");
	global.define("b");

	let expected = vec![
		Symbol { name: "a".into(), scope: GLOBAL_SCOPE, index: 0 },
		Symbol { name: "b".into(), scope: GLOBAL_SCOPE, index: 1 },
	];

	for sym in expected {
		if let Some(result) = global.resolve(&sym.name) {
			assert_eq!(result, &sym)
		}else {
			panic!("name {} is not resolvable", sym.name)
		}
	}
}