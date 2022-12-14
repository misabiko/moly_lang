use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use moly_lib::compiler::symbol_table::{Symbol, SymbolTable, SymbolScope};

#[test]
fn test_define() {
	let expected: HashMap<&'static str, Symbol> = vec![
		("a", Symbol { name: "a".into(), scope: SymbolScope::Global, index: 0}),
		("b", Symbol { name: "b".into(), scope: SymbolScope::Global, index: 1}),
		("c", Symbol { name: "c".into(), scope: SymbolScope::Local, index: 0}),
		("d", Symbol { name: "d".into(), scope: SymbolScope::Local, index: 1}),
		("e", Symbol { name: "e".into(), scope: SymbolScope::Local, index: 0}),
		("f", Symbol { name: "f".into(), scope: SymbolScope::Local, index: 1}),
	].into_iter().collect();

	let mut global = SymbolTable::new(None);
	let a = global.define("a");
	assert_eq!(a, &expected["a"]);

	let b = global.define("b");
	assert_eq!(b, &expected["b"]);
	let global = Rc::new(RefCell::new(global));

	let mut first_local = SymbolTable::new(Some(global.clone()));
	let c = first_local.define("c");
	assert_eq!(c, &expected["c"]);

	let d = first_local.define("d");
	assert_eq!(d, &expected["d"]);
	let first_local = Rc::new(RefCell::new(first_local));

	let mut second_local = SymbolTable::new(Some(first_local.clone()));
	let e = second_local.define("e");
	assert_eq!(e, &expected["e"]);

	let f = second_local.define("f");
	assert_eq!(f, &expected["f"]);
}

#[test]
fn test_resolve_global() {
	let mut global = SymbolTable::new(None);
	global.define("a");
	global.define("b");
	let global = Rc::new(RefCell::new(global));

	let expected = vec![
		Symbol { name: "a".into(), scope: SymbolScope::Global, index: 0 },
		Symbol { name: "b".into(), scope: SymbolScope::Global, index: 1 },
	];

	for sym in expected {
		if let Some(result) = global.borrow_mut().resolve(&sym.name) {
			assert_eq!(result, sym)
		}else {
			panic!("name {} is not resolvable", sym.name)
		}
	}
}

#[test]
fn test_resolve_local() {
	let mut global = SymbolTable::new(None);
	global.define("a");
	global.define("b");
	let global = Rc::new(RefCell::new(global));

	let mut local = SymbolTable::new(Some(global.clone()));
	local.define("c");
	local.define("d");
	let local = Rc::new(RefCell::new(local));

	let expected = vec![
		Symbol { name: "a".into(), scope: SymbolScope::Global, index: 0 },
		Symbol { name: "b".into(), scope: SymbolScope::Global, index: 1 },
		Symbol { name: "c".into(), scope: SymbolScope::Local, index: 0 },
		Symbol { name: "d".into(), scope: SymbolScope::Local, index: 1 },
	];

	for sym in expected {
		if let Some(result) = local.borrow_mut().resolve(&sym.name) {
			assert_eq!(result, sym, "expected {:?} to resolve to {:?}", sym.name, result)
		}else {
			panic!("name {} is not resolvable", sym.name)
		}
	}
}

#[test]
fn test_resolve_nested_local() {
	let mut global = SymbolTable::new(None);
	global.define("a");
	global.define("b");
	let global = Rc::new(RefCell::new(global));

	let mut first_local = SymbolTable::new(Some(global.clone()));
	first_local.define("c");
	first_local.define("d");
	let first_local = Rc::new(RefCell::new(first_local));

	let mut second_local = SymbolTable::new(Some(first_local.clone()));
	second_local.define("e");
	second_local.define("f");
	let second_local = Rc::new(RefCell::new(second_local));

	let expected = vec![
		(first_local, vec![
			Symbol { name: "a".into(), scope: SymbolScope::Global, index: 0 },
			Symbol { name: "b".into(), scope: SymbolScope::Global, index: 1 },
			Symbol { name: "c".into(), scope: SymbolScope::Local, index: 0 },
			Symbol { name: "d".into(), scope: SymbolScope::Local, index: 1 },
		]),
		(second_local, vec![
			Symbol { name: "a".into(), scope: SymbolScope::Global, index: 0 },
			Symbol { name: "b".into(), scope: SymbolScope::Global, index: 1 },
			Symbol { name: "e".into(), scope: SymbolScope::Local, index: 0 },
			Symbol { name: "f".into(), scope: SymbolScope::Local, index: 1 },
		]),
	];

	for (table, expected_symbols) in expected {
		for sym in expected_symbols {
			if let Some(result) = table.borrow_mut().resolve(&sym.name) {
				assert_eq!(result, sym, "expected {:?} to resolve to {:?}", sym.name, result)
			}else {
				panic!("name {} is not resolvable", sym.name)
			}
		}
	}
}

#[test]
fn test_define_resolve_builtins() {
	let global = Rc::new(RefCell::new(SymbolTable::new(None)));
	let first_local = Rc::new(RefCell::new(SymbolTable::new(Some(global.clone()))));
	let second_local = Rc::new(RefCell::new(SymbolTable::new(Some(first_local.clone()))));

	let expected = vec![
		Symbol { name: "a".into(), scope: SymbolScope::Builtin, index: 0 },
		Symbol { name: "b".into(), scope: SymbolScope::Builtin, index: 1 },
		Symbol { name: "e".into(), scope: SymbolScope::Builtin, index: 2 },
		Symbol { name: "f".into(), scope: SymbolScope::Builtin, index: 3 },
	];

	{
		let mut borrow = global.borrow_mut();
		for (i, value) in expected.iter().enumerate() {
			borrow.define_builtin(i as u8, &value.name);
		}
	}

	for table in [global, first_local, second_local] {
		for sym in &expected {
			if let Some(result) = table.borrow_mut().resolve(&sym.name) {
				assert_eq!(&result, sym, "expected {:?} to resolve to {:?}", sym.name, result)
			}else {
				panic!("name {} is not resolvable", sym.name)
			}
		}
	}
}

#[test]
fn test_resolve_free() {
	let mut global = SymbolTable::new(None);
	global.define("a");
	global.define("b");
	let global = Rc::new(RefCell::new(global));

	let mut first_local = SymbolTable::new(Some(global.clone()));
	first_local.define("c");
	first_local.define("d");
	let first_local = Rc::new(RefCell::new(first_local));

	let mut second_local = SymbolTable::new(Some(first_local.clone()));
	second_local.define("e");
	second_local.define("f");
	let second_local = Rc::new(RefCell::new(second_local));

	let expected = vec![
		(
			first_local,
			 vec![
				Symbol { name: "a".into(), scope: SymbolScope::Global, index: 0 },
				Symbol { name: "b".into(), scope: SymbolScope::Global, index: 1 },
				Symbol { name: "c".into(), scope: SymbolScope::Local, index: 0 },
				Symbol { name: "d".into(), scope: SymbolScope::Local, index: 1 },
			],
			vec![]
		),
		(
			second_local,
			vec![
				Symbol { name: "a".into(), scope: SymbolScope::Global, index: 0 },
				Symbol { name: "b".into(), scope: SymbolScope::Global, index: 1 },
				Symbol { name: "c".into(), scope: SymbolScope::Free, index: 0 },
				Symbol { name: "d".into(), scope: SymbolScope::Free, index: 1 },
				Symbol { name: "e".into(), scope: SymbolScope::Local, index: 0 },
				Symbol { name: "f".into(), scope: SymbolScope::Local, index: 1 },
			],
			vec![
				Symbol { name: "c".into(), scope: SymbolScope::Local, index: 0 },
				Symbol { name: "d".into(), scope: SymbolScope::Local, index: 1 },
			]
		),
	];

	for (table, expected_symbols, expected_free_symbols) in expected {
		for sym in expected_symbols {
			if let Some(result) = table.borrow_mut().resolve(&sym.name) {
				assert_eq!(result, sym, "expected {:?} to resolve to {:?}", sym.name, result)
			}else {
				panic!("name {} is not resolvable", sym.name)
			}
		}

		let free_symbols = &table.borrow().free_symbols;
		assert_eq!(free_symbols.len(), expected_free_symbols.len(), "wrong number of free symbols");

		for (expected_sym, sym) in expected_free_symbols.into_iter().zip(free_symbols.iter()) {
			assert_eq!(&expected_sym, sym, "wrong free symbol");
		}
	}
}

#[test]
fn test_resolve_unresolvable_free() {
	let mut global = SymbolTable::new(None);
	global.define("a");
	let global = Rc::new(RefCell::new(global));

	let mut first_local = SymbolTable::new(Some(global.clone()));
	first_local.define("c");
	let first_local = Rc::new(RefCell::new(first_local));

	let mut second_local = SymbolTable::new(Some(first_local.clone()));
	second_local.define("e");
	second_local.define("f");
	let second_local = Rc::new(RefCell::new(second_local));

	let expected = vec![
		Symbol { name: "a".into(), scope: SymbolScope::Global, index: 0 },
		Symbol { name: "c".into(), scope: SymbolScope::Free, index: 0 },
		Symbol { name: "e".into(), scope: SymbolScope::Local, index: 0 },
		Symbol { name: "f".into(), scope: SymbolScope::Local, index: 1 },
	];

	for sym in expected {
		if let Some(result) = second_local.borrow_mut().resolve(&sym.name) {
			assert_eq!(result, sym, "expected {:?} to resolve to {:?}", sym.name, result)
		}else {
			panic!("name {} is not resolvable", sym.name)
		}
	}

	let expected_unresolvable = vec!["b", "d"];

	for name in expected_unresolvable {
		assert!(second_local.borrow_mut().resolve(name).is_none(), "name {} resolved, but was expected not to", name)
	}
}

#[test]
fn test_define_and_resolve_function_name() {
	let mut global = SymbolTable::new(None);
	global.define_function_name("a");

	let expected = Symbol {
		name: "a".into(),
		scope: SymbolScope::Function,
		index: 0,
	};

	if let Some(result) = global.resolve(&expected.name) {
		assert_eq!(result, expected, "expected {:?} to resolve to {:?}", expected.name, result)
	}else {
		panic!("function name {} is not resolvable", expected.name)
	}
}

#[test]
fn test_shadowing_function_name() {
	let mut global = SymbolTable::new(None);
	global.define_function_name("a");
	global.define("a");

	let expected = Symbol {
		name: "a".into(),
		scope: SymbolScope::Global,
		index: 0,
	};

	if let Some(result) = global.resolve(&expected.name) {
		assert_eq!(result, expected, "expected {:?} to resolve to {:?}", expected.name, result)
	}else {
		panic!("function name {} is not resolvable", expected.name)
	}
}