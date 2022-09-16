use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use moly_lang::compiler::symbol_table::{GLOBAL_SCOPE, LOCAL_SCOPE, Symbol, SymbolTable};

#[test]
fn test_define() {
	let expected: HashMap<&'static str, Symbol> = vec![
		("a", Symbol { name: "a".into(), scope: GLOBAL_SCOPE, index: 0}),
		("b", Symbol { name: "b".into(), scope: GLOBAL_SCOPE, index: 1}),
		("c", Symbol { name: "c".into(), scope: LOCAL_SCOPE, index: 0}),
		("d", Symbol { name: "d".into(), scope: LOCAL_SCOPE, index: 1}),
		("e", Symbol { name: "e".into(), scope: LOCAL_SCOPE, index: 0}),
		("f", Symbol { name: "f".into(), scope: LOCAL_SCOPE, index: 1}),
	].into_iter().collect();

	let global = Rc::new(RefCell::new(SymbolTable::new(None)));
	{
		let mut borrow = global.borrow_mut();
		let a = borrow.define("a");
		assert_eq!(a, &expected["a"]);

		let b = borrow.define("b");
		assert_eq!(b, &expected["b"]);
	}

	let first_local = Rc::new(RefCell::new(SymbolTable::new(Some(global.clone()))));
	{
		let mut borrow = first_local.borrow_mut();
		let c = borrow.define("c");
		assert_eq!(c, &expected["c"]);

		let d = borrow.define("d");
		assert_eq!(d, &expected["d"]);
	}

	let second_local = Rc::new(RefCell::new(SymbolTable::new(Some(first_local.clone()))));
	{
		let mut borrow = second_local.borrow_mut();
		let e = borrow.define("e");
		assert_eq!(e, &expected["e"]);

		let f = borrow.define("f");
		assert_eq!(f, &expected["f"]);
	}
}

#[test]
fn test_resolve_global() {
	let global = Rc::new(RefCell::new(SymbolTable::new(None)));
	{
		let mut borrow = global.borrow_mut();
		borrow.define("a");
		borrow.define("b");
	}

	let expected = vec![
		Symbol { name: "a".into(), scope: GLOBAL_SCOPE, index: 0 },
		Symbol { name: "b".into(), scope: GLOBAL_SCOPE, index: 1 },
	];

	for sym in expected {
		if let Some(result) = global.borrow().resolve(&sym.name) {
			assert_eq!(result, sym)
		}else {
			panic!("name {} is not resolvable", sym.name)
		}
	}
}

#[test]
fn test_resolve_local() {
	let global = Rc::new(RefCell::new(SymbolTable::new(None)));
	{
		let mut borrow = global.borrow_mut();
		borrow.define("a");
		borrow.define("b");
	}

	let local = Rc::new(RefCell::new(SymbolTable::new(Some(global.clone()))));
	{
		let mut borrow = local.borrow_mut();
		borrow.define("c");
		borrow.define("d");
	}

	let expected = vec![
		Symbol { name: "a".into(), scope: GLOBAL_SCOPE, index: 0 },
		Symbol { name: "b".into(), scope: GLOBAL_SCOPE, index: 1 },
		Symbol { name: "c".into(), scope: LOCAL_SCOPE, index: 0 },
		Symbol { name: "d".into(), scope: LOCAL_SCOPE, index: 1 },
	];

	for sym in expected {
		if let Some(result) = local.borrow().resolve(&sym.name) {
			assert_eq!(result, sym, "expected {:?} to resolve to {:?}", sym.name, result)
		}else {
			panic!("name {} is not resolvable", sym.name)
		}
	}
}

#[test]
fn test_resolve_nested_local() {
	let global = Rc::new(RefCell::new(SymbolTable::new(None)));
	{
		let mut borrow = global.borrow_mut();
		borrow.define("a");
		borrow.define("b");
	}

	let first_local = Rc::new(RefCell::new(SymbolTable::new(Some(global.clone()))));
	{
		let mut borrow = first_local.borrow_mut();
		borrow.define("c");
		borrow.define("d");
	}

	let second_local = Rc::new(RefCell::new(SymbolTable::new(Some(first_local.clone()))));
	{
		let mut borrow = second_local.borrow_mut();
		borrow.define("e");
		borrow.define("f");
	}

	let expected = vec![
		(first_local, vec![
			Symbol { name: "a".into(), scope: GLOBAL_SCOPE, index: 0 },
			Symbol { name: "b".into(), scope: GLOBAL_SCOPE, index: 1 },
			Symbol { name: "c".into(), scope: LOCAL_SCOPE, index: 0 },
			Symbol { name: "d".into(), scope: LOCAL_SCOPE, index: 1 },
		]),
		(second_local, vec![
			Symbol { name: "a".into(), scope: GLOBAL_SCOPE, index: 0 },
			Symbol { name: "b".into(), scope: GLOBAL_SCOPE, index: 1 },
			Symbol { name: "e".into(), scope: LOCAL_SCOPE, index: 0 },
			Symbol { name: "f".into(), scope: LOCAL_SCOPE, index: 1 },
		]),
	];

	for (table, expected_symbols) in expected {
		for sym in expected_symbols {
			if let Some(result) = table.borrow().resolve(&sym.name) {
				assert_eq!(result, sym, "expected {:?} to resolve to {:?}", sym.name, result)
			}else {
				panic!("name {} is not resolvable", sym.name)
			}
		}
	}
}