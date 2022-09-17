use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type SymbolScope = &'static str;

//TODO Make enum
pub const GLOBAL_SCOPE: SymbolScope = "GLOBAL";
pub const LOCAL_SCOPE: SymbolScope = "LOCAL";
pub const BUILTIN_SCOPE: SymbolScope = "BUILTIN";
pub const FREE_SCOPE: SymbolScope = "FREE";
pub const FUNCTION_SCOPE: SymbolScope = "FUNCTION";
//Could add function argument scope if the need special treatment
// see "Resolving References to Arguments" in monkey compiler book

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Symbol {
	pub name: String,
	pub scope: SymbolScope,
	pub index: usize,
}

#[derive(Clone, PartialEq, Debug)]
pub struct SymbolTable {
	pub outer: Option<Rc<RefCell<SymbolTable>>>,
	pub store: HashMap<String, Symbol>,
	pub num_definitions: usize,
	pub free_symbols: Vec<Symbol>,
}

impl SymbolTable {
	pub fn new(outer: Option<Rc<RefCell<SymbolTable>>>) -> Self {
		Self {
			outer,
			store: Default::default(),
			num_definitions: 0,
			free_symbols: vec![],
		}
	}

	pub fn define(&mut self, name: &str) -> &Symbol {
		let symbol = Symbol {
			name: name.into(),
			index: self.num_definitions,
			scope: if self.outer.is_some() {
				LOCAL_SCOPE
			}else {
				GLOBAL_SCOPE
			}
		};

		self.store.insert(name.into(), symbol);
		self.num_definitions += 1;

		self.store.get(name).unwrap()
	}

	pub fn define_builtin(&mut self, index: usize, name: &str) -> &Symbol {
		let symbol = Symbol {
			name: name.into(),
			index,
			scope: BUILTIN_SCOPE,
		};

		self.store.insert(name.into(), symbol);
		self.store.get(name).unwrap()
	}

	pub fn define_free(&mut self, original: Symbol) -> &Symbol {
		let name = original.name.clone();
		let symbol = Symbol {
			name: name.clone(),
			index: self.free_symbols.len(),
			scope: FREE_SCOPE,
		};

		self.free_symbols.push(original);

		self.store.insert(name.clone(), symbol);

		self.store.get(&name).unwrap()
	}

	pub fn define_function_name(&mut self, name: &str) -> &Symbol {
		let symbol = Symbol {
			name: name.into(),
			index: 0,
			scope: FUNCTION_SCOPE,
		};

		self.store.insert(name.into(), symbol);
		self.store.get(name).unwrap()
	}

	pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
		let symbol = self.store.get(name).cloned();

		if symbol.is_none() {
			let symbol = self.outer.as_ref()
				.and_then(|s|
					s.borrow_mut().resolve(name)
				);

			match symbol {
				None => None,
				Some(symbol @ Symbol {
					scope: GLOBAL_SCOPE | BUILTIN_SCOPE,
					..
				}) => Some(symbol),
				Some(symbol) => Some(self.define_free(symbol).clone())
			}
		}else {
			symbol
		}
	}
}