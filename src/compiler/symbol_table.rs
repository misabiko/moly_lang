use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

///Size of biggest scope (global)
pub type SymbolIndex = u16;

//Could add function argument scope if the need special treatment
// see "Resolving References to Arguments" in monkey compiler book
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SymbolScope {
	/// Maximum of 65536 symbols
	Global,
	/// Maximum of 256 symbols
	Local,
	/// Maximum of 256 symbols
	Builtin,
	/// Symbols captured by closure
	/// Maximum of 256 symbols
	Free,
	/// Symbol representing the current running function, for recursion
	Function,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Symbol {
	pub name: String,
	pub scope: SymbolScope,
	pub index: SymbolIndex,
}

#[derive(Clone, PartialEq, Debug)]
pub struct SymbolTable {
	pub outer: Option<Rc<RefCell<SymbolTable>>>,
	pub store: HashMap<String, Symbol>,
	pub num_definitions: SymbolIndex,
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
				SymbolScope::Local
			}else {
				SymbolScope::Global
			}
		};

		self.store.insert(name.into(), symbol);
		self.num_definitions += 1;

		self.store.get(name).unwrap()
	}

	pub fn define_builtin(&mut self, index: u8, name: &str) -> &Symbol {
		let symbol = Symbol {
			name: name.into(),
			index: index as SymbolIndex,
			scope: SymbolScope::Builtin,
		};

		self.store.insert(name.into(), symbol);
		self.store.get(name).unwrap()
	}

	pub fn define_free(&mut self, original: Symbol) -> &Symbol {
		let name = original.name.clone();
		let symbol = Symbol {
			name: name.clone(),
			index: self.free_symbols.len() as SymbolIndex,
			scope: SymbolScope::Free,
		};

		self.free_symbols.push(original);

		self.store.insert(name.clone(), symbol);

		self.store.get(&name).unwrap()
	}

	pub fn define_function_name(&mut self, name: &str) -> &Symbol {
		let symbol = Symbol {
			name: name.into(),
			index: 0,
			scope: SymbolScope::Function,
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
					scope: SymbolScope::Global | SymbolScope::Builtin,
					..
				}) => Some(symbol),
				Some(symbol) => Some(self.define_free(symbol).clone())
			}
		}else {
			symbol
		}
	}
}