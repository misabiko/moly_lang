use std::collections::HashMap;

pub type SymbolScope = &'static str;

pub const GLOBAL_SCOPE: SymbolScope = "GLOBAL";

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Symbol {
	pub name: String,
	pub scope: SymbolScope,
	pub index: usize,
}

#[derive(Clone)]
pub struct SymbolTable {
	pub store: HashMap<String, Symbol>,
	pub num_definitions: usize,
}

impl SymbolTable {
	pub fn new() -> Self {
		Self {
			store: Default::default(),
			num_definitions: 0,
		}
	}

	pub fn define(&mut self, name: &str) -> &Symbol {
		let symbol = Symbol {
			name: name.into(),
			index: self.num_definitions,
			scope: GLOBAL_SCOPE
		};

		self.store.insert(name.into(), symbol);
		self.num_definitions += 1;

		self.store.get(name).unwrap()
	}

	pub fn resolve(&mut self, name: &str) -> Option<&Symbol> {
		self.store.get(name)
	}
}