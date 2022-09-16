use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type SymbolScope = &'static str;

//TODO Make enum
pub const GLOBAL_SCOPE: SymbolScope = "GLOBAL";
pub const LOCAL_SCOPE: SymbolScope = "LOCAL";
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
}

impl SymbolTable {
	pub fn new(outer: Option<Rc<RefCell<SymbolTable>>>) -> Self {
		Self {
			outer,
			store: Default::default(),
			num_definitions: 0,
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

	pub fn resolve(&self, name: &str) -> Option<Symbol> {
		self.store.get(name).cloned()
			.or_else(||
				self.outer.as_ref()
					.and_then(|s|
						s.borrow_mut().resolve(name)
					)
			)
	}
}