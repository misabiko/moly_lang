use std::collections::HashMap;

pub struct TypeBinding {
	ident: String,
	type_expr: TypeExpr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr {
	Int,
	Bool,
	String,
	//Array,
	//Hash,
	Call,
}

pub type TypeId = u32;

pub struct TypeEnv {
	//types: HashMap<TypeId, TypeExpr>,
	bindings: Vec<TypeBinding>,
}

impl TypeEnv {
	pub fn new() -> Self {
		Self {
			//types: HashMap::new(),
			bindings: Vec::new(),
		}
	}

	pub fn get_identifier_type(&self, ident: &str) -> Option<&TypeExpr>{
		self.bindings.iter()
			.rfind(|b| b.ident == ident)
			.map(|b| &b.type_expr)
	}

	pub fn define_identifier(&mut self, name: &str, type_expr: TypeExpr) {
		self.bindings.push(TypeBinding {
			ident: name.into(),
			type_expr,
		})
	}
}