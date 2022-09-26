use std::collections::HashMap;

pub struct TypeBinding {
	ident: String,
	type_expr: TypeExpr,
}

#[derive(Debug, PartialEq)]
pub enum TypeExpr {
	Int,
	Bool,
	String,
	//Array,
	//Hash,
}

pub type TypeId = u32;

pub struct TypeEnv {
	types: HashMap<TypeId, TypeExpr>,
	bindings: Vec<TypeBinding>,
}

impl TypeEnv {
	pub fn get_identifier_type(&self, ident: &str) -> Option<&TypeExpr>{
		self.bindings.iter()
			.rfind(|b| b.ident == ident)
			.map(|b| &b.type_expr)
	}
}