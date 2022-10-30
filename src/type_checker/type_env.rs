use std::fmt;
use crate::token::IntType;

#[derive(Debug, PartialEq, Clone)]
pub struct TypeBinding {
	pub ident: String,
	pub type_expr: TypeExpr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr {
	Void,
	Int(IntType),
	///Just f32 for now
	Float,
	Bool,
	String,
	Array(Box<TypeExpr>),
	//TODO Rename to function or callable
	FnLiteral {
		parameter_types: Vec<TypeExpr>,
		return_type: Box<TypeExpr>,
	},
	Struct {
		name: String,
		bindings: Vec<TypeBinding>,
	},
	/// Equivalent to Rust's never
	Return(Box<TypeExpr>),
	//TODO Remove TypeExpr::Any once builtin have been removed
	Any,
}

impl fmt::Display for TypeExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			TypeExpr::Void => write!(f, "void"),
			TypeExpr::Int(IntType::U8) => write!(f, "u8"),
			TypeExpr::Int(IntType::U16) => write!(f, "u16"),
			TypeExpr::Int(IntType::U32) => write!(f, "u32"),
			TypeExpr::Int(IntType::U64) => write!(f, "u64"),
			TypeExpr::Int(IntType::I8) => write!(f, "i8"),
			TypeExpr::Int(IntType::I16) => write!(f, "i16"),
			TypeExpr::Int(IntType::I32) => write!(f, "i32"),
			TypeExpr::Int(IntType::I64) => write!(f, "i64"),
			TypeExpr::Float => write!(f, "f32"),
			TypeExpr::Bool => write!(f, "bool"),
			TypeExpr::String => write!(f, "str"),
			TypeExpr::FnLiteral { parameter_types, return_type } => {
				let parameter_types = parameter_types.iter()
					.map(|t| t.to_string())
					.collect::<Vec<String>>()
					.join(", ");

				write!(f, "fn({}) {} {{}}", parameter_types, return_type)
			}
			TypeExpr::Struct { name, .. } => write!(f, "{}", name),
			TypeExpr::Array(element_type) => write!(f, "[{}]", element_type),
			TypeExpr::Return(returned_type) => write!(f, "return {}", returned_type),
			TypeExpr::Any => write!(f, "ANY"),
		}
	}
}

pub type TypeId = u32;

pub struct TypeEnv {
	bindings: Vec<TypeBinding>,
	scope_stack: Vec<TypeScope>,
	custom_types: Vec<(String, TypeExpr)>,
}

struct TypeScope {
	binding_top: usize,
	custom_type_top: usize,
}

impl TypeEnv {
	pub fn new() -> Self {
		Self {
			bindings: Vec::new(),
			scope_stack: Vec::new(),
			custom_types: Vec::new(),
		}
	}

	pub fn get_identifier_type(&self, ident: &str) -> Option<&TypeExpr> {
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

	pub fn get_custom_type(&self, ident: &str) -> Option<&TypeExpr> {
		self.custom_types.iter()
			.rfind(|t| t.0 == ident)
			.map(|t| &t.1)
	}

	pub fn define_type(&mut self, name: &str, type_expr: TypeExpr) {
		//TODO Throw if type already exists
		self.custom_types.push((name.into(), type_expr));
	}

	pub fn push_scope(&mut self) {
		self.scope_stack.push(TypeScope {
			binding_top: self.bindings.len(),
			custom_type_top: self.custom_types.len()
		});
	}

	pub fn pop_scope(&mut self) {
		if let Some(scope) = self.scope_stack.pop() {
			self.bindings.truncate(scope.binding_top);
			self.custom_types.truncate(scope.custom_type_top);
		}
	}
}