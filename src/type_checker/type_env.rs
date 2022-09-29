use std::fmt;
use crate::token::IntType;

pub struct TypeBinding {
	ident: String,
	type_expr: TypeExpr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr {
	Void,
	Int(IntType),
	Bool,
	String,
	Array(Box<TypeExpr>),
	FnLiteral {
		parameter_types: Vec<TypeExpr>,
		return_type: Box<TypeExpr>,
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
			TypeExpr::Bool => write!(f, "bool"),
			TypeExpr::String => write!(f, "str"),
			TypeExpr::FnLiteral { parameter_types, return_type } => {
				let parameter_types = parameter_types.iter()
					.map(|t| t.to_string())
					.collect::<Vec<String>>()
					.join(", ");

				write!(f, "fn({}) {} {{}}", parameter_types, return_type)
			}
			TypeExpr::Array(element_type) => write!(f, "[{}]", element_type),
			TypeExpr::Return(returned_type) => write!(f, "return {}", returned_type),
			TypeExpr::Any => write!(f, "ANY"),
		}
	}
}

pub type TypeId = u32;

pub struct TypeEnv {
	bindings: Vec<TypeBinding>,
}

impl TypeEnv {
	pub fn new() -> Self {
		Self {
			bindings: Vec::new(),
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
}