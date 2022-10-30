use std::fmt;
use crate::token::IntType;
use crate::type_checker::TypeCheckError;

pub struct TypeEnv {
	types: Vec<(TypeId, TypeExpr)>,
	bindings: Vec<TypeBinding>,
	custom_types: Vec<(String, TypeId)>,

	scope_stack: Vec<TypeScope>,
}

impl TypeEnv {
	pub fn new() -> Self {
		Self {
			types: Vec::new(),
			bindings: Vec::new(),
			custom_types: Vec::new(),
			scope_stack: Vec::new(),
		}
	}

	pub fn get_identifier_type(&self, ident: &str) -> Option<&TypeId> {
		self.bindings.iter()
			.rfind(|b| b.ident == ident)
			.map(|b| &b.type_id)
	}

	pub fn define_identifier(&mut self, name: &str, type_id: TypeId) {
		self.bindings.push(TypeBinding {
			ident: name.into(),
			type_id,
		})
	}

	pub fn get_type(&self, id: &TypeId) -> Option<&TypeExpr> {
		self.types.iter()
			.rfind(|t| &t.0 == id)
			.map(|t| &t.1)
	}

	pub fn get_custom_type(&self, ident: &str) -> Option<&TypeId> {
		self.custom_types.iter()
			.rfind(|t| t.0 == ident)
			.map(|t| &t.1)
	}

	pub fn get_custom_name_id(&self, id: usize) -> Option<&(String, TypeId)> {
		self.custom_types.get(id)
	}

	pub fn define_type(&mut self, name: String, type_expr: TypeExpr) -> Result<TypeId, TypeCheckError> {
		/*match type_expr {
			TypeExpr::Void |
			TypeExpr::Int(_) |
			TypeExpr::Float |
			TypeExpr::Bool |
			TypeExpr::String => panic!("Cannot redefine primitive type {}", type_expr),
			TypeExpr::Return(_) |
			TypeExpr::Any => panic!("Cannot define never type {}", type_expr),
			_ => {}
		};*/
		if let Some(t) = self.get_custom_type(&name) {
			return Err(TypeCheckError::Generic(format!("type {} is already defined: {:?}", name, t)))
		}

		let id = match self.get_id(&type_expr) {
			Some(id) => id,
			None => if let TypeExpr::Struct { .. } = type_expr {
				let custom_id = self.custom_types.len();
				let id = TypeId::CustomType(custom_id);
				self.custom_types.push((name, id.clone()));
				id
			}else {
				panic!("Didn't get id for {}", type_expr);
			}
		};
		self.types.push((id.clone(), type_expr));
		Ok(id)
	}

	pub fn push_scope(&mut self) {
		self.scope_stack.push(TypeScope {
			binding_top: self.bindings.len(),
			type_top: self.types.len(),
			custom_type_top: self.custom_types.len(),
		});
	}

	pub fn pop_scope(&mut self) {
		if let Some(scope) = self.scope_stack.pop() {
			self.bindings.truncate(scope.binding_top);
			self.types.truncate(scope.type_top);
			self.custom_types.truncate(scope.custom_type_top);
		}
	}

	pub fn get_id(&self, type_expr: &TypeExpr) -> Option<TypeId> {
		match type_expr {
			TypeExpr::Void => Some(TypeId::Void),
			TypeExpr::Int(IntType::U8) => Some(TypeId::U8),
			TypeExpr::Int(IntType::U16) => Some(TypeId::U16),
			TypeExpr::Int(IntType::U32) => Some(TypeId::U32),
			TypeExpr::Int(IntType::U64) => Some(TypeId::U64),
			TypeExpr::Int(IntType::I8) => Some(TypeId::I8),
			TypeExpr::Int(IntType::I16) => Some(TypeId::I16),
			TypeExpr::Int(IntType::I32) => Some(TypeId::I32),
			TypeExpr::Int(IntType::I64) => Some(TypeId::I64),
			TypeExpr::Float => Some(TypeId::Float),
			TypeExpr::Bool => Some(TypeId::Bool),
			TypeExpr::String => Some(TypeId::String),
			TypeExpr::Return(t) => Some(TypeId::Return(Box::new(self.get_id(t)?))),
			TypeExpr::Any => Some(TypeId::Any),
			TypeExpr::Array(elem_type) => Some(TypeId::Array(Box::new(self.get_id(elem_type)?))),
			TypeExpr::Function { parameter_types, return_type, is_method } => Some(TypeId::Function {
				parameters: parameter_types.iter().map(|t| self.get_id(t)).collect::<Option<Vec<TypeId>>>()?,
				return_type: Box::new(self.get_id(return_type)?),
				is_method: *is_method,
			}),
			TypeExpr::Struct { name, .. } => self.custom_types.iter()
				.find_map(|(n, id)| if n == name {
					Some(id.clone())
				}else {
					None
				})
		}
	}

	pub fn get_method(&self, method: &str, receiver: &TypeId) -> Option<TypeId> {
		for TypeBinding { ident, type_id } in &self.bindings {
			if ident == method {
				if let TypeId::Function { parameters, is_method: true, .. } = type_id {
					if parameters.first() == Some(receiver) {
						return Some(type_id.clone())
					}
				}
			}
		}

		None
	}
}

//TypeExpr is a weird name
#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr {
	Void,
	Int(IntType),
	///Just f32 for now
	Float,
	Bool,
	String,
	Array(Box<TypeExpr>),
	Function {
		parameter_types: Vec<TypeExpr>,
		return_type: Box<TypeExpr>,
		is_method: bool,
	},
	Struct {
		name: String,
		fields: Vec<TypeBinding>,
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
			TypeExpr::Function { parameter_types, return_type, is_method: false } => {
				let parameter_types = parameter_types.iter()
					.map(|t| t.to_string())
					.collect::<Vec<String>>()
					.join(", ");

				write!(f, "fn({}) {} {{}}", parameter_types, return_type)
			}
			TypeExpr::Function { parameter_types, return_type, is_method: true } => {
				let receiver = parameter_types.first().unwrap();
				let parameter_types = parameter_types.iter()
					.skip(1)
					.map(|t| t.to_string())
					.collect::<Vec<String>>()
					.join(", ");

				write!(f, "fn[{}] ({}) {} {{}}", receiver, parameter_types, return_type)
			}
			TypeExpr::Struct { name, .. } => write!(f, "{}", name),
			TypeExpr::Array(element_type) => write!(f, "[{}]", element_type),
			TypeExpr::Return(returned_type) => write!(f, "return {}", returned_type),
			TypeExpr::Any => write!(f, "ANY"),
		}
	}
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum TypeId {
	Void,
	U8,
	U16,
	U32,
	U64,
	I8,
	I16,
	I32,
	I64,
	Float,
	Bool,
	String,
	Array(Box<TypeId>),
	Function {
		parameters: Vec<TypeId>,
		return_type: Box<TypeId>,
		is_method: bool,
	},
	Return(Box<TypeId>),
	//TODO Remove TypeId::Any
	Any,
	CustomType(usize),
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeBinding {
	pub ident: String,
	pub type_id: TypeId,
}

struct TypeScope {
	binding_top: usize,
	type_top: usize,
	custom_type_top: usize,
}