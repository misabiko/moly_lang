use std::fmt;
use crate::token::IntType;
use crate::type_checker::TypeCheckError;
use crate::type_checker::typed_ast::TypedFunction;

#[derive(Debug)]
pub struct TypeEnv {
	types: Vec<TypeInfo>,
	bindings: Vec<TypeBinding>,

	scope_stack: Vec<TypeScope>,
}

#[derive(Debug)]
pub struct TypeInfo {
	pub id: TypeId,
	pub expr: TypeExpr,
	pub custom_name: Option<String>,
}

impl TypeEnv {
	pub fn new() -> Self {
		Self {
			types: Vec::new(),
			bindings: Vec::new(),
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
			.rfind(|t| &t.id == id)
			.map(|t| &t.expr)
	}

	pub fn get_custom_type(&self, ident: &String) -> Option<&TypeId> {
		self.types.iter()
			.rfind(|t| t.custom_name.as_ref() == Some(ident))
			.map(|t| &t.id)
	}

	pub fn get_type_info(&self, index: usize) -> Option<&TypeInfo> {
		self.types.get(index)
	}

	pub fn define_type(&mut self, name: String, type_expr: TypeExpr) -> Result<TypeId, TypeCheckError> {
		if let Some(t) = self.get_custom_type(&name) {
			return Err(TypeCheckError::Generic(format!("type {} is already defined: {:?}", name, t)))
		}

		let id = match self.get_id(&type_expr) {
			Some(id) => id,
			None => match type_expr {
				TypeExpr::Struct { .. } => TypeId::Struct(self.types.len()),
				TypeExpr::Trait { .. } => TypeId::Trait(self.types.len()),
				_ => panic!("Didn't get id for {}", type_expr)
			}
		};
		self.types.push(TypeInfo {
			id: id.clone(),
			expr: type_expr,
			custom_name: Some(name),
		});
		Ok(id)
	}

	pub fn push_scope(&mut self) {
		self.scope_stack.push(TypeScope {
			binding_top: self.bindings.len(),
			type_top: self.types.len(),
		});
	}

	pub fn pop_scope(&mut self) {
		if let Some(scope) = self.scope_stack.pop() {
			self.bindings.truncate(scope.binding_top);
			self.types.truncate(scope.type_top);
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
			TypeExpr::Struct { name, .. } => self.types.iter()
				.find_map(|info| if info.custom_name.as_ref() == Some(name) {
					Some(info.id.clone())
				}else {
					None
				}),
			TypeExpr::Trait { name, .. } => self.types.iter()
				.find_map(|info| if info.custom_name.as_ref() == Some(name) {
					Some(info.id.clone())
				}else {
					None
				}),
			TypeExpr::TraitParam(trait_name, index) => self.types.iter().enumerate()
				.find_map(|(i, info)| if info.custom_name.as_ref() == Some(trait_name) {
					Some(TypeId::TraitParam(i, *index))
				}else {
					None
				}),
			TypeExpr::AnyParam(index) => Some(TypeId::AnyParam(*index)),
		}
	}

	pub fn get_method(&self, method: &String, receiver: &TypeId) -> Option<TypeId> {
		for TypeBinding { ident, type_id } in self.bindings.iter().rev() {
			if ident == method {
				if let TypeId::Function { parameters, is_method: true, .. } = type_id {
					let first = parameters.first().unwrap();

					let mut concrete_params = vec![];
					if type_id_eq(receiver, first, &mut concrete_params) {
						return Some(if concrete_params.is_empty() {
							type_id.clone()
						} else {
							concretize_type_id(type_id, &concrete_params)
						})
					}
				}
			}
		}

		for TypeInfo { id, expr, .. } in self.types.iter().rev() {
			if let TypeId::Trait(_) = id {
				if self.qualifies_trait(receiver, &id) {
					if let TypeExpr::Trait { methods, .. } = &expr {
						if let Some(func) = methods.iter().find(|m| m.name.as_ref() == Some(method)) {
							return Some(TypeId::Function {
								parameters: func.parameters.iter().map(|(_, id)| id.clone()).collect(),
								return_type: Box::new(func.return_type.clone()),
								is_method: true,
							})
						}
					}else {
						panic!("Should've found a trait for {:?}", expr)
					}
				}
			}
		}

		None
	}

	pub fn qualifies_trait(&self, _type_id: &TypeId, _trait_id: &TypeId) -> bool {
		true//TODO qualifies trait
	}
}

//a must be concrete type
pub fn type_id_eq(a: &TypeId, b: &TypeId, mut params: &mut Vec<TypeId>) -> bool {
	if let TypeId::AnyParam(index) = b {
		return if params.len() > *index {
			let b = params[*index].clone();
			type_id_eq(a, &b, &mut params)
		} else {
			params.push(a.clone());
			true
		}
	}

	if a == b {
		return true;
	}

	match (a, b) {
		(TypeId::Array(a_elem), TypeId::Array(b_elem)) => type_id_eq(a_elem, b_elem, &mut params),
		_ => false,
	}
}

pub fn concretize_type_id(type_id: &TypeId, params: &Vec<TypeId>) -> TypeId {
	match type_id {
		TypeId::AnyParam(index) => params[*index].clone(),
		TypeId::Array(t) => TypeId::Array(Box::new(concretize_type_id(t, &params))),
		TypeId::Function { parameters, return_type, is_method } => TypeId::Function {
			parameters: parameters.iter()
				.map(|t| concretize_type_id(t, params))
				.collect(),
			return_type: Box::new(concretize_type_id(return_type, params)),
			is_method: *is_method,
		},
		_ => type_id.clone(),
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
	Trait {
		name: String,
		methods: Vec<TypedFunction>,
	},
	TraitParam(String, usize),
	//Could rename to something like AnonTraitParam
	AnyParam(usize),
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
			TypeExpr::Trait { .. } => write!(f, "trait"),
			TypeExpr::TraitParam(trait_name, index) => write!(f, "TraitParam[{}][{}]", trait_name, index),
			TypeExpr::AnyParam(index) => write!(f, "AnyParam[{}]", index),
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
	Struct(usize),
	Trait(usize),
	TraitParam(usize, usize),
	AnyParam(usize),
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeBinding {
	pub ident: String,
	pub type_id: TypeId,
}

#[derive(Debug)]
struct TypeScope {
	binding_top: usize,
	type_top: usize,
}