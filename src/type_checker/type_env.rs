use std::fmt;

pub struct TypeBinding {
	ident: String,
	type_expr: TypeExpr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr {
	Int {
		//TODO If unsigned field unused, dissolve into enum?
		unsigned: bool,
		size: IntegerSize,
	},
	Bool,
	String,
	//Array,
	//Hash,
	FnLiteral {
		return_type: Option<Box<TypeExpr>>,
	},
	Call {
		return_type: Option<Box<TypeExpr>>,
	},
}

impl fmt::Display for TypeExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TypeExpr::Int { unsigned: true, size: IntegerSize::S8 } => write!(f, "u8"),
			TypeExpr::Int { unsigned: true, size: IntegerSize::S16 } => write!(f, "u16"),
			TypeExpr::Int { unsigned: true, size: IntegerSize::S32 } => write!(f, "u32"),
			TypeExpr::Int { unsigned: true, size: IntegerSize::S64 } => write!(f, "u64"),
			TypeExpr::Int { unsigned: false, size: IntegerSize::S8 } => write!(f, "i8"),
			TypeExpr::Int { unsigned: false, size: IntegerSize::S16 } => write!(f, "i16"),
			TypeExpr::Int { unsigned: false, size: IntegerSize::S32 } => write!(f, "i32"),
			TypeExpr::Int { unsigned: false, size: IntegerSize::S64 } => write!(f, "i64"),
			TypeExpr::Bool => write!(f, "bool"),
			TypeExpr::String => write!(f, "str"),
			TypeExpr::FnLiteral { return_type } => {
				let return_type = if let Some(r) = return_type {
					format!(" {}", r)
				}else {
					"".into()
				};

				write!(f, "fn() {} {{}}", return_type)
			},
			TypeExpr::Call { return_type: None } => write!(f, "() -> void"),
			TypeExpr::Call { return_type: Some(return_type) } => write!(f, "() -> {}", return_type.as_ref()),
		}
	}
}

#[derive(Debug, PartialEq, Copy, Clone)]
#[repr(u8)]
pub enum IntegerSize {
	S8 = 8,
	S16 = 16,
	S32 = 32,
	S64 = 64,
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