use crate::object::{Builtin, Object};
use crate::token::IntType;
use crate::type_checker::type_env::TypeExpr;

pub struct BuiltinInfo {
	pub name: &'static str,
	pub type_expr: TypeExpr,
	pub builtin: Builtin,
}

pub fn get_builtin_functions() -> Vec<BuiltinInfo> {
	vec![
		BuiltinInfo {
			name: "len",
			type_expr: TypeExpr::Function {
				parameter_types: vec![TypeExpr::AnyParam(0)],
				return_type: Box::new(TypeExpr::Int(IntType::U64)),
				is_method: true,
			},
			builtin: |args| {
				Some(match &args[0] {
					Object::Array(elements) => Object::U64(elements.len() as u64),
					Object::String(value) => Object::U64(value.len() as u64),
					arg => Object::Error(format!("argument to `len` not supported, got {:?}", arg)),
				})
			},
		},
		BuiltinInfo {
			name: "print",
			type_expr: TypeExpr::Function {
				parameter_types: vec![TypeExpr::String],
				return_type: Box::new(TypeExpr::Void),
				is_method: false,
			},
			builtin: |args| {
				for arg in args {
					print!("{}", arg);
				}

				None
			},
		},
		BuiltinInfo {
			name: "println",
			type_expr: TypeExpr::Function {
				parameter_types: vec![TypeExpr::String],
				return_type: Box::new(TypeExpr::Void),
				is_method: false,
			},
			builtin: |args| {
				for arg in args {
					println!("{}", arg);
				}

				None
			},
		},
		//These require template typing
		BuiltinInfo {
			name: "first",
			type_expr: TypeExpr::Function {
				parameter_types: vec![TypeExpr::Array(Box::new(TypeExpr::AnyParam(0)))],
				return_type: Box::new(TypeExpr::AnyParam(0)),
				is_method: true,
			},
			builtin: |args| {
				if let Object::Array(elements) = &args[0] {
					elements.first().cloned()
				}else {
					Some(Object::Error(format!("argument to `first` must be Array, got {:?}", args[0])))
				}
			},
		},
		BuiltinInfo {
			name: "last",
			type_expr: TypeExpr::Function {
				parameter_types: vec![TypeExpr::Array(Box::new(TypeExpr::AnyParam(0)))],
				return_type: Box::new(TypeExpr::AnyParam(0)),
				is_method: true,
			},
			builtin: |args| {
				if let Object::Array(elements) = &args[0] {
					elements.last().cloned()
				}else {
					Some(Object::Error(format!("argument to `last` must be Array, got {:?}", args[0])))
				}
			},
		},
		BuiltinInfo {
			name: "rest",
			type_expr: TypeExpr::Function {
				parameter_types: vec![TypeExpr::Array(Box::new(TypeExpr::AnyParam(0)))],
				return_type: Box::new(TypeExpr::AnyParam(0)),
				is_method: true,
			},
			builtin: |args| {
				if let Object::Array(elements) = &args[0] {
					Some(Object::Array(elements.iter().skip(1).cloned().collect()))
				}else {
					Some(Object::Error(format!("argument to `rest` must be Array, got {:?}", args[0])))
				}
			},
		},
		BuiltinInfo {
			name: "push",
			type_expr: TypeExpr::Function {
				parameter_types: vec![TypeExpr::Array(Box::new(TypeExpr::AnyParam(0))), TypeExpr::AnyParam(0)],
				return_type: Box::new(TypeExpr::Array(Box::new(TypeExpr::AnyParam(0)))),
				is_method: true,
			},
			builtin: |args| {
				if let Object::Array(elements) = &args[0] {
					let mut new_elements = elements.clone();
					new_elements.push(args[1].clone());

					Some(Object::Array(new_elements))
				}else {
					Some(Object::Error(format!("argument to `push` must be Array, got {:?}", args[0])))
				}
			},
		},
		//Temporary hard coded functions for wasm
		BuiltinInfo {
			name: "printI32",
			type_expr: TypeExpr::Function {
				parameter_types: vec![TypeExpr::Int(IntType::I32)],
				return_type: Box::new(TypeExpr::Void),
				is_method: false,
			},
			builtin: |_args| {
				None
			},
		},
		BuiltinInfo {
			name: "printF32",
			type_expr: TypeExpr::Function {
				parameter_types: vec![TypeExpr::Float],
				return_type: Box::new(TypeExpr::Void),
				is_method: false,
			},
			builtin: |_args| {
				None
			},
		},
		BuiltinInfo {
			name: "setpixel",
			type_expr: TypeExpr::Function {
				parameter_types: vec![TypeExpr::Float, TypeExpr::Float, TypeExpr::Float],
				return_type: Box::new(TypeExpr::Void),
				is_method: false,
			},
			builtin: |_args| {
				None
			},
		},
	]
}

pub fn get_builtin_by_name(name: &'static str) -> Option<Builtin> {
	get_builtin_functions().iter()
		.find(|b| b.name == name)
		.map(|b| b.builtin)
}