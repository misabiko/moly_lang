use crate::object::{Builtin, Object};
use crate::token::IntType;
use crate::type_checker::type_env::{TypeExpr, TypeId};
use crate::type_checker::typed_ast::{TypedFunction, TypedStatementBlock};

pub struct BuiltinInfo {
	pub name: &'static str,
	pub type_expr: TypeExpr,
	pub builtin: Builtin,
}

pub struct BuiltinTraitInfo {
	pub name: &'static str,
	pub type_expr: TypeExpr,
	pub builtins: Vec<Builtin>,
}

pub fn get_builtin_traits() -> Vec<BuiltinTraitInfo> {
	vec![
		/*BuiltinTraitInfo {
			name: "Array",
			type_expr: TypeExpr::Trait {
				methods: vec![
					TypedFunction {
						name: Some("first".into()),
						parameters: vec![TypeExpr::Array(Box::new(TypeExpr::TraitParam()))],
						body: TypedStatementBlock {},
						is_method: true
					}
				]
			}
		},*/
		BuiltinTraitInfo {
			name: "Length",
			type_expr: TypeExpr::Trait {
				name: "Length".into(),
				methods: vec![
					TypedFunction {
						name: Some("len".into()),
						parameters: vec![("a".into(), TypeId::TraitParam(0, 0))],
						body: TypedStatementBlock {
							statements: vec![],
							return_type: TypeId::U64,
						},
						return_type: TypeId::U64,
						is_method: true,
					}
				]
			},
			builtins: vec![|args| {
				Some(match &args[0] {
					Object::Array(elements) => Object::U64(elements.len() as u64),
					Object::String(value) => Object::U64(value.len() as u64),
					arg => Object::Error(format!("argument to `len` not supported, got {:?}", arg)),
				})
			}]
		},
	]
}

pub fn get_builtin_functions() -> Vec<BuiltinInfo> {
	vec![
		BuiltinInfo {
			name: "blehlen",
			type_expr: TypeExpr::Function {
				parameter_types: vec![TypeExpr::Any],
				return_type: Box::new(TypeExpr::Int(IntType::U64)),
				is_method: false,
			},
			builtin: |_args| {
				Some(Object::Error(format!("switching to traits")))
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
			name: "blehfirst",
			type_expr: TypeExpr::Function {
				parameter_types: vec![TypeExpr::Array(Box::new(TypeExpr::Any))],
				return_type: Box::new(TypeExpr::Any),
				is_method: false,
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
				parameter_types: vec![TypeExpr::Array(Box::new(TypeExpr::Any))],
				return_type: Box::new(TypeExpr::Any),
				is_method: false,
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
				parameter_types: vec![TypeExpr::Array(Box::new(TypeExpr::Any))],
				return_type: Box::new(TypeExpr::Any),
				is_method: false,
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
				parameter_types: vec![TypeExpr::AnyParam(0)],
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
				parameter_types: vec![TypeExpr::AnyParam(0)],
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
				parameter_types: vec![TypeExpr::Any, TypeExpr::Any, TypeExpr::Any],
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