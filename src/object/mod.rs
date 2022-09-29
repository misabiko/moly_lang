use std::fmt;
use std::fmt::Formatter;
use crate::code::Instructions;

pub mod builtins;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
	U8(u8),
	U16(u16),
	U32(u32),
	U64(u64),
	I8(i8),
	I16(i16),
	I32(i32),
	I64(i64),
	Boolean(bool),
	String(String),
	Array(Vec<Object>),
	Function(Function),
	Builtin(Builtin),
	Error(String),
	Closure(Closure),
}

impl fmt::Display for Object {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Object::U8(value) => write!(f, "{}", value),
			Object::U16(value) => write!(f, "{}", value),
			Object::U32(value) => write!(f, "{}", value),
			Object::U64(value) => write!(f, "{}", value),
			Object::I8(value) => write!(f, "{}", value),
			Object::I16(value) => write!(f, "{}", value),
			Object::I32(value) => write!(f, "{}", value),
			Object::I64(value) => write!(f, "{}", value),
			Object::Boolean(value) => write!(f, "{}", value),
			Object::String(value) => write!(f, "{}", value),
			Object::Array(elements) => write!(f, "[{}]", join_object_vec(elements)),
			Object::Function(Function { instructions, .. }) => write!(f, "Function[{:?}]", instructions),
			Object::Builtin(_) => write!(f, "builtin function"),
			Object::Error(err) => write!(f, "ERROR: {}", err),
			Object::Closure { .. } => write!(f, "Closure[{:?}]", self),
		}
	}
}

fn join_object_vec(objs: &[Object]) -> String {
	objs.iter()
		.map(|p| p.to_string())
		.collect::<Vec<String>>()
		.join(", ")
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
	pub instructions: Instructions,
	/// Number of local symbols, includes num_parameters
	pub num_locals: u8,
	pub num_parameters: u8,
}

pub type Builtin = fn(Vec<Object>) -> Option<Object>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Closure {
	pub func: Function,
	pub free: Vec<Object>,
}