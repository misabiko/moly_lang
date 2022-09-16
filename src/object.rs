use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;
use crate::code::Instructions;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
	Integer(i64),
	Boolean(bool),
	String(String),
	Array(Vec<Object>),
	Hash(HashMap<HashingObject, (HashingObject, Object)>),
	Function(Instructions),
}

impl fmt::Display for Object {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Object::Integer(value) => write!(f, "{}", value),
			Object::Boolean(value) => write!(f, "{}", value),
			Object::String(value) => write!(f, "{}", value),
			Object::Array(elements) => write!(f, "[{}]", join_object_vec(elements)),
			Object::Hash(pairs) => {
				let pairs = pairs.iter()
					.map(|(k, v)| format!("{}: {}", k, v.1))
					.collect::<Vec<String>>()
					.join(", ");

				write!(f, "{{{}}}", pairs)
			},
			Object::Function(instructions) => write!(f, "Function[{:?}]", instructions)
		}
	}
}

fn join_object_vec(objs: &[Object]) -> String {
	objs.iter()
		.map(|p| p.to_string())
		.collect::<Vec<String>>()
		.join(", ")
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashingObject {
	Integer(i64),
	Boolean(bool),
	String(String),
}

impl fmt::Display for HashingObject {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			HashingObject::Integer(value) => write!(f, "{}", value),
			HashingObject::Boolean(value) => write!(f, "{}", value),
			HashingObject::String(value) => write!(f, "{}", value),
		}
	}
}

impl TryFrom<Object> for HashingObject {
	type Error = String;

	fn try_from(value: Object) -> Result<Self, Self::Error> {
		match value {
			Object::Integer(value) => Ok(HashingObject::Integer(value)),
			Object::Boolean(value) => Ok(HashingObject::Boolean(value)),
			Object::String(value) => Ok(HashingObject::String(value)),
			_ => Err(format!("unusable as hash key: {:?}", value)),
		}
	}
}