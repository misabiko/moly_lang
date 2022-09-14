use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
	Integer(i64),
	Boolean(bool),
	String(String),
	Array(Vec<Object>),
}

impl fmt::Display for Object {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Object::Integer(value) => write!(f, "{}", value),
			Object::Boolean(value) => write!(f, "{}", value),
			Object::String(value) => write!(f, "{}", value),
			Object::Array(elements) => write!(f, "[{}]", join_object_vec(elements)),
		}
	}
}

fn join_object_vec(objs: &[Object]) -> String {
	objs.iter()
		.map(|p| p.to_string())
		.collect::<Vec<String>>()
		.join(", ")
}