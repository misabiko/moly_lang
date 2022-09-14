use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
	Integer(i64),
	Boolean(bool),
	String(String),
}

impl fmt::Display for Object {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Object::Integer(value) => write!(f, "{}", value),
			Object::Boolean(value) => write!(f, "{}", value),
			Object::String(value) => write!(f, "{}", value),
		}
	}
}