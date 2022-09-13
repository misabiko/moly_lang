use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Clone)]
pub enum Object {
	Integer(i64),
}

impl fmt::Display for Object {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Object::Integer(value) => write!(f, "{}", value),
		}
	}
}