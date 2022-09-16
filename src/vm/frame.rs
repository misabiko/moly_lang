use crate::code::Instructions;
use crate::object::Function;

pub struct Frame {
	pub func: Function,
	pub ip: usize,
	pub base_pointer: usize,
}

impl Frame {
	pub fn new(func: Function, base_pointer: usize) -> Self {
		Frame {
			func,
			ip: 0,
			base_pointer,
		}
	}

	//TODO Dissolve if Object::Function doesn't expand
	pub fn instructions(&self) -> &Instructions {
		&self.func.instructions
	}
}