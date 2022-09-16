use crate::code::Instructions;

pub struct Frame {
	pub func: Instructions,	// Object::Function(Instructions)
	pub ip: usize,
}

impl Frame {
	pub fn new(func: Instructions) -> Self {
		Frame { func, ip: 0 }
	}

	//TODO Dissolve if Object::Function doesn't expand
	pub fn instructions(&self) -> &Instructions {
		&self.func
	}
}