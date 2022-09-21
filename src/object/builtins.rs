use crate::object::{Builtin, Object};

pub struct Builtins {
	pub name: &'static str,
	pub builtin: Builtin,
}

pub const BUILTINS: &[Builtins] = &[
	Builtins {
		name: "len",
		builtin: |args| {
			if args.len() != 1 {
				return Some(Object::Error(format!("wrong number of arguments. got={}, want=1", args.len())));
			}

			Some(match &args[0] {
				Object::Array(elements) => Object::Integer(elements.len() as i64),
				Object::String(value) => Object::Integer(value.len() as i64),
				arg => Object::Error(format!("argument to `len` not supported, got {:?}", arg)),
			})
		},
	},
	Builtins {
		name: "print",
		builtin: |args| {
			for arg in args {
				println!("{}", arg);
			}

			None
		},
	},
	Builtins {
		name: "first",
		builtin: |args| {
			if args.len() != 1 {
				return Some(Object::Error(format!("wrong number of arguments. got={}, want=1", args.len())));
			}

			if let Object::Array(elements) = &args[0] {
				elements.first().cloned()
			}else {
				Some(Object::Error(format!("argument to `first` must be Array, got {:?}", args[0])))
			}
		},
	},
	Builtins {
		name: "last",
		builtin: |args| {
			if args.len() != 1 {
				return Some(Object::Error(format!("wrong number of arguments. got={}, want=1", args.len())));
			}

			if let Object::Array(elements) = &args[0] {
				elements.last().cloned()
			}else {
				Some(Object::Error(format!("argument to `last` must be Array, got {:?}", args[0])))
			}
		},
	},
	Builtins {
		name: "rest",
		builtin: |args| {
			if args.len() != 1 {
				return Some(Object::Error(format!("wrong number of arguments. got={}, want=1", args.len())));
			}

			if let Object::Array(elements) = &args[0] {
				Some(Object::Array(elements.iter().skip(1).cloned().collect()))
			}else {
				Some(Object::Error(format!("argument to `rest` must be Array, got {:?}", args[0])))
			}
		},
	},
	Builtins {
		name: "push",
		builtin: |args| {
			if args.len() != 2 {
				return Some(Object::Error(format!("wrong number of arguments. got={}, want=2", args.len())));
			}

			if let Object::Array(elements) = &args[0] {
				let mut new_elements = elements.clone();
				new_elements.push(args[1].clone());

				Some(Object::Array(new_elements))
			}else {
				Some(Object::Error(format!("argument to `push` must be Array, got {:?}", args[0])))
			}
		},
	},
];

//TODO Dissolve BUILTINS into this?
pub fn get_builtin_by_name(name: &'static str) -> Option<&Builtin> {
	BUILTINS.iter()
		.find(|b| b.name == name)
		.map(|b| &b.builtin)
}