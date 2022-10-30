use std::fmt::Write;
use crate::MolyError;
use crate::parser::ParserErrorCause;
use crate::type_checker::TypeCheckError;

impl TypeCheckError {
	pub fn show(&self) -> String {
		match self {
			TypeCheckError::UnknownField { left, field } => {
				let mut message = String::new();

				writeln!(&mut message, "I cannot find the field `{}` for the type `{}`:", field, left).unwrap();

				message
			}
			err => format!("show {:#?}", err)
		}
	}
}

pub fn show_error(error: MolyError, input: String) -> String {
	match error {
		MolyError::Parse(err) => match &err.cause {
			ParserErrorCause::UnexpectedTokenType { expected, actual } => {
				let mut message = String::new();
				writeln!(&mut message, "I am partway through parsing the input, but I got stuck here:\n").unwrap();

				let token = err.token.as_ref().unwrap();

				let mut lines = input.lines().skip(token.line);

				if token.line > 0 {
					let line_str = lines.next().unwrap();
					write!(&mut message, "{}|{}", token.line - 1, line_str).unwrap();
				}

				let line_number = format!("{}| ", token.line);
				write!(&mut message, "{}{}",
					   line_number,
					   lines.next().unwrap()
				).unwrap();

				writeln!(&mut message, "\n{}^", " ".repeat(line_number.len() + token.position)).unwrap();
				writeln!(&mut message, "pos: {}", token.position).unwrap();

				writeln!(&mut message, "I was expecting a token of type {:?}, but I found {:?}.", expected, actual).unwrap();

				message
			}
			cause => format!("{:#?}\n{:#?}", err.token, cause)
		},
		MolyError::TypeCheck(err) => match err {
			TypeCheckError::UnknownField { left, field } => {
				let mut message = String::new();

				writeln!(&mut message, "I cannot find the field `{}` for the type `{}`:", field, left).unwrap();

				message
			}
			TypeCheckError::UnknownVariable(name) => {
				let mut message = String::new();

				writeln!(&mut message, "I cannot find a `{}` variable:", name).unwrap();

				message
			}
			err => format!("show {:#?}", err)
		}
	}
}