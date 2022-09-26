use crate::ast::{Expression, Program, Statement};
use crate::type_checker::typed_ast::{TypedExpression, TypedProgram, TypedStatement};

pub mod typed_ast;
pub mod type_env;

pub struct TypeChecker {

}

impl TypeChecker {
	pub fn new() -> Self { Self {} }

	pub fn check(&mut self, program: Program) -> TCResult<TypedProgram> {
		Ok(TypedProgram {
			statements: program.statements.into_iter()
				.map(|stmt| self.check_statement(stmt))
				.collect::<TCResult<Vec<TypedStatement>>>()?
		})
	}

	pub fn check_statement(&mut self, stmt: Statement) -> TCResult<TypedStatement> {
		match stmt {
			Statement::Expression(expr) => self.check_expression(expr)
				.map(|expr| TypedStatement::Expression(expr)),
			stmt => panic!("unimplemented statement type check {:?}", stmt)//TODO Remove rest
		}
	}

	pub fn check_expression(&mut self, expr: Expression) -> TCResult<TypedExpression> {
		match expr {
			Expression::Boolean(value) => Ok(TypedExpression::Boolean(value)),
			expr => panic!("unimplemented expression type check {:?}", expr)//TODO Remove rest
		}
	}
}

type TCResult<T> = Result<T, String>;