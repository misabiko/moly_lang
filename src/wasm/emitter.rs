use crate::type_checker::typed_ast::{TypedExpression, TypedStatement, TypedStatementBlock};

pub fn compile_block_with_header(/*block: TypedStatementBlock*/) -> Result<Vec<u8>, String> {
	let mut code = vec![];
	code.extend(MAGIC_MODULE_HEADER);
	code.extend(MODULE_VERSION);

	let float_void_type = flatten(vec![
		vec![FUNCTION_TYPE],
		encode_vector(&[Valtype::F32 as u8]),
		vec![EMPTY_ARRAY]
	]);

	// the type section is a vector of function types
	code.extend(create_section(
		Section::Type,
		&encode_vectors(vec![
			VOID_VOID_TYPE.to_vec(),
			float_void_type,
		])
	));

	// the import section is a vector of imported functions
	code.extend(create_section(
		Section::Import,
		&encode_vectors(vec![flatten(vec![
			encode_string("env"),
			encode_string("print"),
			vec![
				ExportType::Func as u8,
				0x01, // type index
			],
		])])
	));

	// the function section is a vector of type indices that indicate the type of each function
	// in the code section
	code.extend(create_section(
		Section::Func,
		&encode_vector(&[0x00 /* type index */])
	));

	// the export section is a vector of exported functions
	code.extend(create_section(
		Section::Export,
		&encode_vectors(vec![
			flatten(vec![
				encode_string("run"),
				vec![
					ExportType::Func as u8,
					0x01,	// function index
				]
			])
		])
	));

	code.extend(create_section(
		Section::Code,
		&encode_vectors(vec![
			encode_vector(&flatten(vec![
				vec![EMPTY_ARRAY],	//locals
				compile_block(block)?,
				vec![Opcodes::End as u8],
			]))
		]),
	));

	// compile_block(&mut code, block)?;

	Ok(code)
}

fn compile_block(code: &mut Vec<u8>, block: TypedStatementBlock) -> CompilerResult {
	let mut code = vec![];

	for stmt in block.statements {
		compile_statement(&mut code, stmt)?
	}

	Ok(())
}

fn compile_statement(code: &mut Vec<u8>, stmt: TypedStatement) -> CompilerResult {
	match stmt {
		_ => {}
	}

	Ok(())
}

fn compile_expression(code: &mut Vec<u8>, expr: TypedExpression) -> CompilerResult {
	match expr {
		_ => {}
	}

	Ok(())
}

// https://webassembly.github.io/spec/core/binary/modules.html#sections
// sections are encoded by their type followed by their vector contents
fn create_section(section: Section, data: &[u8]) -> Vec<u8> {
	let mut output = vec![section as u8];
	output.extend(encode_vector(data));
	output
}

// https://webassembly.github.io/spec/core/binary/conventions.html#binary-vec
// Vectors are encoded with their length followed by their element sequence
fn encode_vector(data: &[u8]) -> Vec<u8> {
	let mut len = vec![];
	leb128::write::unsigned(&mut len, data.len() as u64).unwrap();
	let mut output = vec![len[0]];
	output.extend(data);
	output
}

fn encode_vectors(data: Vec<Vec<u8>>) -> Vec<u8> {
	let mut len = vec![];
	leb128::write::unsigned(&mut len, data.len() as u64).unwrap();
	let mut output = vec![len[0]];
	output.extend(flatten(data));
	output
}

fn encode_string(s: &str) -> Vec<u8> {
	let mut e = vec![s.len() as u8];
	e.extend(s.bytes().collect::<Vec<u8>>());
	e
}

fn flatten(data: Vec<Vec<u8>>) -> Vec<u8> {
	data.into_iter().flatten().collect()
}

type CompilerResult = Result<(), String>;

// http://webassembly.github.io/spec/core/binary/types.html#function-types
const FUNCTION_TYPE: u8 = 0x60;
const EMPTY_ARRAY: u8 = 0x0;

const MAGIC_MODULE_HEADER: [u8; 4] = [0x00, 0x61, 0x73, 0x6d];
const MODULE_VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

const VOID_VOID_TYPE: [u8; 3] = [FUNCTION_TYPE, EMPTY_ARRAY, EMPTY_ARRAY];

// https://webassembly.github.io/spec/core/binary/modules.html#sections
#[repr(u8)]
enum Section {
	Custom = 0,
	Type = 1,
	Import = 2,
	Func = 3,
	Table = 4,
	Memory = 5,
	Global = 6,
	Export = 7,
	Start = 8,
	Element = 9,
	Code = 10,
	Data = 11,
}

// https://webassembly.github.io/spec/core/binary/types.html
#[repr(u8)]
enum Valtype {
	I32 = 0x7f,
	F32 = 0x7d,
}

// http://webassembly.github.io/spec/core/binary/modules.html#export-section
#[repr(u8)]
enum ExportType {
	Func = 0x00,
	Table = 0x01,
	Mem = 0x02,
	Global = 0x03,
}

// https://webassembly.github.io/spec/core/binary/instructions.html
#[repr(u8)]
enum Opcodes {
	End = 0x0b,
	Call = 0x10,
	GetLocal = 0x20,
	F32Const = 0x43,
	F32Add = 0x92,
}