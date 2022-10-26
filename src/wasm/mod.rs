use wasm_bindgen::prelude::*;
use serde::Serialize;

mod emitter;
mod encoding;

pub use emitter::compile_block_with_header;
use crate::build_wasm;

#[wasm_bindgen]
pub fn eval_wasm(input: String) -> Result<JsValue, JsValue> {
	let bytecode = match build_wasm(&input, false) {
		Ok(b) => b,
		Err(err) => {
			web_sys::console::error_1(&JsValue::from_str(&err));
			vec![]
		}
	};

	let wat = match wasmprinter::print_bytes(bytecode.clone()) {
		Ok(w) => w,
		Err(err) => {
			web_sys::console::error_1(&JsValue::from_str(&err.to_string()));
			String::new()
		},
	};

	Ok(serde_wasm_bindgen::to_value(&Bytecode {
		bytecode,
		wat,
	})?)
}

#[derive(Serialize)]
pub struct Bytecode {
	pub bytecode: Vec<u8>,
	pub wat: String,
}