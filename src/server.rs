use std::error::Error;
use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use lsp_types::{GotoDefinitionResponse, InitializeParams, OneOf, ServerCapabilities};
use lsp_types::request::GotoDefinition;

pub fn start() -> Result<(), Box<dyn Error + Sync + Send>> {
	//TODO Confirm stdout works https://github.com/rust-analyzer/lsp-server/blob/b4219ec00c9b7181494933acc817380180776572/examples/goto_def.rs#L55
	println!("starting moly LSP server");

	let (connection, io_threads) = Connection::stdio();

	let server_capabilities = serde_json::to_value(&ServerCapabilities {
		definition_provider: Some(OneOf::Left(true)),
		..Default::default()
	}).unwrap();

	let initialization_params = connection.initialize(server_capabilities)?;
	main_loop(connection, initialization_params)?;
	io_threads.join()?;

	println!("shutting down server");

	Ok(())
}

fn main_loop(
	connection: Connection,
	params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
	let _params: InitializeParams = serde_json::from_value(params).unwrap();
	println!("starting main loop");

	for msg in &connection.receiver {
		println!("got msg: {:?}", msg);
		match msg {
			Message::Request(req) => {
				if connection.handle_shutdown(&req)? {
					return Ok(())
				}
				println!("got request: {:?}", req);
				match cast::<GotoDefinition>(req) {
					Ok((id, params)) => {
						println!("got gotoDefinition request #{}: {:?}", id, params);
						let result = Some(GotoDefinitionResponse::Array(Vec::new()));
						let result = serde_json::to_value(&result).unwrap();
						let resp = Response {
							id,
							result: Some(result),
							error: None,
						};
						connection.sender.send(Message::Response(resp))?;
						continue;
					}
					Err(err @ ExtractError::JsonError { .. }) => panic!("{:?}", err),
					Err(ExtractError::MethodMismatch(req)) => panic!("method mismatch: {:?}", req),//TODO test https://github.com/rust-lang/rust-analyzer/blob/d022e0ec536948ced38ac67dec0d64c312264f7c/lib/lsp-server/examples/goto_def.rs#L100
				};
			}
			Message::Response(r) => println!("got response: {:?}", r),
			Message::Notification(n) => println!("got notification: {:?}", n),
		}
	}

	Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
	where
		R: lsp_types::request::Request,
		R::Params: serde::de::DeserializeOwned,
{
	req.extract(R::METHOD)
}
