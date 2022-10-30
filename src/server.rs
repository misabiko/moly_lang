use std::error::Error;
use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use lsp_types::{GotoDefinitionResponse, InitializeParams, OneOf, ServerCapabilities};
use lsp_types::request::GotoDefinition;

//TODO Feature: Lift return from "if"

pub fn start() -> Result<(), Box<dyn Error + Sync + Send>> {
	//On VSCode, stdout is ignored and stderr will be printed in Output > Moly Language Server
	//TODO Check if stderr is a lsp thing or vscode thing
	eprintln!("starting moly LSP server");

	let (connection, io_threads) = Connection::stdio();

	let server_capabilities = serde_json::to_value(&ServerCapabilities {
		definition_provider: Some(OneOf::Left(true)),
		..Default::default()
	}).unwrap();

	let initialization_params = connection.initialize(server_capabilities)?;
	main_loop(connection, initialization_params)?;
	io_threads.join()?;

	eprintln!("shutting down server");

	Ok(())
}

fn main_loop(
	connection: Connection,
	params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
	let _params: InitializeParams = serde_json::from_value(params).unwrap();
	//TODO Check if we can specify log levels
	eprintln!("starting main loop");

	for msg in &connection.receiver {
		eprintln!("got msg: {:#?}", msg);
		match msg {
			Message::Request(req) => {
				if connection.handle_shutdown(&req)? {
					return Ok(())
				}
				eprintln!("got request: {:#?}", req);
				match cast::<GotoDefinition>(req) {
					Ok((id, params)) => {
						eprintln!("got gotoDefinition request #{}: {:#?}", id, params);

						//TODO Get symbol from request

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
					Err(err @ ExtractError::JsonError { .. }) => panic!("{:#?}", err),
					Err(ExtractError::MethodMismatch(req)) => panic!("method mismatch: {:#?}", req),//TODO test https://github.com/rust-lang/rust-analyzer/blob/d022e0ec536948ced38ac67dec0d64c312264f7c/lib/lsp-server/examples/goto_def.rs#L100
				};
			}
			Message::Response(r) => eprintln!("got response: {:#?}", r),
			Message::Notification(n) => eprintln!("got notification: {:#?}", n),
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