import { ExtensionContext } from "vscode";
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';

//TODO Add tests

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	//TODO Call "cargo run moly -- server" on debug
	const serverOptions: ServerOptions = {
		command: 'moly',
		args: ['server'],
	};

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'moly'}],
		// synchronize: {
		// 	fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		// }
	};

	client = new LanguageClient(
		'moly',
		'Moly Language Server',
		serverOptions,
		clientOptions
	);

	//TODO Log server responses from client?

	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}