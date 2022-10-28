import { ExtensionContext, workspace } from "vscode";
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	//TODO Get server path

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

	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}