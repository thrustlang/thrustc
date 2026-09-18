import * as vscode from "vscode";
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
    let configuration: vscode.WorkspaceConfiguration = vscode.workspace.getConfiguration("thrust");
    let command: string = configuration.get<string>("lsp.path", "thrustc_lsp");
    let args: string[] = configuration.get<string[]>("lsp.args", []);

    let serverOptions: ServerOptions = {
        command,
        args,
        transport: TransportKind.stdio,
    };

    let documentSelector = [
        {
            scheme: "file",
            language: "thrust",
        },
    ];

    let clientOptions: LanguageClientOptions = {
        documentSelector,
    };

    client = new LanguageClient(
        "thrustLanguageServer",
        "Thrust Language Server",
        serverOptions,
        clientOptions,
    );

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }

    return client.stop();
}
