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
    let mode: string = configuration.get<string>("lsp.mode", "stable");
    let cpu: string = configuration.get<string>("lsp.cpu", "");
    let cpuFeatures: string = configuration.get<string>("lsp.cpuFeatures", "");
    let stdVersion: string = configuration.get<string>("lsp.stdVersion", "");
    let stdPath: string = configuration.get<string>("lsp.stdPath", "");

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
        initializationOptions: {
            mode,
            cpu,
            cpuFeatures,
            stdVersion,
            stdPath,
        },
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
