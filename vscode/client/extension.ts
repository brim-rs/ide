import path from "path";
import {
  CancellationToken,
  commands,
  EventEmitter,
  ExtensionContext,
  InlayHint,
  InlayHintsProvider,
  languages,
  ProviderResult,
  Range,
  Selection,
  TextDocument,
  TextDocumentChangeEvent,
  TextEdit,
  Uri,
  window,
  workspace,
  WorkspaceEdit,
} from "vscode";

import {
  Disposable,
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;
const outputChannel = window.createOutputChannel("Brim Extension");

export async function activate(context: ExtensionContext) {
  outputChannel.appendLine("Activating Brim Language Server extension...");

  // I know this is bad, but only for development
  const command =
    "C:\\dev\\brim-projects\\ide\\target\\debug\\brim-language-server.exe";

  const run: Executable = {
    command,
    options: {
      env: {
        ...process.env,
        RUST_LOG: "debug",
      },
    },
  };

  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };
  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "brim" }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
    },
    outputChannel: outputChannel,
  };

  client = new LanguageClient(
    "brim-language-server",
    "brim language server",
    serverOptions,
    clientOptions,
  );

  await client.start();
  outputChannel.appendLine("Brim Language Server started.");
}

export function deactivate(): Thenable<void> | undefined {
  outputChannel.appendLine("Deactivating Brim Language Server extension...");
  if (!client) {
    return undefined;
  }
  return client.stop();
}
