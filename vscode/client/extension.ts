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
import { activate as lspActivate } from "./lsp";

export const outputChannel = window.createOutputChannel("Brim Extension");

export async function activate(context: ExtensionContext) {
  lspActivate();
}
