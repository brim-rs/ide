import * as vscode from "vscode";
import { outputChannel } from "./extension";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";
export let client: LanguageClient | null = null;

export async function activate() {
  try {
    const path = getExecPath();

    if (!path) {
      await stopClient();
      return;
    }

    const newClient = await startClient(path);
    stopClient();
    client = newClient;
  } catch (err) {
    vscode.window.showWarningMessage(
      `Failed to run Brim LSP${err instanceof Error ? `: ${err.message}` : ""}`,
    );
  }
}

const startClient = async (path: string): Promise<LanguageClient> => {
  const run: Executable = {
    command: path,
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
    documentSelector: [
      { scheme: "file", language: "brim" },
      { scheme: "untitled", language: "brim" },
    ],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/.clientrc"),
    },
    outputChannel: outputChannel,
  };

  const langClient = new LanguageClient(
    "brim-language-server",
    "brim language server",
    serverOptions,
    clientOptions,
  );

  await langClient.start();

  return langClient;
};

const getExecPath = (): string => {
  const config = vscode.workspace.getConfiguration("brim");
  const path = config.get("lspPath") as string | undefined;
  if (!path) {
    throw new Error("No LSP path configured.");
  }
  return path;
};

const stopClient = async (): Promise<void> => {
  if (!client) return;
  const oldClient = client;
  client = null;
  await oldClient.stop();
  await oldClient.dispose();
};
