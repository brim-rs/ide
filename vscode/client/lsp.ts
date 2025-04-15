import * as vscode from 'vscode';
import { outputChannel } from './extension';
import { LanguageClient } from 'vscode-languageclient/node';
export let client: LanguageClient | null = null;

export async function activate() {
    const path = getExecPath();

    if (!path) {
        await stopClient();
        return 
    }

    try {
        const client = await startClient(path);
    }
}

const getExecPath = (): string => {
    const config = vscode.workspace.getConfiguration("brim");
    const path = config.get("path") as string | undefined;
    if (!path) {
        throw new Error("TODO: No LSP path configured.");
    }
    return path;
}


const  stopClient = async (): Promise<void> =>{
    if (!client) return;
    const oldClient = client;
    client = null;
    await oldClient.stop();
    await oldClient.dispose();
}