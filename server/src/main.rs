mod on_change;
mod semantic;

use crate::on_change::url_to_path;
use crate::semantic::{semantic_tokens, LEGEND_TYPE};
use dashmap::DashMap;
use ropey::Rope;
use serde_json::Value;
use std::mem;
use std::path::PathBuf;
use brim::transformer::HirModule;
use tokio::net::{TcpListener, TcpStream};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::{error, info};
use tracing_subscriber::{fmt, EnvFilter};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

#[derive(Debug)]
struct Backend {
    client: Client,
    modules: DashMap<String, HirModule>,
    ropes: DashMap<String, Rope>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    ..Default::default()
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("brim".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                ..ServerCapabilities::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("Server initialized");
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, did_open: DidOpenTextDocumentParams) {
        let path: PathBuf = url_to_path(&did_open.text_document.uri.to_string()).into();

        self.on_change(
            TextDocumentItem {
                uri: did_open.text_document.uri,
                language_id: did_open.text_document.language_id,
                version: did_open.text_document.version,
                text: did_open.text_document.text,
            },
            path,
        )
        .await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        let path: PathBuf = url_to_path(&params.text_document.uri.to_string()).into();

        self.on_change(
            TextDocumentItem {
                uri: params.text_document.uri.clone(),
                language_id: "brim".to_string(),
                version: params.text_document.version,
                text: params.content_changes.pop().unwrap().text,
            },
            path,
        )
        .await;
    }
    async fn did_save(&self, _: DidSaveTextDocumentParams) {
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {

    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => error!("Error applying edit: {:?}", err),
        }

        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();

        let module = self.modules.get(&uri).unwrap().value().clone();
        let rope = self.ropes.get(&uri).unwrap().value().clone();
        let tokens = semantic_tokens(module, rope);

        info!("Returning semantic tokens full");
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri.to_string();

        let module = self.modules.get(&uri).unwrap().value().clone();
        let rope = self.ropes.get(&uri).unwrap().value().clone();
        let tokens = semantic_tokens(module, rope);

        info!("Returning semantic tokens range");
        Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let stderr_logger = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::new("info"))
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .without_time()
        .finish();

    tracing::subscriber::set_global_default(stderr_logger)
        .expect("Failed to set global default subscriber");

    info!("Starting server");
    let (service, socket) = LspService::new(|client| Backend {
        client,
        modules: Default::default(),
        ropes: Default::default(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}