use crate::on_change::url_to_path;
use crate::semantic::{semantic_tokens, CustomSemanticToken, LEGEND_TYPE};
use brim::CompiledModules;
use serde_json::Value;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::{error, info};

#[derive(Debug)]
pub struct Backend {
    pub client: Client,
    pub compiled: Arc<Mutex<CompiledModules>>,
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

        if let Err(err) = self.initial_scan().await {
            let msg = format!("Failed to run initial scan: {err}");
            self.client
                .show_message(MessageType::ERROR, msg.clone())
                .await;
            error!("{msg}");
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, did_open: DidOpenTextDocumentParams) {
        // let path: PathBuf = url_to_path(&did_open.text_document.uri.to_string()).into();
        //
        // self.on_change(
        //     TextDocumentItem {
        //         uri: did_open.text_document.uri,
        //         language_id: did_open.text_document.language_id,
        //         version: did_open.text_document.version,
        //         text: did_open.text_document.text,
        //     },
        //     path,
        // )
        // .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        // let path: PathBuf = url_to_path(&params.text_document.uri.to_string()).into();
        //
        // self.on_change(
        //     TextDocumentItem {
        //         uri: params.text_document.uri.clone(),
        //         language_id: "brim".to_string(),
        //         version: params.text_document.version,
        //         text: params.content_changes.pop().unwrap().text,
        //     },
        //     path,
        // )
        // .await;
    }
    async fn did_save(&self, _: DidSaveTextDocumentParams) {}

    async fn did_close(&self, _: DidCloseTextDocumentParams) {}

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {}

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {}

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {}

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
        let path = url_to_path(params.text_document.uri.as_ref());

        let data = self.to_plain_semantics(self.get_tokens(path).await);
        info!("Returning semantic tokens full");
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        })))
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let path = url_to_path(params.text_document.uri.as_ref());
        let start = params.range.start;
        let end = params.range.end;
        let data = self.get_tokens(path).await;

        let filtered_tokens = data
            .iter()
            .filter(|token| {
                let token_pos = token.pos;

                let after_start = token_pos.line > start.line
                    || (token_pos.line == start.line && token_pos.character >= start.character);

                let before_end = token_pos.line < end.line
                    || (token_pos.line == end.line && token_pos.character < end.character);

                after_start && before_end
            })
            .cloned()
            .collect::<Vec<_>>();

        let plain_tokens = self.to_plain_semantics(filtered_tokens);

        info!("Returning semantic tokens range");
        Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
            result_id: None,
            data: plain_tokens,
        })))
    }
}

impl Backend {
    async fn get_compiled(&self) -> tokio::sync::MutexGuard<'_, CompiledModules> {
        self.compiled.lock().await
    }

    async fn get_tokens(&self, path: PathBuf) -> Vec<CustomSemanticToken> {
        let compiled = self.get_compiled().await;
        let module = compiled.find_module_by_path(path);

        if let Some(ref mut module) = module.cloned() {
            info!("Found module for tokens_full: {:?}", module.path);
            let res = semantic_tokens(module);
            if let Err(err) = res {
                let msg = format!("Failed to run semantic analyze: {err}");
                self.client
                    .show_message(MessageType::ERROR, msg.clone())
                    .await;
                error!("{msg}");

                vec![]
            } else {
                res.unwrap()
            }
        } else {
            vec![]
        }
    }

    fn to_plain_semantics(&self, tokens: Vec<CustomSemanticToken>) -> Vec<SemanticToken> {
        tokens
            .iter()
            .map(|token| SemanticToken {
                delta_line: token.delta_line,
                delta_start: token.delta_start,
                length: token.length,
                token_type: token.token_type,
                token_modifiers_bitset: 0,
            })
            .collect()
    }
}
