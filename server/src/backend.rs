use crate::on_change::url_to_path;
use crate::semantic::{semantic_tokens, CustomSemanticToken, LEGEND_TYPE};
use brim::modules::Module;
use brim::{paths_equal, MainContext};
use dashmap::{DashMap, DashSet};
use serde_json::Value;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};
use std::sync::Arc;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::{error, info};

#[derive(Debug, Clone)]
pub struct Backend {
    pub client: Client,
    pub main_ctx: Arc<Mutex<MainContext>>,
    pub files_with_diagnostics: DashSet<PathBuf>,
    pub semantic_token_map: DashMap<PathBuf, Vec<CustomSemanticToken>>,
    // the compiler can't get the text from file that wasn't saved, so we will use the text provided by the client and update it after initial compilation
    pub updated_content: DashMap<PathBuf, String>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            main_ctx: Arc::new(Mutex::new(MainContext::new())),
            semantic_token_map: DashMap::new(),
            files_with_diagnostics: DashSet::new(),
            updated_content: DashMap::new(),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
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
        self.on_change().await;
        info!("Server initialized");
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, _: DidOpenTextDocumentParams) {
        self.on_change().await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let path = url_to_path(params.text_document.uri.as_ref());
        let content = &params.content_changes[0].text;
        self.updated_content.insert(path.clone(), content.clone());

        self.on_change().await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let path = url_to_path(params.text_document.uri.as_ref());
        self.updated_content.remove(&path);

        self.on_change().await;
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {}

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
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

        let data = self.query_tokens(path.clone());
        let data = self.to_plain_semantics(data);
        info!("Returning semantic tokens full with {} tokens", data.len());
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
        let data = self.query_tokens(path.clone());

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

        Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
            result_id: None,
            data: plain_tokens,
        })))
    }
}

impl Backend {
    async fn get_compiled(&self) -> tokio::sync::MutexGuard<'_, MainContext> {
        self.main_ctx.lock().await
    }

    pub(crate) async fn get_tokens(&self, mut module: Module) -> Vec<CustomSemanticToken> {
        let res = semantic_tokens(&mut module);
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

    pub fn query_tokens(&self, path: PathBuf) -> Vec<CustomSemanticToken> {
        self.semantic_token_map
            .iter()
            .find(|x| paths_equal(x.key(), &path))
            .map(|x| x.value().clone())
            .unwrap()
    }
}
