use std::sync::Arc;
use brim::CompiledModules;
use tokio::sync::Mutex;
use tower_lsp::{LspService, Server};
use tracing::info;
use tracing_subscriber::EnvFilter;
use crate::backend::Backend;

mod on_change;
mod semantic;
mod backend;


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
        compiled: Arc::new(Mutex::new(CompiledModules::new())),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}