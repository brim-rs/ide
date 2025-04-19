#![feature(let_chains)]

use crate::backend::Backend;
use brim::MainContext;
use dashmap::{DashMap, DashSet};
use std::collections::{HashMap, HashSet};
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use tokio::sync::Mutex;
use tower_lsp::{LspService, Server};
use tracing::info;
use tracing_subscriber::EnvFilter;

mod backend;
mod on_change;
mod semantic;
mod span;

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
    let (service, socket) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
