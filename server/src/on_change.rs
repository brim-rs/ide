use std::path::PathBuf;
use log::info;
use crate::Backend;
use percent_encoding::percent_decode;
use ropey::Rope;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, MessageType, Position, Range, TextDocumentItem};

impl Backend {
    pub async fn on_change(&self, doc: TextDocumentItem, path: PathBuf) {
        info!("Performing on_change for {:?}", path);
    }
}

pub fn url_to_path(url: &str) -> String {
    let path = url.replace("file://", "");
    percent_decode(path.as_bytes())
        .decode_utf8_lossy()
        .to_string()
        .strip_prefix('/')
        .expect("Failed to get path")
        .to_string()
}
