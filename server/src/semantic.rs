use brim::transformer::HirModule;
use ropey::Rope;
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType};

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::STRUCT,
    SemanticTokenType::TYPE,
];

pub fn semantic_tokens(module: &HirModule) -> Vec<SemanticToken> {
    let mut result = vec![];

    result
}
