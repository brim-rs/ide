use anyhow::Result;
use brim::ast::expr::{Expr, ExprKind};
use brim::ast::item::{ImportsKind, Item, ItemKind, Use};
use brim::ast::token::{LitKind, TokenKind};
use brim::files::{get_file, Files, SimpleFile};
use brim::span::Span;
use brim::transformer::HirModule;
use brim::walker::AstWalker;
use ropey::Rope;
use std::time::Instant;
use tower_lsp::lsp_types::{Position, SemanticToken, SemanticTokenType};
use tracing::info;

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

#[derive(Debug, Eq, PartialEq, Copy, Clone, Default)]
pub struct CustomSemanticToken {
    pub delta_line: u32,
    pub delta_start: u32,
    pub length: u32,
    pub token_type: u32,
    pub token_modifiers_bitset: u32,
    pub pos: Position,
}

pub fn semantic_tokens(module: &mut HirModule) -> Result<Vec<CustomSemanticToken>> {
    let start = Instant::now();
    let file = get_file(module.mod_id.as_usize())?;
    let mut analyzer = SemanticAnalyzer::new(file.clone());

    for item in &mut module.barrel.items {
        analyzer.walk_item(item);
    }

    let tokens = analyzer.build_semantic_tokens();
    info!(
        "Finished semantic analyze with {} tokens in {:.2?}",
        tokens.len(),
        start.elapsed()
    );

    Ok(tokens)
}

pub struct SemanticAnalyzer {
    pub tokens: Vec<(usize, usize, u32, SemanticTokenType)>,
    pub file: SimpleFile,
}

impl SemanticAnalyzer {
    pub fn new(file: SimpleFile) -> Self {
        Self {
            tokens: Vec::new(),
            file,
        }
    }

    pub fn add_span(&mut self, kind: SemanticTokenType, span: Span) {
        let line = self
            .file
            .line_index((), span.start().to_usize())
            .unwrap_or(0);
        let start = self
            .file
            .column_number((), line, span.start().to_usize())
            .unwrap_or(0)
            - 1;

        // Store the raw position and length, not deltas
        self.tokens.push((line, start, span.length() as u32, kind));
    }

    pub fn build_semantic_tokens(&mut self) -> Vec<CustomSemanticToken> {
        // Sort by line and then by column
        self.tokens
            .sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));

        let mut result = Vec::with_capacity(self.tokens.len());
        let mut pre_line = 0;
        let mut pre_start = 0;

        for (line, start, length, kind) in &self.tokens {
            let delta_line = (*line - pre_line) as u32;
            let delta_start = if delta_line == 0 {
                *start - pre_start
            } else {
                *start
            } as u32;

            result.push(CustomSemanticToken {
                delta_line,
                delta_start,
                length: *length,
                token_type: LEGEND_TYPE.iter().position(|t| t == kind).unwrap_or(0) as u32,
                token_modifiers_bitset: 0,
                pos: Position {
                    line: *line as u32,
                    character: *start as u32,
                },
            });

            pre_line = *line;
            pre_start = *start;
        }

        result
    }
}

impl AstWalker for SemanticAnalyzer {
    fn walk_item(&mut self, item: &mut Item) {
        info!("walking item {}", item.ident);
        for attr in &mut item.attrs {
            self.add_span(SemanticTokenType::OPERATOR, attr.at_span);
            self.add_span(SemanticTokenType::DECORATOR, attr.name.span);

            self.add_span(SemanticTokenType::OPERATOR, attr.name.span.move_by(1));
            for expr in &mut attr.args {
                self.walk_expr(expr);
            }
            self.add_span(SemanticTokenType::OPERATOR, attr.name.span.from_end());
        }
        self.add_span(SemanticTokenType::KEYWORD, item.vis.span);

        match &mut item.kind {
            ItemKind::Fn(func) => self.visit_fn(func),
            ItemKind::Use(use_stmt) => self.visit_use(use_stmt),
            ItemKind::Struct(str) => self.visit_struct(str),
            ItemKind::TypeAlias(type_alias) => self.visit_type_alias(type_alias),
            ItemKind::External(external) => {
                for item in &mut external.items {
                    match &mut item.kind {
                        ItemKind::Fn(func) => self.visit_fn(func),
                        ItemKind::TypeAlias(ty) => self.visit_type_alias(ty),
                        _ => unreachable!("not allowed"),
                    }
                }
            }
            ItemKind::Enum(_) => {}
            ItemKind::Namespace(_) | ItemKind::Module(_) => {}
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        match &expr.kind {
            ExprKind::Literal(lit, span) => {
                let kind = match lit.kind {
                    LitKind::Str
                    | LitKind::ByteStr
                    | LitKind::Char
                    | LitKind::Byte
                    | LitKind::CStr => SemanticTokenType::STRING,
                    LitKind::Integer | LitKind::Float => SemanticTokenType::NUMBER,
                    LitKind::Bool => SemanticTokenType::KEYWORD,

                    // only supposed to be found after comptime evaluation
                    LitKind::None | LitKind::Err(_) => unreachable!(),
                };

                self.add_span(kind, *span);
            }
            _ => {}
        }
    }

    fn visit_use(&mut self, use_stmt: &mut Use) {
        self.add_span(SemanticTokenType::KEYWORD, use_stmt.use_span);
        match &use_stmt.imports {
            ImportsKind::Default(ident) => {
                self.add_span(SemanticTokenType::VARIABLE, ident.span);
            }
            ImportsKind::List(idents, commas, (obrace, cbrace)) => {
                self.add_span(SemanticTokenType::OPERATOR, *obrace);
                for (ident, comma) in idents.iter().zip(commas.iter()) {
                    self.add_span(SemanticTokenType::VARIABLE, ident.span);
                    self.add_span(SemanticTokenType::OPERATOR, *comma);
                }
                self.add_span(SemanticTokenType::OPERATOR, *cbrace);
            }
            ImportsKind::All(star) => {
                self.add_span(SemanticTokenType::OPERATOR, *star);
            }
        }
        self.add_span(SemanticTokenType::KEYWORD, use_stmt.from_span);
        self.add_span(SemanticTokenType::STRING, use_stmt.path_span);
    }
}
