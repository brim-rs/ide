use anyhow::Result;
use brim::ast::expr::{BinOpKind, Expr, ExprKind, Match, UnaryOp};
use brim::ast::item::{
    Block, FnDecl, FnReturnType, GenericKind, Generics, ImportsKind, Item, ItemKind, Struct,
    TypeAlias, TypeAliasValue, Use, VisibilityKind,
};
use brim::ast::stmts::{IfStmt, Let, Stmt, StmtKind};
use brim::ast::token::{LitKind, TokenKind};
use brim::ast::ty::{Mutable, Ty, TyKind};
use brim::files::{get_file, Files, SimpleFile};
use brim::span::Span;
use brim::transformer::HirModule;
use brim::walker::AstWalker;
use std::time::Instant;
use tower_lsp::lsp_types::{Position, SemanticTokenType};
use tracing::{info, warn};

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
    info!("Starting semantic analyze");
    let file = get_file(module.mod_id.as_usize())?;
    let mut analyzer = SemanticAnalyzer::new(file.clone());

    for comment in &module.barrel.comments {
        analyzer.add_span(SemanticTokenType::COMMENT, comment.clone());
    }

    for token in &module.barrel.tokens {
        let kind = match token.kind {
            TokenKind::At => SemanticTokenType::METHOD,
            _ if token.is_operator() | token.is_separator() => SemanticTokenType::OPERATOR,
            _ if token.is_any_keyword() => SemanticTokenType::KEYWORD,
            _ => continue,
        };
        analyzer.add_span(kind, token.span);
    }

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

    pub fn add_any_double(&mut self, double: (Span, Span)) {
        self.add_span(SemanticTokenType::OPERATOR, double.0);
        self.add_span(SemanticTokenType::OPERATOR, double.1);
    }
}

impl AstWalker for SemanticAnalyzer {
    fn walk_item(&mut self, item: &mut Item) {
        for attr in &mut item.attrs {
            self.add_span(SemanticTokenType::OPERATOR, attr.at_span);
            self.add_span(SemanticTokenType::DECORATOR, attr.name.span);

            self.add_span(SemanticTokenType::OPERATOR, attr.name.span.move_by(1));
            for expr in &mut attr.args {
                self.visit_expr(expr);
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
                if let Some((_, span)) = &external.abi {
                    self.add_span(SemanticTokenType::STRING, span.clone());
                }

                for item in &mut external.items {
                    match &mut item.kind {
                        ItemKind::Fn(func) => self.visit_fn(func),
                        ItemKind::TypeAlias(ty) => self.visit_type_alias(ty),
                        _ => unreachable!("not allowed"),
                    }
                }
            }
            ItemKind::Enum(en) => {
                self.visit_generics(&mut en.generics);
                self.add_span(SemanticTokenType::ENUM, en.ident.span);

                for variant in &mut en.variants {
                    self.add_span(SemanticTokenType::ENUM_MEMBER, variant.ident.span);
                    for field in &mut variant.fields {
                        self.visit_ty(&mut field.ty);
                    }
                }
            }
            ItemKind::Module(module) => {
                for ident in &module.idents {
                    self.add_span(SemanticTokenType::NAMESPACE, ident.span);
                }
            }
            ItemKind::Namespace(_) => {}
        }

        for semi in &item.semis {
            self.add_span(SemanticTokenType::OPERATOR, *semi);
        }
    }

    fn visit_type_alias(&mut self, alias: &mut TypeAlias) {
        self.add_span(SemanticTokenType::TYPE, alias.ident.span);

        match &alias.ty {
            TypeAliasValue::Ty(ty) => self.visit_ty(&mut ty.clone()),
            TypeAliasValue::Const(expr) => self.visit_expr(&mut expr.clone()),
        }
    }

    fn visit_struct(&mut self, str: &mut Struct) {
        self.add_span(SemanticTokenType::STRUCT, str.ident.span);

        self.visit_generics(&mut str.generics);

        for field in &mut str.fields {
            if field.vis.kind == VisibilityKind::Public {
                self.add_span(SemanticTokenType::KEYWORD, field.vis.span);
            }

            self.add_span(SemanticTokenType::PROPERTY, field.ident.span);
            self.add_span(SemanticTokenType::OPERATOR, field.colon);
            self.visit_ty(&mut field.ty);
        }

        for item in &mut str.items {
            self.walk_item(item);
        }
    }

    fn visit_block(&mut self, block: &mut Block) {
        for stmt in &mut block.stmts {
            self.walk_stmt(stmt);
        }
    }

    fn visit_generics(&mut self, generics: &mut Generics) {
        if let Some((o, c)) = generics.chevrons {
            self.add_any_double((o, c));
        }

        for param in &mut generics.params {
            self.add_span(SemanticTokenType::TYPE, param.ident.span);

            match &param.kind {
                GenericKind::Type { default, colon } => {
                    if let Some(def) = default {
                        self.visit_ty(&mut def.clone());
                        self.add_span(SemanticTokenType::OPERATOR, colon.unwrap());
                    }
                }
                GenericKind::NonType {
                    default,
                    ty,
                    eq,
                    colon,
                } => {
                    self.visit_ty(&mut ty.clone());
                    self.add_span(SemanticTokenType::OPERATOR, *colon);
                    if let Some(def) = default {
                        self.visit_expr(&mut def.clone());
                        self.add_span(SemanticTokenType::OPERATOR, eq.unwrap());
                    }
                }
            }
        }
    }

    fn visit_fn(&mut self, func: &mut FnDecl) {
        if let Some(cnst) = func.sig.constant {
            self.add_span(SemanticTokenType::KEYWORD, cnst);
        }
        self.add_span(SemanticTokenType::FUNCTION, func.sig.name.span);

        self.visit_generics(&mut func.generics);

        for param in &mut func.sig.params {
            self.add_span(SemanticTokenType::PARAMETER, param.name.span);
            self.add_span(SemanticTokenType::OPERATOR, param.colon);
            self.visit_ty(&mut param.ty);
        }
        if let FnReturnType::Ty(ty) = &mut func.sig.return_type {
            self.visit_ty(ty);
        }

        if let Some(body) = &mut func.body {
            self.visit_block(body);
        }
    }

    fn visit_ty(&mut self, ty: &mut Ty) {
        match &mut ty.kind {
            TyKind::Ident {
                ident, generics, ..
            } => {
                self.add_span(SemanticTokenType::TYPE, ident.span);
                if let Some((obrace, cbrace)) = generics.braces {
                    self.add_any_double((obrace, cbrace));
                }

                for arg in &mut generics.params {
                    self.visit_ty(&mut arg.ty);
                }
            }
            TyKind::Primitive(prim) => {
                self.add_span(SemanticTokenType::TYPE, ty.span);
            }
            TyKind::Ref(span, ty, mutable) | TyKind::Ptr(span, ty, mutable) => {
                self.add_span(SemanticTokenType::OPERATOR, span.clone());

                if let Mutable::Yes(span) = mutable {
                    self.add_span(SemanticTokenType::KEYWORD, *span);
                }

                self.visit_ty(ty);
            }
            TyKind::Const(span, ty) => {
                self.add_span(SemanticTokenType::KEYWORD, *span);
                self.visit_ty(ty);
            }
            TyKind::Mut(ty, span) => {
                self.add_span(SemanticTokenType::KEYWORD, *span);
                self.visit_ty(ty);
            }
            TyKind::Option(inner, span) => {
                self.visit_ty(inner);
                self.add_span(SemanticTokenType::OPERATOR, *span);
            }
            TyKind::Result(ok, err) => {
                self.visit_ty(ok);
                self.add_span(SemanticTokenType::OPERATOR, ok.span.from_end().move_by(1));
                self.visit_ty(err);
            }
            TyKind::Vec(ty) => {
                self.visit_ty(ty);
            }
            _ => warn!("not implemented for ty {:?}", ty),
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
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
            ExprKind::Return(expr, span) => {
                self.add_span(SemanticTokenType::KEYWORD, span.clone());

                self.visit_expr(expr);
            }
            ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    self.walk_stmt(stmt);
                }
            }
            ExprKind::Type(ty) => self.visit_ty(ty),
            ExprKind::Var(ident) => {
                self.add_span(SemanticTokenType::VARIABLE, ident.span);
            }
            ExprKind::Paren(expr) => {
                self.visit_expr(expr);
            }
            ExprKind::Unary(span, op, operand) => {
                match op.clone() {
                    UnaryOp::Try => self.add_span(SemanticTokenType::KEYWORD, span.clone()),
                    _ => self.add_span(SemanticTokenType::OPERATOR, span.clone()),
                }
                self.visit_expr(operand);
            }
            ExprKind::Binary(lhs, op, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::Array(args) => {
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            ExprKind::Path(idents) | ExprKind::Field(idents) => {
                for ident in idents {
                    self.add_span(SemanticTokenType::VARIABLE, ident.span);
                }
            }
            ExprKind::Assign(lhs, rhs) | ExprKind::AssignOp(lhs, _, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::Index(expr, index) => {
                self.visit_expr(expr);
                self.visit_expr(index);
            }
            ExprKind::Call(ident, args) => {
                self.add_span(SemanticTokenType::METHOD, ident.span);

                for arg in args {
                    self.visit_expr(arg);
                }
            }
            ExprKind::MethodCall(idents, call) => {
                for ident in idents {
                    self.add_span(SemanticTokenType::VARIABLE, ident.span);
                }

                self.visit_expr(call);
            }
            ExprKind::StaticAccess(idents, expr) => {
                for (i, ident) in idents.iter().enumerate() {
                    if i == 0 {
                        self.add_span(SemanticTokenType::STRUCT, ident.span);
                    } else {
                        self.add_span(SemanticTokenType::VARIABLE, ident.span);
                    }
                }

                self.visit_expr(expr);
            }
            ExprKind::StructConstructor(ident, generics, fields) => {
                self.add_span(SemanticTokenType::STRUCT, ident.span);
                for arg in &mut generics.params {
                    self.visit_ty(&mut arg.ty);
                }

                for (ident, expr) in fields {
                    self.add_span(SemanticTokenType::PROPERTY, ident.span);
                    self.visit_expr(expr);
                }
            }
            ExprKind::Builtin(ident, call) => {
                self.add_span(SemanticTokenType::FUNCTION, ident.span);
                for arg in call {
                    self.visit_expr(arg);
                }
            }
            ExprKind::Ternary(cond, then, el) => {
                self.visit_expr(cond);
                self.visit_expr(then);
                self.visit_expr(el);
            }
            ExprKind::Unwrap(expr) => {
                self.visit_expr(expr);
            }
            ExprKind::Match(mt) => self.visit_match(mt),
            _ => warn!("not implemented for expr {:?}", expr),
        }
    }

    fn walk_stmt(&mut self, stmt: &mut Stmt) {
        match &mut stmt.kind {
            StmtKind::Let(let_stmt) => self.visit_let(let_stmt),
            StmtKind::Expr(expr) => self.visit_expr(expr),
            StmtKind::If(stmt) => self.visit_if(stmt),
            StmtKind::Match(mt) => self.visit_match(mt),
        }
    }

    fn visit_if(&mut self, if_stmt: &mut IfStmt) {
        self.visit_expr(&mut if_stmt.condition);
        self.visit_expr(&mut if_stmt.then_block);

        for else_if in &mut if_stmt.else_ifs {
            self.visit_expr(&mut else_if.condition);
            self.visit_expr(&mut else_if.block);
        }

        if let Some(else_block) = &mut if_stmt.else_block {
            self.visit_expr(else_block);
        }
    }

    fn visit_let(&mut self, let_stmt: &mut Let) {
        self.add_span(SemanticTokenType::KEYWORD, let_stmt.keyword);
        self.add_span(SemanticTokenType::VARIABLE, let_stmt.ident.span);

        if let Some(ty) = &let_stmt.ty {
            self.add_span(SemanticTokenType::OPERATOR, let_stmt.colon.unwrap());
            self.visit_ty(&mut ty.clone());
        }

        if let Some(val) = &let_stmt.value {
            self.add_span(SemanticTokenType::OPERATOR, let_stmt.eq.unwrap());
            self.visit_expr(&mut val.clone());
        }
    }

    fn visit_use(&mut self, use_stmt: &mut Use) {
        self.add_span(SemanticTokenType::KEYWORD, use_stmt.use_span);
        match &use_stmt.imports {
            ImportsKind::Default(ident) => {
                self.add_span(SemanticTokenType::VARIABLE, ident.span);
            }
            ImportsKind::List(idents, (obrace, cbrace)) => {
                self.add_span(SemanticTokenType::OPERATOR, *obrace);
                for ident in idents {
                    self.add_span(SemanticTokenType::VARIABLE, ident.span);
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
