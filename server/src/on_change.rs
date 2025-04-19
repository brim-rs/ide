use crate::span::byte_span_to_range;
use crate::Backend;
use anyhow::Result;
use brim::args::RunArgs;
use brim::compiler::CompilerContext;
use brim::config::toml::Config;
use brim::diagnostic::{Diagnostic, LabelStyle, Severity};
use brim::discover::ModuleDiscover;
use brim::errors::{MainFunctionNotFunction, NoMainFunction};
use brim::files::{files, get_file, get_path, update_file, SimpleFiles};
use brim::graph::ProjectResolver;
use brim::items::HirItemKind;
use brim::lints::Lints;
use brim::modules::ModuleMap;
use brim::parser::Parser;
use brim::resolver::ImportResolver;
use brim::session::Session;
use brim::temp_diag::TemporaryDiagnosticContext;
use brim::transformer::HirModuleMap;
use brim::{paths_equal, MainContext, ModuleId, Project, SimpleModules};
use dashmap::DashMap;
use percent_encoding::percent_decode;
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::atomic::Ordering;
use tower_lsp::lsp_types::{
    Diagnostic as VSCodeDiag, DiagnosticSeverity, MessageType, Position, Range, TextDocumentItem,
    Url,
};
use tower_lsp::{Client, LanguageServer};
use tracing::{error, info};

impl Backend {
    pub async fn on_change(&self) {
        *self.main_ctx.lock().await = MainContext::new();

        if let Err(err) = self.scan().await {
            let msg = format!("Failed to run scan: {err}");

            for path in self.files_with_diagnostics.iter() {
                self.client
                    .publish_diagnostics(path_to_uri(&path), vec![], None)
                    .await;
            }

            self.client
                .show_message(MessageType::ERROR, msg.clone())
                .await;
        }
    }

    pub async fn scan(&self) -> Result<()> {
        let start = std::time::Instant::now();
        let config = Config::get(&std::env::current_dir()?, None)?;
        let args = RunArgs::default();
        let lints = Lints::configure(&config.lints);

        let mut resolver = ProjectResolver::new(".");
        let order = resolver.resolve_project()?;
        let configs = resolver.get_configs(&order);

        let mut compiled_projects = MainContext::new();
        let mut simple = SimpleModules {
            items: Default::default(),
        };
        let mut diagnostics = vec![];
        let mut compiled = vec![];

        for config in configs {
            let sess = &mut Session::new(config.cwd.clone(), config.clone(), args.color_choice);
            let ctx = &mut CompilerContext::new(args.clone(), lints.clone());

            let result = match compile_project(
                sess,
                ctx,
                &mut compiled_projects,
                &mut simple,
                self.updated_content.clone(),
            ) {
                Ok(hir) => {
                    for path in self.files_with_diagnostics.iter() {
                        self.client
                            .publish_diagnostics(path_to_uri(&path), vec![], None)
                            .await;
                    }

                    hir
                }
                Err((result, diags)) => {
                    for diag in diags {
                        for label in diag.labels {
                            let path = get_path(label.file_id)?;

                            let diag = VSCodeDiag {
                                severity: map_severity(label.style),
                                message: label.message,
                                range: byte_span_to_range(
                                    &SimpleFiles::from_files(files()),
                                    label.file_id,
                                    label.range,
                                )?,
                                ..Default::default()
                            };

                            self.files_with_diagnostics.insert(path.clone());
                            diagnostics.push((path_to_uri(&path), diag));
                        }
                    }

                    result
                }
            };

            compiled_projects.map.insert(
                config.project.name.clone(),
                Project {
                    config,
                    hir: result.hir_map.clone(),
                },
            );

            compiled.push(result);
        }

        let projects = compiled_projects
            .map
            .keys()
            .cloned()
            .collect::<Vec<_>>()
            .join(", ");

        *self.main_ctx.lock().await = compiled_projects.clone();

        for c in &compiled {
            for module in &c.map.modules {
                self.semantic_token_map
                    .insert(module.path.clone(), self.get_tokens(module.clone()).await);
            }
        }

        _ = self.client.semantic_tokens_refresh().await;

        for (path, diag) in diagnostics {
            self.client
                .publish_diagnostics(path, vec![diag], None)
                .await;
        }

        info!("Finished scan with {} in {:.2?}", projects, start.elapsed());

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct CompileResult {
    pub map: ModuleMap,
    pub hir_map: HirModuleMap,
}

impl CompileResult {
    pub fn new(map: ModuleMap, hir_map: HirModuleMap) -> Self {
        Self { map, hir_map }
    }
}

pub fn compile_project(
    sess: &mut Session,
    comp: &mut CompilerContext,
    main_ctx: &mut MainContext,
    simple: &mut SimpleModules,
    updated_content: DashMap<PathBuf, String>,
) -> Result<CompileResult, (CompileResult, Vec<Diagnostic<usize>>)> {
    let entry_file = sess.main_file().unwrap();
    let path = get_path(entry_file).unwrap();

    if let Some(saved) = updated_content
        .iter()
        .find(|x| paths_equal(x.key(), path.clone()))
    {
        update_file(entry_file, path, saved.value().clone())
    }

    let mut parser = Parser::new(entry_file, sess.config.experimental.clone());
    let barrel = parser.parse_barrel();
    for diag in &parser.dcx.diags {
        comp.emit_diag(diag.clone());
    }

    let resolver_temp = &mut TemporaryDiagnosticContext::new();

    let mut discover = ModuleDiscover::new(resolver_temp, sess);

    discover
        .map
        .insert_or_update(get_path(entry_file).unwrap(), barrel.clone());
    let mut visited = HashSet::new();

    let module_map = discover
        .create_module_map(&barrel, &mut visited, &updated_content)
        .unwrap();
    let mut resolver = ImportResolver::new(resolver_temp, sess, main_ctx.clone(), module_map);
    let map = resolver.resolve().unwrap();

    comp.extend_temp(resolver_temp.clone());

    if comp.should_bail() {
        return Err((
            CompileResult::new(map, HirModuleMap::new()),
            comp.emitted.clone(),
        ));
    }

    let hir = comp.analyze(map.clone(), main_ctx, simple).unwrap();

    if hir.modules().is_empty() && comp.should_bail() {
        return Err((
            CompileResult::new(map, HirModuleMap::new()),
            comp.emitted.clone(),
        ));
    }

    if sess.config.is_bin() {
        let main_mod = hir.get_module(ModuleId::from_usize(entry_file)).unwrap();
        let main_fn = main_ctx.resolve_symbol(&"main".to_string(), entry_file);
        if let Some(item) = main_fn {
            if let HirItemKind::Fn(func) = &item.kind {
                comp.validate_main_function(func, entry_file);
            } else {
                comp.emit(MainFunctionNotFunction {
                    span: (item.ident.span, entry_file),
                });
            }
        } else {
            comp.emit(NoMainFunction {
                file: main_mod.path.display().to_string(),
            });
        }
    }

    if comp.should_bail() {
        return Err((CompileResult::new(map, hir), comp.emitted.clone()));
    }

    Ok(CompileResult::new(map, hir))
}

pub fn url_to_path(url: &str) -> PathBuf {
    let path = url.replace("file://", "");

    percent_decode(path.as_bytes())
        .decode_utf8_lossy()
        .to_string()
        .strip_prefix('/')
        .expect("Failed to get path")
        .into()
}

pub fn path_to_uri(path: &PathBuf) -> Url {
    let path = path.to_str().unwrap();
    let path = format!("file://{}", path);
    Url::parse(&path).expect(&format!("Failed to parse {} path", path))
}

pub fn map_severity(style: LabelStyle) -> Option<DiagnosticSeverity> {
    match style {
        LabelStyle::Error => Some(DiagnosticSeverity::ERROR),
        LabelStyle::Warning => Some(DiagnosticSeverity::WARNING),
        LabelStyle::Primary => Some(DiagnosticSeverity::INFORMATION),
        LabelStyle::Note => Some(DiagnosticSeverity::HINT),
        LabelStyle::Add(_) => None,
    }
}
