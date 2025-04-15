use crate::Backend;
use anyhow::Result;
use brim::args::RunArgs;
use brim::compiler::CompilerContext;
use brim::config::toml::Config;
use brim::diagnostic::Diagnostic;
use brim::discover::ModuleDiscover;
use brim::errors::NoMainFunction;
use brim::files::get_path;
use brim::graph::ProjectResolver;
use brim::lints::Lints;
use brim::parser::Parser;
use brim::resolver::ImportResolver;
use brim::session::Session;
use brim::temp_diag::TemporaryDiagnosticContext;
use brim::transformer::HirModuleMap;
use brim::{CompiledModule, CompiledModules, ModuleId, SimpleModules};
use percent_encoding::percent_decode;
use std::collections::HashSet;
use std::path::PathBuf;
use tower_lsp::lsp_types::{MessageType, TextDocumentItem};
use tracing::info;

impl Backend {
    pub async fn on_change(&self, doc: TextDocumentItem, path: PathBuf) {
        info!("Performing on_change for {:?}", path);
    }

    pub async fn scan(&self) -> anyhow::Result<()> {
        let start = std::time::Instant::now();
        let config = Config::get(&std::env::current_dir()?, None)?;
        let args = RunArgs::default();
        let lints = Box::new(Lints::configure(&config.lints));
        let lints = Box::leak(lints);

        let mut resolver = ProjectResolver::new(".");
        let order = resolver.resolve_project()?;
        let configs = resolver.get_configs(&order);

        let mut compiled_projects = CompiledModules::new();
        let mut simple = SimpleModules {
            items: Default::default(),
        };

        for config in configs {
            let sess = &mut Session::new(config.cwd.clone(), config.clone(), args.color_choice);
            let ctx = &mut CompilerContext::new(args.clone(), lints);

            let hir = match compile_project(sess, ctx, &mut compiled_projects, &mut simple) {
                Ok(hir) => hir,
                Err(diags) => {
                    for diag in diags {
                        self.client
                            .show_message(
                                MessageType::WARNING,
                                format!("Diagnostic: {}", diag.message),
                            )
                            .await;
                    }
                    continue;
                }
            };

            compiled_projects.map.insert(
                config.project.name.clone(),
                CompiledModule {
                    config,
                    hir: hir.clone(),
                },
            );

            compiled_projects
                .expanded_by_builtins
                .extend(hir.expanded_by_builtins);
            compiled_projects.builtin_args.extend(hir.builtin_args);
        }

        let projects = compiled_projects
            .map
            .keys()
            .cloned()
            .collect::<Vec<_>>()
            .join(", ");

        *self.compiled.lock().await = compiled_projects;

        info!("Finished scan with {} in {:.2?}", projects, start.elapsed());

        Ok(())
    }
}

pub fn compile_project(
    sess: &mut Session,
    comp: &mut CompilerContext,
    compiled: &mut CompiledModules,
    simple: &mut SimpleModules,
) -> Result<HirModuleMap, Vec<Diagnostic<usize>>> {
    let entry_file = sess.main_file().unwrap();
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

    let module_map = discover.create_module_map(&barrel, &mut visited).unwrap();
    let mut resolver = ImportResolver::new(resolver_temp, sess, compiled.clone(), module_map);
    let map = resolver.resolve().unwrap();

    comp.extend_temp(resolver_temp.clone());

    if comp.should_bail() {
        return Err(comp.emitted.clone());
    }

    let hir = comp.analyze(map, compiled, simple).unwrap();

    if hir.modules.is_empty() && comp.should_bail() {
        return Err(comp.emitted.clone());
    }

    if sess.config.is_bin() {
        let main_mod = hir.get_module(ModuleId::from_usize(entry_file)).unwrap();
        let main_fn = hir.get_fn(ModuleId::from_usize(entry_file), "main");

        if let Some(func) = main_fn {
            comp.validate_main_function(func, entry_file);
        } else {
            comp.emit(NoMainFunction {
                file: main_mod.path.display().to_string(),
            });
        }
    }

    if comp.should_bail() {
        return Err(comp.emitted.clone());
    }

    Ok(hir)
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
