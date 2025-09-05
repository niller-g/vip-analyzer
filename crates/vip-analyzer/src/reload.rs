//! Project loading & configuration updates.
//!
//! This is quite tricky. The main problem is time and changes -- there's no
//! fixed "project" rust-analyzer is working with, "current project" is itself
//! mutable state. For example, when the user edits `Cargo.toml` by adding a new
//! dependency, project model changes. What's more, switching project model is
//! not instantaneous -- it takes time to run `cargo metadata` and (for proc
//! macros) `cargo check`.
//!
//! The main guiding principle here is, as elsewhere in rust-analyzer,
//! robustness. We try not to assume that the project model exists or is
//! correct. Instead, we try to provide a best-effort service. Even if the
//! project is currently loading and we don't have a full project model, we
//! still want to respond to various  requests.
// FIXME: This is a mess that needs some untangling work
use std::{iter, mem};

use ide::FileChange;
use ide_db::base_db::ProjectGraphBuilder;
use itertools::Itertools;
use lsp_types::FileSystemWatcher;
use project_model::{Project, WorkspaceFolders};
use stdx::{format_to, thread::ThreadIntent};
use triomphe::Arc;
use vfs::{AbsPath, AbsPathBuf, ChangeKind};

use crate::{
    config::Config,
    flycheck::{FlycheckConfig, FlycheckHandle},
    global_state::{FetchWorkspaceRequest, FetchWorkspaceResponse, GlobalState},
    lsp_ext,
    main_loop::Task,
    op_queue::Cause,
};
use tracing::info;

#[derive(Debug)]
pub(crate) enum ProjectWorkspaceProgress {
    Begin,
    Report(String),
    End(Vec<anyhow::Result<Project>>),
}

impl GlobalState {
    /// Is the server quiescent?
    ///
    /// This indicates that we've fully loaded the projects and
    /// are ready to do semantic work.
    pub(crate) fn is_quiescent(&self) -> bool {
        self.vfs_done
            && !self.fetch_workspaces_queue.op_in_progress()
            && self.vfs_progress_config_version >= self.vfs_config_version
    }

    /// Is the server ready to respond to analysis dependent LSP requests?
    ///
    /// Unlike `is_quiescent`, this returns false when we're indexing
    /// the project, because we're holding the salsa lock and cannot
    /// respond to LSP requests that depend on salsa data.
    fn is_fully_ready(&self) -> bool {
        self.is_quiescent() && !self.prime_caches_queue.op_in_progress()
    }

    pub(crate) fn update_configuration(&mut self, config: Config) {
        let _p = tracing::info_span!("GlobalState::update_configuration").entered();
        let old_config = mem::replace(&mut self.config, Arc::new(config));
        // if self.config.lru_parse_query_capacity() != old_config.lru_parse_query_capacity() {
        //     self.analysis_host.update_lru_capacity(self.config.lru_parse_query_capacity());
        // }
        // if self.config.lru_query_capacities_config() != old_config.lru_query_capacities_config() {
        //     self.analysis_host.update_lru_capacities(
        //         &self.config.lru_query_capacities_config().cloned().unwrap_or_default(),
        //     );
        // }

        if self.config.discovered_projects != old_config.discovered_projects {
            let req = FetchWorkspaceRequest { path: None };
            self.fetch_workspaces_queue.request_op("discovered projects changed".to_owned(), req)
        } else if self.config.flycheck() != old_config.flycheck() {
            self.reload_flycheck();
        }
    }

    pub(crate) fn current_status(&self) -> lsp_ext::ServerStatusParams {
        let mut status = lsp_ext::ServerStatusParams {
            health: lsp_ext::Health::Ok,
            quiescent: self.is_fully_ready(),
            message: None,
        };
        let mut message = String::new();

        if let Some(err) = &self.config_errors {
            status.health |= lsp_ext::Health::Warning;
            format_to!(message, "{err}\n");
        }
        if let Some(err) = &self.last_flycheck_error {
            status.health |= lsp_ext::Health::Warning;
            message.push_str(err);
            message.push('\n');
        }

        // if self.config.linked_or_discovered_projects().is_empty()
        //     && self.config.detached_files().is_empty()
        // {
        //     status.health |= lsp_ext::Health::Warning;
        //     message.push_str("Failed to discover workspace.\n");
        //     message.push_str("Consider adding the `Cargo.toml` of the workspace to the [`linkedProjects`](https://rust-analyzer.github.io/manual.html#rust-analyzer.linkedProjects) setting.\n\n");
        // }
        if self.fetch_workspace_error().is_err() {
            status.health |= lsp_ext::Health::Error;
            message.push_str("Failed to load workspaces.");
            // if self.config.has_linked_projects() {
            //     message.push_str(
            //             "`rust-analyzer.linkedProjects` have been specified, which may be incorrect. Specified project paths:\n",
            //         );
            //     message
            //         .push_str(&format!("    {}", self.config.linked_manifests().format("\n    ")));
            //     if self.config.has_linked_project_jsons() {
            //         message.push_str("\nAdditionally, one or more project jsons are specified")
            //     }
            // }
            message.push_str("\n\n");
        }

        if !message.is_empty() {
            status.message = Some(message.trim_end().to_owned());
        }

        status
    }

    pub(crate) fn fetch_workspaces(&mut self, cause: Cause, _path: Option<AbsPathBuf>) {
        info!(%cause, "will fetch workspaces");

        self.task_pool.handle.spawn_with_sender(ThreadIntent::Worker, {
            let discovered_projects = self.config.discovered_projects.clone();
            let ide_vars = self.config.workspace_ide_variables().clone();

            move |sender| {
                let progress = {
                    let sender = sender.clone();
                    move |msg| {
                        sender
                            .send(Task::FetchWorkspace(ProjectWorkspaceProgress::Report(msg)))
                            .unwrap()
                    }
                };

                sender.send(Task::FetchWorkspace(ProjectWorkspaceProgress::Begin)).unwrap();

                let mut workspaces = discovered_projects
                    .iter()
                    .map(|project| {
                        project_model::Project::load(
                            project.clone(),
                            ide_vars.manifest_vars(project).cloned().unwrap_or_default(),
                            &progress,
                        )
                    })
                    .collect::<Vec<_>>();

                let mut i = 0;
                while i < workspaces.len() {
                    if let Ok(w) = &workspaces[i] {
                        let dupes: Vec<_> = workspaces[i + 1..]
                            .iter()
                            .positions(|it| {
                                it.as_ref().is_ok_and(|ws| ws.manifest_path() == w.manifest_path())
                            })
                            .collect();
                        dupes.into_iter().rev().for_each(|d| {
                            _ = workspaces.remove(d + i + 1);
                        });
                    }
                    i += 1;
                }

                info!(?workspaces, "did fetch workspaces");
                sender
                    .send(Task::FetchWorkspace(ProjectWorkspaceProgress::End(workspaces)))
                    .unwrap();
            }
        });
    }

    pub(crate) fn switch_workspaces(&mut self, cause: Cause) {
        let _p = tracing::info_span!("GlobalState::switch_workspaces").entered();
        info!(%cause, "will switch workspaces");

        let Some(FetchWorkspaceResponse { workspaces }) =
            self.fetch_workspaces_queue.last_op_result()
        else {
            return;
        };

        let switching_from_empty_workspace = self.workspaces.is_empty();

        info!(%cause, %switching_from_empty_workspace);
        if self.fetch_workspace_error().is_err() && !switching_from_empty_workspace {
            self.recreate_project_graph(cause);
            // It only makes sense to switch to a partially broken workspace
            // if we don't have any workspace at all yet.
            return;
        }

        info!(%cause);
        let workspaces =
            workspaces.iter().filter_map(|res| res.as_ref().ok().cloned()).collect::<Vec<_>>();

        let same_workspaces = workspaces.len() == self.workspaces.len()
            && workspaces.iter().zip(self.workspaces.iter()).all(|(l, r)| l == r);

        if same_workspaces {
            if switching_from_empty_workspace {
                // Switching from empty to empty is a no-op
                return;
            }
        } else {
            info!("abandon workspaces");
            self.workspaces = Arc::new(workspaces);
        }

        let filter = WorkspaceFolders::all_directories(&self.workspaces);

        let mut watchers: Vec<FileSystemWatcher> = if self
            .config
            .did_change_watched_files_relative_pattern_support()
        {
            // When relative patterns are supported by the client, prefer using them
            filter
                .flat_map(|base| [(base.clone(), "**/*.{pack,ph,pro,cl,i}"), (base, "**/*.vipprj")])
                .map(|(base, pat)| lsp_types::FileSystemWatcher {
                    glob_pattern: lsp_types::GlobPattern::Relative(lsp_types::RelativePattern {
                        base_uri: lsp_types::OneOf::Right(
                            lsp_types::Url::from_file_path(base).unwrap(),
                        ),
                        pattern: pat.to_owned(),
                    }),
                    kind: None,
                })
                .collect()
        } else {
            // When they're not, integrate the base to make them into absolute patterns
            filter
                .flat_map(|base| {
                    [format!("{base}/**/*.{{pack,ph,pro,cl,i}}"), format!("{base}/**/*.vipprj")]
                })
                .map(|glob_pattern| lsp_types::FileSystemWatcher {
                    glob_pattern: lsp_types::GlobPattern::String(glob_pattern),
                    kind: None,
                })
                .collect()
        };

        watchers.extend(
            iter::once(self.workspaces.iter().map(|ws| ws.manifest_path().as_ref())).flatten().map(
                |glob_pattern: &AbsPath| lsp_types::FileSystemWatcher {
                    glob_pattern: lsp_types::GlobPattern::String(glob_pattern.to_string()),
                    kind: None,
                },
            ),
        );

        let registration_options = lsp_types::DidChangeWatchedFilesRegistrationOptions { watchers };
        let registration = lsp_types::Registration {
            id: "workspace/didChangeWatchedFiles".to_owned(),
            method: "workspace/didChangeWatchedFiles".to_owned(),
            register_options: Some(serde_json::to_value(registration_options).unwrap()),
        };
        self.send_request::<lsp_types::request::RegisterCapability>(
            lsp_types::RegistrationParams { registrations: vec![registration] },
            |_, _| (),
        );

        let project_folders = WorkspaceFolders::new(&self.workspaces);

        self.vfs_config_version += 1;
        self.loader.handle.set_config(vfs::loader::Config {
            load: project_folders.load,
            watch: vec![], // FIXME: nothing here since we only support that the client does the watching
            version: self.vfs_config_version,
        });

        info!(?cause, "recreating the project graph");
        self.recreate_project_graph(cause);
        info!("did switch workspaces");
    }

    fn recreate_project_graph(&mut self, cause: Cause) {
        info!(?cause, "Building Project Graph");
        self.report_progress(
            "Building ProjectGraph",
            crate::lsp::utils::Progress::Begin,
            None,
            None,
            None,
        );

        let project_graph = {
            let vfs = &self.vfs.read().0;
            let load = |path: &AbsPath| {
                let vfs_path = vfs::VfsPath::from(path.to_path_buf());
                vfs.file_id(&vfs_path)
            };
            ws_to_project_graph(&self.workspaces, load)
        };

        let mut change = FileChange::default();
        change.set_project_graph(project_graph);
        self.analysis_host.apply_change(change);
        self.finish_loading_project_graph();
        self.report_progress(
            "Building ProjectGraph",
            crate::lsp::utils::Progress::End,
            None,
            None,
            None,
        );
    }

    pub(crate) fn finish_loading_project_graph(&mut self) {
        self.process_changes();
        self.reload_flycheck();
    }

    pub(super) fn fetch_workspace_error(&self) -> Result<(), String> {
        let mut buf = String::new();

        let Some(FetchWorkspaceResponse { workspaces }) =
            self.fetch_workspaces_queue.last_op_result()
        else {
            return Ok(());
        };

        if workspaces.is_empty() {
            stdx::format_to!(buf, "vip-analyzer failed to fetch workspace");
        } else {
            for ws in workspaces {
                if let Err(err) = ws {
                    stdx::format_to!(buf, "vip-analyzer failed to load workspace: {:#}\n", err);
                }
            }
        }

        if buf.is_empty() {
            return Ok(());
        }

        Err(buf)
    }

    fn reload_flycheck(&mut self) {
        let _p = tracing::info_span!("GlobalState::reload_flycheck").entered();
        let config = self.config.flycheck();
        let sender = self.flycheck_sender.clone();
        let invocation_strategy = match config {
            FlycheckConfig::VipBuilderCommand { .. } => {
                crate::flycheck::InvocationStrategy::PerWorkspace
            }
        };

        self.flycheck = match invocation_strategy {
            crate::flycheck::InvocationStrategy::PerWorkspace => self
                .workspaces
                .iter()
                .enumerate()
                .map(|(id, project)| {
                    FlycheckHandle::spawn(
                        id,
                        sender.clone(),
                        config.clone(),
                        project.project_root().to_path_buf(),
                        project.manifest_path().clone(),
                        project.pro_dir().clone(),
                    )
                })
                .collect::<Vec<_>>(),
        }
        .into();
    }
}

fn ws_to_project_graph(
    workspaces: &[Project],
    mut load: impl FnMut(&AbsPath) -> Option<vfs::FileId>,
) -> ProjectGraphBuilder {
    let mut project_graph = ProjectGraphBuilder::default();

    for ws in workspaces {
        let Some((project_data, extra)) = ws.as_project_data(&mut load) else { continue };
        project_graph.add_project_root(project_data, extra);
    }
    project_graph.shrink_to_fit();

    project_graph
}

pub(crate) fn should_refresh_for_change(
    path: &AbsPath,
    change_kind: ChangeKind,
    additional_paths: &[&str],
) -> bool {
    let Some(file_name) = path.file_name() else { return false };

    if path.extension() == Some("vipprj") {
        return true;
    }

    if additional_paths.contains(&file_name) {
        return true;
    }

    if change_kind == ChangeKind::Modify {
        return false;
    }

    false
}

/// Similar to [`str::eq_ignore_ascii_case`] but instead of ignoring
/// case, we say that `-` and `_` are equal.
#[expect(unused)]
fn eq_ignore_underscore(s1: &str, s2: &str) -> bool {
    if s1.len() != s2.len() {
        return false;
    }

    s1.as_bytes().iter().zip(s2.as_bytes()).all(|(c1, c2)| {
        let c1_underscore = c1 == &b'_' || c1 == &b'-';
        let c2_underscore = c2 == &b'_' || c2 == &b'-';

        c1 == c2 || (c1_underscore && c2_underscore)
    })
}
