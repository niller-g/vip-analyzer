//! The context or environment in which the language server functions. In our
//! server implementation this is know as the `WorldState`.
//!
//! Each tick provides an immutable snapshot of the state as `WorldSnapshot`.

use std::{borrow::Cow, time::Instant};

use crossbeam_channel::{Receiver, Sender, unbounded};
use ide::{Analysis, AnalysisHost, Cancellable};
use lsp_types::Url;
use parking_lot::{
    MappedRwLockReadGuard, RwLock, RwLockReadGuard, RwLockUpgradableReadGuard, RwLockWriteGuard,
};
use project_model::Project;
use rustc_hash::FxHashMap;
use tracing::{Level, span, trace};
use triomphe::Arc;
use vfs::{AbsPathBuf, AnchoredPathBuf, FileId, Vfs, VfsPath};

use crate::{
    config::{Config, ConfigErrors},
    diagnostics::{CheckFixes, DiagnosticCollection},
    flycheck::{FlycheckHandle, FlycheckMessage},
    line_index::{LineEndings, LineIndex},
    lsp::{
        from_proto::{self},
        to_proto::url_from_abs_path,
    },
    lsp_ext,
    main_loop::Task,
    mem_docs::MemDocs,
    op_queue::{Cause, OpQueue},
    reload,
    task_pool::TaskPool,
};

pub(crate) struct FetchWorkspaceRequest {
    pub(crate) path: Option<AbsPathBuf>,
}

pub(crate) struct FetchWorkspaceResponse {
    pub(crate) workspaces: Vec<anyhow::Result<Project>>,
}

// Enforces drop order
pub(crate) struct Handle<H, C> {
    pub(crate) handle: H,
    pub(crate) receiver: C,
}

pub(crate) type ReqHandler = fn(&mut GlobalState, lsp_server::Response);
type ReqQueue = lsp_server::ReqQueue<(String, Instant), ReqHandler>;

/// `GlobalState` is the primary mutable state of the language server
///
/// The most interesting components are `vfs`, which stores a consistent
/// snapshot of the file systems, and `analysis_host`, which stores our
/// incremental salsa database.
///
/// Note that this struct has more than one impl in various modules!
#[doc(alias = "GlobalMess")]
pub(crate) struct GlobalState {
    sender: Sender<lsp_server::Message>,
    req_queue: ReqQueue,

    pub(crate) task_pool: Handle<TaskPool<Task>, Receiver<Task>>,
    pub(crate) fmt_pool: Handle<TaskPool<Task>, Receiver<Task>>,

    pub(crate) config: Arc<Config>,
    pub(crate) config_errors: Option<ConfigErrors>,
    pub(crate) analysis_host: AnalysisHost,
    pub(crate) diagnostics: DiagnosticCollection,
    pub(crate) mem_docs: MemDocs,

    // status
    pub(crate) shutdown_requested: bool,
    pub(crate) last_reported_status: lsp_ext::ServerStatusParams,

    // Flycheck
    pub(crate) flycheck: Arc<[FlycheckHandle]>,
    pub(crate) flycheck_sender: Sender<FlycheckMessage>,
    pub(crate) flycheck_receiver: Receiver<FlycheckMessage>,
    pub(crate) last_flycheck_error: Option<String>,

    // VFS
    pub(crate) loader: Handle<Box<dyn vfs::loader::Handle>, Receiver<vfs::loader::Message>>,
    pub(crate) vfs: Arc<RwLock<(vfs::Vfs, FxHashMap<FileId, LineEndings>)>>,
    pub(crate) vfs_config_version: u32,
    pub(crate) vfs_progress_config_version: u32,
    pub(crate) vfs_done: bool,
    // used to track how long VFS loading takes. this can't be on `vfs::loader::Handle`,
    // as that handle's lifetime is the same as `GlobalState` itself.
    pub(crate) vfs_span: Option<tracing::span::EnteredSpan>,
    pub(crate) wants_to_switch: Option<Cause>,

    /// `workspaces` field stores the data we actually use, while the `OpQueue`
    /// stores the result of the last fetch.
    ///
    /// If the fetch (partially) fails, we do not update the current value.
    ///
    /// The handling of build data is subtle. We fetch workspace in two phases:
    ///
    /// *First*, we run `cargo metadata`, which gives us fast results for
    /// initial analysis.
    ///
    /// *Second*, we run `cargo check` which runs build scripts and compiles
    /// proc macros.
    ///
    /// We need both for the precise analysis, but we want rust-analyzer to be
    /// at least partially available just after the first phase. That's because
    /// first phase is much faster, and is much less likely to fail.
    ///
    /// This creates a complication -- by the time the second phase completes,
    /// the results of the first phase could be invalid. That is, while we run
    /// `cargo check`, the user edits `Cargo.toml`, we notice this, and the new
    /// `cargo metadata` completes before `cargo check`.
    ///
    /// An additional complication is that we want to avoid needless work. When
    /// the user just adds comments or whitespace to Cargo.toml, we do not want
    /// to invalidate any salsa caches.
    pub(crate) workspaces: Arc<Vec<Project>>,
    // pub(crate) detached_files: FxHashSet<ManifestPath>,

    // op queues
    pub(crate) fetch_workspaces_queue: OpQueue<FetchWorkspaceRequest, FetchWorkspaceResponse>,
    pub(crate) prime_caches_queue: OpQueue,
    // pub(crate) discover_workspace_queue: OpQueue,
}

/// An immutable snapshot of the world's state at a point in time.
pub(crate) struct GlobalStateSnapshot {
    pub(crate) config: Arc<Config>,
    pub(crate) analysis: Analysis,
    #[expect(dead_code)]
    pub(crate) check_fixes: CheckFixes,
    mem_docs: MemDocs,
    // pub(crate) semantic_tokens_cache: Arc<Mutex<FxHashMap<Url, SemanticTokens>>>,
    vfs: Arc<RwLock<(vfs::Vfs, FxHashMap<FileId, LineEndings>)>>,
    pub(crate) workspaces: Arc<Vec<Project>>,
    pub(crate) flycheck: Arc<[FlycheckHandle]>,
}

impl std::panic::UnwindSafe for GlobalStateSnapshot {}

impl GlobalState {
    pub(crate) fn new(sender: Sender<lsp_server::Message>, config: Config) -> GlobalState {
        let loader = {
            let (sender, receiver) = unbounded::<vfs::loader::Message>();
            let handle: vfs_notify::NotifyHandle = vfs::loader::Handle::spawn(sender);
            let handle = Box::new(handle) as Box<dyn vfs::loader::Handle>;
            Handle { handle, receiver }
        };

        let task_pool = {
            let (sender, receiver) = unbounded();
            let handle = TaskPool::new_with_threads(sender, config.main_loop_num_threads());
            Handle { handle, receiver }
        };
        let fmt_pool = {
            let (sender, receiver) = unbounded();
            let handle = TaskPool::new_with_threads(sender, 1);
            Handle { handle, receiver }
        };

        let analysis_host = AnalysisHost::new(None);
        let (flycheck_sender, flycheck_receiver) = unbounded();

        let mut this = GlobalState {
            sender,
            req_queue: ReqQueue::default(),
            task_pool,
            fmt_pool,
            loader,
            config: Arc::new(config.clone()),
            analysis_host,
            diagnostics: Default::default(),
            mem_docs: MemDocs::default(),
            shutdown_requested: false,
            last_reported_status: lsp_ext::ServerStatusParams {
                health: lsp_ext::Health::Ok,
                quiescent: true,
                message: None,
            },
            config_errors: Default::default(),

            flycheck: Arc::from_iter([]),
            flycheck_sender,
            flycheck_receiver,
            last_flycheck_error: None,
            vfs: Arc::new(RwLock::new((vfs::Vfs::default(), Default::default()))),
            vfs_config_version: 0,
            vfs_progress_config_version: 0,
            vfs_span: None,
            vfs_done: true,
            wants_to_switch: None,

            workspaces: Arc::from(Vec::new()),
            // detached_files: FxHashSet::default(),
            fetch_workspaces_queue: OpQueue::default(),
            prime_caches_queue: OpQueue::default(),
        };
        // Apply any required database inputs from the config.
        this.update_configuration(config);
        this
    }

    pub(crate) fn process_changes(&mut self) -> bool {
        let _p = span!(Level::INFO, "GlobalState::process_changes").entered();

        let (change, workspace_structure_change) = {
            let mut change = ide::FileChange::default();
            let mut guard = self.vfs.write();
            let changed_files = guard.0.take_changes();
            if changed_files.is_empty() {
                return false;
            }

            // downgrade to read lock to allow more readers while we are normalizing text
            let guard = RwLockWriteGuard::downgrade_to_upgradable(guard);
            let vfs: &Vfs = &guard.0;

            let mut workspace_structure_change = None;
            let mut bytes = vec![];
            for file in changed_files.into_values() {
                let vfs_path = vfs.file_path(file.file_id);

                if let Some(path) = vfs_path.as_path() {
                    let path = path.to_path_buf();
                    if file.is_created_or_deleted() {
                        if workspace_structure_change.is_none() {
                            workspace_structure_change = Some(path)
                        }
                    } else if reload::should_refresh_for_change(&path, file.kind(), &[]) {
                        trace!(?path, kind = ?file.kind(), "refreshing for a change");
                        if workspace_structure_change.is_none() {
                            workspace_structure_change = Some(path)
                        }
                    }
                }

                // Clear native diagnostics when their file gets deleted
                if !file.exists() {
                    self.diagnostics.clear_native_for(file.file_id);
                }

                let text =
                    if let vfs::Change::Create(v, _) | vfs::Change::Modify(v, _) = file.change {
                        stdx::to_utf8_else_utf16(&v, true).map(Cow::into_owned).ok().map(|text| {
                            // FIXME: Consider doing normalization in the `vfs` instead? That allows
                            // getting rid of some locking
                            let (text, line_endings) = LineEndings::normalize(text);
                            (text, line_endings)
                        })
                    } else {
                        None
                    };
                // delay `line_endings_map` changes until we are done normalizing the text
                // this allows delaying the re-acquisition of the write lock
                bytes.push((file.file_id, text));
            }
            let (vfs, line_endings_map) = &mut *RwLockUpgradableReadGuard::upgrade(guard);
            bytes.into_iter().for_each(|(file_id, text)| {
                let text = match text {
                    None => None,
                    Some((text, line_endings)) => {
                        line_endings_map.insert(file_id, line_endings);
                        Some(text)
                    }
                };
                change.change_file(file_id, vfs.file_path(file_id).clone(), text);
            });
            (change, workspace_structure_change)
        };

        let _p = span!(Level::INFO, "GlobalState::process_changes/apply_change").entered();
        self.analysis_host.apply_change(change);

        // FIXME: `workspace_structure_change` is computed from `should_refresh_for_change` which is
        // path syntax based. That is not sufficient for all cases so we should lift that check out
        // into a `QueuedTask`, see `handle_did_save_text_document`.
        // Or maybe instead of replacing that check, kick off a semantic one if the syntactic one
        // didn't find anything (to make up for the lack of precision).
        {
            // FIXME: ideally we should only trigger a workspace fetch for non-library changes
            // but something's going wrong with the source root business when we add a new local
            // crate see https://github.com/rust-lang/rust-analyzer/issues/13029
            if let Some(path) = workspace_structure_change {
                let _p = span!(Level::INFO, "GlobalState::process_changes/ws_structure_change")
                    .entered();

                self.fetch_workspaces_queue.request_op(
                    format!("workspace vfs file change: {path}"),
                    FetchWorkspaceRequest { path: Some(path.to_owned()) },
                );
            }
        }

        true
    }

    pub(crate) fn snapshot(&self) -> GlobalStateSnapshot {
        GlobalStateSnapshot {
            config: Arc::clone(&self.config),
            workspaces: Arc::clone(&self.workspaces),
            analysis: self.analysis_host.analysis(),
            vfs: Arc::clone(&self.vfs),
            check_fixes: Arc::clone(&self.diagnostics.check_fixes),
            mem_docs: self.mem_docs.clone(),
            // semantic_tokens_cache: Arc::clone(&self.semantic_tokens_cache),
            flycheck: self.flycheck.clone(),
        }
    }

    pub(crate) fn send_request<R: lsp_types::request::Request>(
        &mut self,
        params: R::Params,
        handler: ReqHandler,
    ) {
        let request = self.req_queue.outgoing.register(R::METHOD.to_owned(), params, handler);
        self.send(request.into());
    }

    pub(crate) fn complete_request(&mut self, response: lsp_server::Response) {
        let handler = self
            .req_queue
            .outgoing
            .complete(response.id.clone())
            .expect("received response for unknown request");
        handler(self, response)
    }

    pub(crate) fn send_notification<N: lsp_types::notification::Notification>(
        &self,
        params: N::Params,
    ) {
        let not = lsp_server::Notification::new(N::METHOD.to_owned(), params);
        self.send(not.into());
    }

    pub(crate) fn register_request(
        &mut self,
        request: &lsp_server::Request,
        request_received: Instant,
    ) {
        self.req_queue
            .incoming
            .register(request.id.clone(), (request.method.clone(), request_received));
    }

    pub(crate) fn respond(&mut self, response: lsp_server::Response) {
        if let Some((method, start)) = self.req_queue.incoming.complete(&response.id) {
            if let Some(err) = &response.error {
                if err.message.starts_with("server panicked") {
                    self.poke_vip_analyzer_developer(format!("{}, check the log", err.message));
                }
            }

            let duration = start.elapsed();
            tracing::debug!(name: "message response", method, %response.id, duration = format_args!("{:0.2?}", duration));
            self.send(response.into());
        }
    }

    pub(crate) fn cancel(&mut self, request_id: lsp_server::RequestId) {
        if let Some(response) = self.req_queue.incoming.cancel(request_id) {
            self.send(response.into());
        }
    }

    pub(crate) fn is_completed(&self, request: &lsp_server::Request) -> bool {
        self.req_queue.incoming.is_completed(&request.id)
    }

    fn send(&self, message: lsp_server::Message) {
        self.sender.send(message).unwrap()
    }

    pub(crate) fn publish_diagnostics(
        &mut self,
        uri: Url,
        version: Option<i32>,
        mut diagnostics: Vec<lsp_types::Diagnostic>,
    ) {
        // We put this on a separate thread to avoid blocking the main thread with serialization work
        self.task_pool.handle.spawn_with_sender(stdx::thread::ThreadIntent::Worker, {
            let sender = self.sender.clone();
            move |_| {
                // VSCode assumes diagnostic messages to be non-empty strings, so we need to patch
                // empty diagnostics. Neither the docs of VSCode nor the LSP spec say whether
                // diagnostic messages are actually allowed to be empty or not and patching this
                // in the VSCode client does not work as the assertion happens in the protocol
                // conversion. So this hack is here to stay, and will be considered a hack
                // until the LSP decides to state that empty messages are allowed.

                // See https://github.com/rust-lang/rust-analyzer/issues/11404
                // See https://github.com/rust-lang/rust-analyzer/issues/13130
                let patch_empty = |message: &mut String| {
                    if message.is_empty() {
                        " ".clone_into(message);
                    }
                };

                for d in &mut diagnostics {
                    patch_empty(&mut d.message);
                    if let Some(dri) = &mut d.related_information {
                        for dri in dri {
                            patch_empty(&mut dri.message);
                        }
                    }
                }

                let not = lsp_server::Notification::new(
                    <lsp_types::notification::PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_owned(),
                    lsp_types::PublishDiagnosticsParams { uri, diagnostics, version },
                );
                _ = sender.send(not.into());
            }
        });
    }
}

impl Drop for GlobalState {
    fn drop(&mut self) {
        self.analysis_host.request_cancellation();
    }
}

impl GlobalStateSnapshot {
    fn vfs_read(&self) -> MappedRwLockReadGuard<'_, vfs::Vfs> {
        RwLockReadGuard::map(self.vfs.read(), |(it, _)| it)
    }

    pub(crate) fn url_to_file_id(&self, url: &Url) -> anyhow::Result<FileId> {
        url_to_file_id(&self.vfs_read(), url)
    }

    pub(crate) fn file_id_to_url(&self, id: FileId) -> Url {
        file_id_to_url(&self.vfs_read(), id)
    }

    #[expect(unused)]
    pub(crate) fn vfs_path_to_file_id(&self, vfs_path: &VfsPath) -> anyhow::Result<FileId> {
        vfs_path_to_file_id(&self.vfs_read(), vfs_path)
    }

    pub(crate) fn file_line_index(&self, file_id: FileId) -> Cancellable<LineIndex> {
        let endings = self.vfs.read().1[&file_id];
        let index = self.analysis.file_line_index(file_id)?;
        let res = LineIndex { index, endings, encoding: self.config.caps().negotiated_encoding() };
        Ok(res)
    }

    #[expect(unused)]
    pub(crate) fn file_version(&self, file_id: FileId) -> Option<i32> {
        Some(self.mem_docs.get(self.vfs_read().file_path(file_id))?.version)
    }

    pub(crate) fn url_file_version(&self, url: &Url) -> Option<i32> {
        let path = from_proto::vfs_path(url).ok()?;
        Some(self.mem_docs.get(&path)?.version)
    }

    #[expect(unused)]
    pub(crate) fn anchored_path(&self, path: &AnchoredPathBuf) -> Url {
        let mut base = self.vfs_read().file_path(path.anchor).clone();
        base.pop();
        let path = base.join(&path.path).unwrap();
        let path = path.as_path().unwrap();
        url_from_abs_path(path)
    }

    #[expect(unused)]
    pub(crate) fn file_id_to_file_path(&self, file_id: FileId) -> vfs::VfsPath {
        self.vfs_read().file_path(file_id).clone()
    }

    #[expect(unused)]
    pub(crate) fn file_exists(&self, file_id: FileId) -> bool {
        self.vfs.read().0.exists(file_id)
    }
}

pub(crate) fn file_id_to_url(vfs: &vfs::Vfs, id: FileId) -> Url {
    let path = vfs.file_path(id);
    let path = path.as_path().unwrap();
    url_from_abs_path(path)
}

pub(crate) fn url_to_file_id(vfs: &vfs::Vfs, url: &Url) -> anyhow::Result<FileId> {
    let path = from_proto::vfs_path(url)?;
    let res = vfs.file_id(&path).ok_or_else(|| anyhow::format_err!("file not found: {path}"))?;
    Ok(res)
}

pub(crate) fn vfs_path_to_file_id(vfs: &vfs::Vfs, vfs_path: &VfsPath) -> anyhow::Result<FileId> {
    let res =
        vfs.file_id(vfs_path).ok_or_else(|| anyhow::format_err!("file not found: {vfs_path}"))?;
    Ok(res)
}
