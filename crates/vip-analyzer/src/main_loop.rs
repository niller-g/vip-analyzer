//! The main loop of `rust-analyzer` responsible for dispatching LSP
//! requests/replies and notifications back to the client.

use std::{
    fmt,
    ops::Div as _,
    panic::AssertUnwindSafe,
    time::{Duration, Instant},
};

use crossbeam_channel::{Receiver, select};
use ide_db::base_db::{VfsPath, salsa::Database as _};
use lsp_server::{Connection, Notification, Request};
use lsp_types::notification::Notification as _;
use stdx::thread::ThreadIntent;
use tracing::{Level, error, span};
use vfs::{FileId, loader::LoadingProgress};

use crate::{
    config::Config,
    diagnostics::{DiagnosticsGeneration, NativeDiagnosticsFetchKind, fetch_native_diagnostics},
    flycheck::{self, FlycheckMessage},
    global_state::{
        FetchWorkspaceRequest, FetchWorkspaceResponse, GlobalState, file_id_to_url, url_to_file_id,
    },
    handlers::dispatch::{NotificationDispatcher, RequestDispatcher},
    lsp::{
        from_proto,
        utils::{Progress, notification_is},
    },
    lsp_ext,
    reload::ProjectWorkspaceProgress,
};

pub fn main_loop(config: Config, connection: Connection) -> anyhow::Result<()> {
    tracing::info!("initial config: {:#?}", config);

    // Windows scheduler implements priority boosts: if thread waits for an
    // event (like a condvar), and event fires, priority of the thread is
    // temporary bumped. This optimization backfires in our case: each time the
    // `main_loop` schedules a task to run on a threadpool, the worker threads
    // gets a higher priority, and (on a machine with fewer cores) displaces the
    // main loop! We work around this by marking the main loop as a
    // higher-priority thread.
    //
    // https://docs.microsoft.com/en-us/windows/win32/procthread/scheduling-priorities
    // https://docs.microsoft.com/en-us/windows/win32/procthread/priority-boosts
    // https://github.com/rust-lang/rust-analyzer/issues/2835
    #[cfg(windows)]
    unsafe {
        use windows_sys::Win32::System::Threading::*;
        let thread = GetCurrentThread();
        let thread_priority_above_normal = 1;
        SetThreadPriority(thread, thread_priority_above_normal);
    }

    GlobalState::new(connection.sender, config).run(connection.receiver)
}

enum Event {
    Lsp(lsp_server::Message),
    Task(Task),
    Vfs(vfs::loader::Message),
    Flycheck(FlycheckMessage),
}

impl fmt::Display for Event {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Event::Lsp(_) => write!(f, "Event::Lsp"),
            Event::Task(_) => write!(f, "Event::Task"),
            Event::Vfs(_) => write!(f, "Event::Vfs"),
            Event::Flycheck(_) => write!(f, "Event::Flycheck"),
        }
    }
}

#[derive(Debug)]
pub(crate) enum DiagnosticsTaskKind {
    Syntax(DiagnosticsGeneration, Vec<(FileId, Vec<lsp_types::Diagnostic>)>),
    Semantic(DiagnosticsGeneration, Vec<(FileId, Vec<lsp_types::Diagnostic>)>),
}

#[derive(Debug)]
pub(crate) enum Task {
    Response(lsp_server::Response),
    Retry(lsp_server::Request),
    Diagnostics(DiagnosticsTaskKind),
    PrimeCaches(PrimeCachesProgress),
    FetchWorkspace(ProjectWorkspaceProgress),
}

#[derive(Debug)]
pub(crate) enum PrimeCachesProgress {
    Begin,
    Report(ide::ParallelPrimeCachesProgress),
    End { cancelled: bool },
}

impl fmt::Debug for Event {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let debug_non_verbose = |not: &Notification, f: &mut fmt::Formatter<'_>| {
            f.debug_struct("Notification").field("method", &not.method).finish()
        };

        match self {
            Event::Lsp(lsp_server::Message::Notification(not)) => {
                if notification_is::<lsp_types::notification::DidOpenTextDocument>(not)
                    || notification_is::<lsp_types::notification::DidChangeTextDocument>(not)
                {
                    return debug_non_verbose(not, f);
                }
            }
            Event::Task(Task::Response(resp)) => {
                return f
                    .debug_struct("Response")
                    .field("id", &resp.id)
                    .field("error", &resp.error)
                    .finish();
            }
            _ => (),
        }
        match self {
            Event::Lsp(it) => fmt::Debug::fmt(it, f),
            Event::Task(it) => fmt::Debug::fmt(it, f),
            // Event::QueuedTask(it) => fmt::Debug::fmt(it, f),
            Event::Vfs(it) => fmt::Debug::fmt(it, f),
            Event::Flycheck(it) => fmt::Debug::fmt(it, f),
            // Event::TestResult(it) => fmt::Debug::fmt(it, f),
        }
    }
}

impl GlobalState {
    fn run(mut self, inbox: Receiver<lsp_server::Message>) -> anyhow::Result<()> {
        self.update_status_or_notify();

        if self.config.did_save_text_document_dynamic_registration() {
            // let additional_patterns = self
            //     .config
            //     .discover_workspace_config()
            //     .map(|cfg| cfg.files_to_watch.clone().into_iter())
            //     .into_iter()
            //     .flatten()
            //     .map(|f| format!("**/{f}"));
            self.register_did_save_capability(std::iter::empty());
        }

        // if self.config.discover_workspace_config().is_none() {
        self.fetch_workspaces_queue
            .request_op("startup".to_owned(), FetchWorkspaceRequest { path: None });
        if let Some((cause, FetchWorkspaceRequest { path })) =
            self.fetch_workspaces_queue.should_start_op()
        {
            self.fetch_workspaces(cause, path);
        }
        // }

        while let Ok(event) = self.next_event(&inbox) {
            let Some(event) = event else {
                anyhow::bail!("client exited without proper shutdown sequence");
            };
            if matches!(
                &event,
                Event::Lsp(lsp_server::Message::Notification(Notification { method, .. }))
                if method == lsp_types::notification::Exit::METHOD
            ) {
                return Ok(());
            }
            self.handle_event(event);
        }

        Err(anyhow::anyhow!("A receiver has been dropped, something panicked!"))
    }

    fn register_did_save_capability(&mut self, additional_patterns: impl Iterator<Item = String>) {
        let additional_filters = additional_patterns.map(|pattern| lsp_types::DocumentFilter {
            language: None,
            scheme: None,
            pattern: (Some(pattern)),
        });

        let mut selectors = vec![
            lsp_types::DocumentFilter {
                language: None,
                scheme: None,
                pattern: Some("**/*.{pack,ph,pro,cl,i}".into()),
            },
            lsp_types::DocumentFilter {
                language: None,
                scheme: None,
                pattern: Some("**/*.vipprj".into()),
            },
        ];
        selectors.extend(additional_filters);

        let save_registration_options = lsp_types::TextDocumentSaveRegistrationOptions {
            include_text: Some(false),
            text_document_registration_options: lsp_types::TextDocumentRegistrationOptions {
                document_selector: Some(selectors),
            },
        };

        let registration = lsp_types::Registration {
            id: "textDocument/didSave".to_owned(),
            method: "textDocument/didSave".to_owned(),
            register_options: Some(serde_json::to_value(save_registration_options).unwrap()),
        };
        self.send_request::<lsp_types::request::RegisterCapability>(
            lsp_types::RegistrationParams { registrations: vec![registration] },
            |_, _| (),
        );
    }

    fn next_event(
        &self,
        inbox: &Receiver<lsp_server::Message>,
    ) -> Result<Option<Event>, crossbeam_channel::RecvError> {
        // Make sure we reply to formatting requests ASAP so the editor doesn't block
        if let Ok(task) = self.fmt_pool.receiver.try_recv() {
            return Ok(Some(Event::Task(task)));
        }

        select! {
            recv(inbox) -> msg =>
                return Ok(msg.ok().map(Event::Lsp)),

            recv(self.task_pool.receiver) -> task =>
                task.map(Event::Task),

            recv(self.fmt_pool.receiver) -> task =>
                task.map(Event::Task),

            recv(self.loader.receiver) -> task =>
                task.map(Event::Vfs),

            recv(self.flycheck_receiver) -> task =>
                task.map(Event::Flycheck),
        }
        .map(Some)
    }

    fn handle_event(&mut self, event: Event) {
        let loop_start = Instant::now();
        // NOTE: don't count blocking select! call as a loop-turn time
        let _p = tracing::span!(Level::INFO, "GlobalState::handle_event", event = %event).entered();

        let event_dbg_msg = format!("{event:?}");
        tracing::debug!(?loop_start, ?event, "handle_event");
        if tracing::enabled!(tracing::Level::INFO) {
            let task_queue_len = self.task_pool.handle.len();
            if task_queue_len > 0 {
                tracing::info!("task queue len: {}", task_queue_len);
            }
        }

        let was_quiescent = self.is_quiescent();
        match event {
            Event::Lsp(msg) => match msg {
                lsp_server::Message::Request(req) => self.on_new_request(loop_start, req),
                lsp_server::Message::Notification(not) => self.on_notification(not),
                lsp_server::Message::Response(resp) => self.complete_request(resp),
            },
            Event::Task(task) => {
                let _p = tracing::info_span!("GlobalState::handle_event/task").entered();
                let mut prime_caches_progress = Vec::new();

                self.handle_task(&mut prime_caches_progress, task);
                // Coalesce multiple task events into one loop turn
                while let Ok(task) = self.task_pool.receiver.try_recv() {
                    self.handle_task(&mut prime_caches_progress, task);
                }

                for progress in prime_caches_progress {
                    let (state, message, fraction, title);
                    match progress {
                        PrimeCachesProgress::Begin => {
                            state = Progress::Begin;
                            message = None;
                            fraction = 0.0;
                            title = "Indexing";
                        }
                        PrimeCachesProgress::Report(report) => {
                            state = Progress::Report;
                            title = report.work_type;

                            message = match &*report.projects_currently_indexing {
                                [crate_name] => Some(format!(
                                    "{}/{} ({crate_name})",
                                    report.projects_done, report.projects_total
                                )),
                                [crate_name, rest @ ..] => Some(format!(
                                    "{}/{} ({} + {} more)",
                                    report.projects_done,
                                    report.projects_total,
                                    crate_name,
                                    rest.len()
                                )),
                                _ => None,
                            };

                            fraction =
                                Progress::fraction(report.projects_done, report.projects_total);
                        }
                        PrimeCachesProgress::End { cancelled } => {
                            state = Progress::End;
                            message = None;
                            fraction = 1.0;
                            title = "Indexing";

                            self.analysis_host.raw_database_mut().trigger_lru_eviction();
                            self.prime_caches_queue.op_completed(());
                            if cancelled {
                                self.prime_caches_queue
                                    .request_op("restart after cancellation".to_owned(), ());
                            }
                        }
                    };

                    self.report_progress(
                        title,
                        state,
                        message,
                        Some(fraction),
                        Some("vipAnalyzer/cachePriming".to_owned()),
                    );
                }
            }
            Event::Vfs(message) => {
                let _p = tracing::info_span!("GlobalState::handle_event/vfs").entered();
                self.handle_vfs_msg(message);
                // Coalesce many VFS event into a single loop turn
                while let Ok(message) = self.loader.receiver.try_recv() {
                    self.handle_vfs_msg(message);
                }
            }
            Event::Flycheck(message) => {
                let _p = tracing::info_span!("GlobalState::handle_event/flycheck").entered();
                self.handle_flycheck_msg(message);
                // Coalesce many flycheck updates into a single loop turn
                while let Ok(message) = self.flycheck_receiver.try_recv() {
                    self.handle_flycheck_msg(message);
                }
            }
        }
        let event_handling_duration = loop_start.elapsed();
        let (state_changed, memdocs_added_or_removed) = if self.vfs_done {
            if let Some(cause) = self.wants_to_switch.take() {
                self.switch_workspaces(cause);
            }
            (self.process_changes(), self.mem_docs.take_changes())
        } else {
            (false, false)
        };

        if self.is_quiescent() {
            let became_quiescent = !was_quiescent;
            if became_quiescent {
                if self.config.check_on_save() {
                    // Project has loaded properly, kick off initial flycheck
                    self.flycheck.iter().for_each(|flycheck| flycheck.restart_workspace());
                }
                if self.config.prefill_caches() {
                    self.prime_caches_queue.request_op("became quiescent".to_owned(), ());
                }
            }

            let client_refresh = became_quiescent || state_changed;
            if client_refresh {
                // Refresh semantic tokens if the client supports it.
                if self.config.semantic_tokens_refresh() {
                    // self.semantic_tokens_cache.lock().clear();
                    self.send_request::<lsp_types::request::SemanticTokensRefresh>((), |_, _| ());
                }

                // Refresh code lens if the client supports it.
                if self.config.code_lens_refresh() {
                    self.send_request::<lsp_types::request::CodeLensRefresh>((), |_, _| ());
                }

                // Refresh inlay hints if the client supports it.
                if self.config.inlay_hints_refresh() {
                    self.send_request::<lsp_types::request::InlayHintRefreshRequest>((), |_, _| ());
                }

                if self.config.diagnostics_refresh() {
                    self.send_request::<lsp_types::request::WorkspaceDiagnosticRefresh>(
                        (),
                        |_, _| (),
                    );
                }
            }

            let project_or_mem_docs_changed =
                became_quiescent || state_changed || memdocs_added_or_removed;
            if project_or_mem_docs_changed && !self.config.text_document_diagnostic() {
                self.update_diagnostics();
            }
        }

        if let Some(diagnostic_changes) = self.diagnostics.take_changes() {
            for file_id in diagnostic_changes {
                let uri = file_id_to_url(&self.vfs.read().0, file_id);
                let version = from_proto::vfs_path(&uri)
                    .ok()
                    .and_then(|path| self.mem_docs.get(&path).map(|it| it.version));

                let diagnostics =
                    self.diagnostics.diagnostics_for(file_id).cloned().collect::<Vec<_>>();
                self.publish_diagnostics(uri, version, diagnostics);
            }
        }

        // if self.config.cargo_autoreload_config()
        //     || self.config.discover_workspace_config().is_some()
        // {
        if let Some((cause, FetchWorkspaceRequest { path })) =
            self.fetch_workspaces_queue.should_start_op()
        {
            self.fetch_workspaces(cause, path);
        }
        // }

        if let Some((cause, ())) = self.prime_caches_queue.should_start_op() {
            self.prime_caches(cause);
        }

        self.update_status_or_notify();

        let loop_duration = loop_start.elapsed();
        if loop_duration > Duration::from_millis(100) && was_quiescent {
            tracing::warn!(
                "overly long loop turn took {loop_duration:?} (event handling took {event_handling_duration:?}): {event_dbg_msg}"
            );
            self.poke_vip_analyzer_developer(format!(
                "overly long loop turn took {loop_duration:?} (event handling took {event_handling_duration:?}): {event_dbg_msg}"
            ));
        }
    }

    fn prime_caches(&mut self, cause: String) {
        tracing::debug!(%cause, "will prime caches");
        let num_worker_threads = self.config.prime_caches_num_threads();

        self.task_pool.handle.spawn_with_sender(ThreadIntent::Worker, {
            let analysis = AssertUnwindSafe(self.snapshot().analysis);
            move |sender| {
                sender.send(Task::PrimeCaches(PrimeCachesProgress::Begin)).unwrap();
                let res = analysis.parallel_prime_caches(num_worker_threads, |progress| {
                    let report = PrimeCachesProgress::Report(progress);
                    sender.send(Task::PrimeCaches(report)).unwrap();
                });
                sender
                    .send(Task::PrimeCaches(PrimeCachesProgress::End { cancelled: res.is_err() }))
                    .unwrap();
            }
        });
    }

    fn update_diagnostics(&mut self) {
        let generation = self.diagnostics.next_generation();
        let subscriptions = {
            let vfs = &self.vfs.read().0;
            self.mem_docs
                .iter()
                .map(|path| vfs.file_id(path).unwrap())
                .collect::<std::sync::Arc<_>>()
        };
        tracing::trace!("updating notifications for {:?}", subscriptions);
        // Split up the work on multiple threads, but we don't wanna fill the entire task pool with
        // diagnostic tasks, so we limit the number of tasks to a quarter of the total thread pool.
        let max_tasks = self.config.main_loop_num_threads().div(4).max(1);
        let chunk_length = subscriptions.len() / max_tasks;
        let remainder = subscriptions.len() % max_tasks;

        let mut start = 0;
        for task_idx in 0..max_tasks {
            let extra = if task_idx < remainder { 1 } else { 0 };
            let end = start + chunk_length + extra;
            let slice = start..end;
            if slice.is_empty() {
                break;
            }
            // Diagnostics are triggered by the user typing
            // so we run them on a latency sensitive thread.
            let snapshot = self.snapshot();
            self.task_pool.handle.spawn_with_sender(ThreadIntent::LatencySensitive, {
                let subscriptions = subscriptions.clone();
                // Do not fetch semantic diagnostics (and populate query results) if we haven't even
                // loaded the initial workspace yet.
                let fetch_semantic =
                    self.vfs_done && self.fetch_workspaces_queue.last_op_result().is_some();
                move |sender| {
                    // We aren't observing the semantics token cache here
                    let snapshot = AssertUnwindSafe(&snapshot);
                    let Ok(diags) = std::panic::catch_unwind(|| {
                        fetch_native_diagnostics(
                            &snapshot,
                            subscriptions.clone(),
                            slice.clone(),
                            NativeDiagnosticsFetchKind::Syntax,
                        )
                    }) else {
                        return;
                    };
                    sender
                        .send(Task::Diagnostics(DiagnosticsTaskKind::Syntax(generation, diags)))
                        .unwrap();

                    if fetch_semantic {
                        let Ok(diags) = std::panic::catch_unwind(|| {
                            fetch_native_diagnostics(
                                &snapshot,
                                subscriptions.clone(),
                                slice.clone(),
                                NativeDiagnosticsFetchKind::Semantic,
                            )
                        }) else {
                            return;
                        };
                        sender
                            .send(Task::Diagnostics(DiagnosticsTaskKind::Semantic(
                                generation, diags,
                            )))
                            .unwrap();
                    }
                }
            });
            start = end;
        }
    }

    fn update_status_or_notify(&mut self) {
        let status = self.current_status();
        if self.last_reported_status != status {
            self.last_reported_status = status.clone();

            if self.config.server_status_notification() {
                self.send_notification::<lsp_ext::ServerStatusNotification>(status);
            } else if let (
                health @ (lsp_ext::Health::Warning | lsp_ext::Health::Error),
                Some(message),
            ) = (status.health, &status.message)
            {
                let open_log_button = tracing::enabled!(tracing::Level::ERROR)
                    && (self.fetch_workspace_error().is_err());
                self.show_message(
                    match health {
                        lsp_ext::Health::Ok => lsp_types::MessageType::INFO,
                        lsp_ext::Health::Warning => lsp_types::MessageType::WARNING,
                        lsp_ext::Health::Error => lsp_types::MessageType::ERROR,
                    },
                    message.clone(),
                    open_log_button,
                );
            }
        }
    }

    fn handle_task(&mut self, prime_caches_progress: &mut Vec<PrimeCachesProgress>, task: Task) {
        match task {
            Task::Response(response) => self.respond(response),
            // Only retry requests that haven't been cancelled. Otherwise we do unnecessary work.
            Task::Retry(req) if !self.is_completed(&req) => self.on_request(req),
            Task::Retry(_) => (),
            Task::Diagnostics(kind) => {
                self.diagnostics.set_native_diagnostics(kind);
            }
            Task::PrimeCaches(progress) => match progress {
                PrimeCachesProgress::Begin => prime_caches_progress.push(progress),
                PrimeCachesProgress::Report(_) => {
                    match prime_caches_progress.last_mut() {
                        Some(last @ PrimeCachesProgress::Report(_)) => {
                            // Coalesce subsequent update events.
                            *last = progress;
                        }
                        _ => prime_caches_progress.push(progress),
                    }
                }
                PrimeCachesProgress::End { .. } => prime_caches_progress.push(progress),
            },
            Task::FetchWorkspace(progress) => {
                let (state, msg) = match progress {
                    ProjectWorkspaceProgress::Begin => (Progress::Begin, None),
                    ProjectWorkspaceProgress::Report(msg) => (Progress::Report, Some(msg)),
                    ProjectWorkspaceProgress::End(workspaces) => {
                        let resp = FetchWorkspaceResponse { workspaces };
                        self.fetch_workspaces_queue.op_completed(resp);
                        if let Err(e) = self.fetch_workspace_error() {
                            error!("FetchWorkspaceError:\n{e}");
                        }
                        self.wants_to_switch = Some("fetched workspace".to_owned());
                        self.diagnostics.clear_check_all();
                        (Progress::End, None)
                    }
                };

                self.report_progress("Fetching", state, msg, None, None);
            }
        }
    }

    fn handle_vfs_msg(&mut self, message: vfs::loader::Message) {
        let _p = tracing::span!(Level::INFO, "GlobalState::handle_vfs_msg").entered();
        let is_changed = matches!(message, vfs::loader::Message::Changed { .. });
        match message {
            vfs::loader::Message::Changed { files } | vfs::loader::Message::Loaded { files } => {
                let _p = tracing::span!(Level::INFO, "GlobalState::handle_vfs_msg{changed/load}")
                    .entered();
                let vfs = &mut self.vfs.write().0;
                for (path, contents) in files {
                    let path = VfsPath::from(path);
                    // if the file is in mem docs, it's managed by the client via notifications
                    // so only set it if its not in there
                    if !self.mem_docs.contains(&path)
                        && (is_changed || vfs.file_id(&path).is_none())
                    {
                        vfs.set_file_contents(path, contents);
                    }
                }
            }
            vfs::loader::Message::Progress { n_total, n_done, dir, config_version } => {
                let _p = span!(Level::INFO, "GlobalState::handle_vfs_msg/progress").entered();
                stdx::always!(config_version <= self.vfs_config_version);

                let (n_done, state) = match n_done {
                    LoadingProgress::Started => {
                        self.vfs_span =
                            Some(span!(Level::INFO, "vfs_load", total = n_total).entered());
                        (0, Progress::Begin)
                    }
                    LoadingProgress::Progress(n_done) => (n_done.min(n_total), Progress::Report),
                    LoadingProgress::Finished => {
                        self.vfs_span = None;
                        (n_total, Progress::End)
                    }
                };

                self.vfs_progress_config_version = config_version;
                self.vfs_done = state == Progress::End;

                let mut message = format!("{n_done}/{n_total}");
                if let Some(dir) = dir {
                    message += &format!(
                        ": {}",
                        match dir.strip_prefix(self.config.root_path()) {
                            Some(relative_path) => relative_path.as_utf8_path(),
                            None => dir.as_ref(),
                        }
                    );
                }

                self.report_progress(
                    "Roots Scanned",
                    state,
                    Some(message),
                    Some(Progress::fraction(n_done, n_total)),
                    None,
                );
            }
        }
    }

    fn handle_flycheck_msg(&mut self, message: FlycheckMessage) {
        let snap = self.snapshot();
        match message {
            FlycheckMessage::AddDiagnostic { id, diagnostic } => {
                let project = snap
                    .workspaces
                    .get(id)
                    .unwrap_or_else(|| panic!("workspace with {id} not found"));
                let diagnostics = crate::diagnostics::to_proto::map_vip_diagnostic_to_lsp(
                    &diagnostic,
                    &snap,
                    project,
                );
                for diag in diagnostics {
                    match url_to_file_id(&self.vfs.read().0, &diag.url) {
                        Ok(file_id) => self.diagnostics.add_check_diagnostic(
                            id,
                            file_id,
                            diag.diagnostic,
                            diag.fix,
                        ),
                        Err(err) => {
                            error!(
                                "flycheck {id}: File with vip diagnostic not found in VFS: {}",
                                err
                            );
                        }
                    };
                }
            }

            FlycheckMessage::ClearDiagnostics { id } => self.diagnostics.clear_check(id),

            FlycheckMessage::Progress { id, progress } => {
                let (state, message) = match progress {
                    flycheck::Progress::DidStart => (Progress::Begin, None),
                    flycheck::Progress::DidCompileFile(file_name) => {
                        (Progress::Report, Some(format!("compiled '{file_name}'")))
                    }
                    flycheck::Progress::DidCancel => {
                        self.last_flycheck_error = None;
                        (Progress::End, None)
                    }
                    flycheck::Progress::DidFailToRestart(err) => {
                        self.last_flycheck_error =
                            Some(format!("vip check failed to start: {err}"));
                        return;
                    }
                    flycheck::Progress::DidFinish(result) => {
                        self.last_flycheck_error =
                            result.err().map(|err| format!("vip check failed to start: {err}"));
                        (Progress::End, None)
                    }
                };

                // When we're running multiple flychecks, we have to include a disambiguator in
                // the title, or the editor complains. Note that this is a user-facing string.
                let name_str = match self.flycheck.get(id).map(|handle| handle.name().to_owned()) {
                    Some(name) => format!(" {name}"),
                    None => "".to_owned(),
                };
                let title = if self.flycheck.len() == 1 {
                    format!("{}{name_str}", self.config.flycheck())
                } else {
                    format!("{}{name_str} (#{})", self.config.flycheck(), id + 1)
                };
                self.report_progress(
                    &title,
                    state,
                    message,
                    None,
                    Some(format!("vip-analyzer/flycheck/{id}")),
                );
            }
        }
    }

    /// Registers and handles a request. This should only be called once per incoming request.
    fn on_new_request(&mut self, request_received: Instant, req: Request) {
        let _p =
            span!(Level::INFO, "GlobalState::on_new_request", req.method = ?req.method).entered();
        self.register_request(&req, request_received);
        self.on_request(req);
    }

    /// Handles a request.
    fn on_request(&mut self, req: Request) {
        let mut dispatcher = RequestDispatcher { req: Some(req), global_state: self };
        dispatcher.on_sync_mut::<lsp_types::request::Shutdown>(|s, ()| {
            s.shutdown_requested = true;
            Ok(())
        });

        match &mut dispatcher {
            RequestDispatcher { req: Some(req), global_state: this } if this.shutdown_requested => {
                this.respond(lsp_server::Response::new_err(
                    req.id.clone(),
                    lsp_server::ErrorCode::InvalidRequest as i32,
                    "Shutdown already requested.".to_owned(),
                ));
                return;
            }
            _ => (),
        }

        use crate::handlers::request as handlers;
        use lsp_types::request as lsp_request;

        const RETRY: bool = true;
        const NO_RETRY: bool = false;

        dispatcher
            // Request handlers that must run on the main thread
            // because they mutate GlobalState:
            .on_sync_mut::<lsp_ext::ReloadWorkspace>(handlers::handle_workspace_reload)
            // Formatting should be done immediately as the editor might wait on it, but we can't
            // put it on the main thread as we do not want the main thread to block on rustfmt.
            // So we have an extra thread just for formatting requests to make sure it gets handled
            // as fast as possible.
            .on_fmt_thread::<lsp_request::Formatting>(handlers::handle_formatting)
            // FIXME: Some of these NO_RETRY could be retries if the file they are interested didn't change.
            // All other request handlers
            .on_with_vfs_default::<lsp_request::DocumentDiagnosticRequest>(
                handlers::handle_document_diagnostics,
                || {
                    lsp_types::DocumentDiagnosticReportResult::Report(
                        lsp_types::DocumentDiagnosticReport::Full(
                            lsp_types::RelatedFullDocumentDiagnosticReport {
                                related_documents: None,
                                full_document_diagnostic_report:
                                    lsp_types::FullDocumentDiagnosticReport {
                                        result_id: Some("vip-analyzer".to_owned()),
                                        items: vec![],
                                    },
                            },
                        ),
                    )
                },
                || lsp_server::ResponseError {
                    code: lsp_server::ErrorCode::ServerCancelled as i32,
                    message: "server cancelled the request".to_owned(),
                    data: serde_json::to_value(lsp_types::DiagnosticServerCancellationData {
                        retrigger_request: true,
                    })
                    .ok(),
                },
            )
            .on::<NO_RETRY, lsp_request::GotoDefinition>(handlers::handle_goto_definition)
            .on::<NO_RETRY, lsp_request::GotoDeclaration>(handlers::handle_goto_declaration)
            .on::<NO_RETRY, lsp_request::References>(handlers::handle_references)
            .on::<RETRY, lsp_ext::AnalyzerStatus>(handlers::handle_analyzer_status)
            .on::<RETRY, lsp_ext::ViewFileText>(handlers::handle_view_file_text)
            .on::<NO_RETRY, lsp_ext::ViewSyntaxTree>(handlers::handle_view_syntax_tree)
            .finish();
    }

    /// Handles an incoming notification.
    fn on_notification(&mut self, not: Notification) {
        let _p =
            span!(Level::INFO, "GlobalState::on_notification", not.method = ?not.method).entered();
        use crate::handlers::notification as handlers;
        use lsp_types::notification as notifs;

        NotificationDispatcher { not: Some(not), global_state: self }
            .on_sync_mut::<notifs::Cancel>(handlers::handle_cancel)
            .on_sync_mut::<notifs::WorkDoneProgressCancel>(
                handlers::handle_work_done_progress_cancel,
            )
            .on_sync_mut::<notifs::DidOpenTextDocument>(handlers::handle_did_open_text_document)
            .on_sync_mut::<notifs::DidChangeTextDocument>(handlers::handle_did_change_text_document)
            .on_sync_mut::<notifs::DidCloseTextDocument>(handlers::handle_did_close_text_document)
            .on_sync_mut::<notifs::DidSaveTextDocument>(handlers::handle_did_save_text_document)
            .on_sync_mut::<notifs::DidChangeConfiguration>(
                handlers::handle_did_change_configuration,
            )
            .on_sync_mut::<notifs::DidChangeWorkspaceFolders>(
                handlers::handle_did_change_workspace_folders,
            )
            .on_sync_mut::<notifs::DidChangeWatchedFiles>(handlers::handle_did_change_watched_files)
            .on_sync_mut::<lsp_ext::CancelFlycheck>(handlers::handle_cancel_flycheck)
            .on_sync_mut::<lsp_ext::ClearFlycheck>(handlers::handle_clear_flycheck)
            .on_sync_mut::<lsp_ext::RunFlycheck>(handlers::handle_run_flycheck)
            .finish();
    }
}
