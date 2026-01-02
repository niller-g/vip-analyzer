//! Flycheck provides the functionality needed to run `cargo check` to provide
//! LSP diagnostics based on the output of the command.

use crate::command::{CommandHandle, VipCliParser};
use crossbeam_channel::{Receiver, Sender, select_biased, unbounded};
use paths::AbsPathBuf;
use project_model::{ManifestPath, ProDir};
use std::{fmt, io, process::Command, time::Duration};
use toolchain::{VipBuilderDiagnostic, VipBuilderOptions};
use triomphe::Arc;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub(crate) enum InvocationStrategy {
    #[default]
    PerWorkspace,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum FlycheckConfig {
    VipBuilderCommand { options: VipBuilderOptions },
}
impl fmt::Display for FlycheckConfig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlycheckConfig::VipBuilderCommand { options } => {
                write!(f, "vipBuilder")?;
                if let Some(action) = &options.action
                    && action != &toolchain::BuildAction::default()
                {
                    write!(f, " {}", action)?;
                }
                Ok(())
            }
        }
    }
}

/// Flycheck wraps the shared state and communication machinery used for
/// running `cargo check` (or other compatible command) and providing
/// diagnostics based on the output.
/// The spawned thread is shut down when this struct is dropped.
#[derive(Debug)]
pub(crate) struct FlycheckHandle {
    // XXX: drop order is significant
    sender: Sender<StateChange>,
    _thread: stdx::thread::JoinHandle,
    id: usize,
    name: String,
}

impl FlycheckHandle {
    pub(crate) fn spawn(
        id: usize,
        sender: Sender<FlycheckMessage>,
        config: FlycheckConfig,
        workspace_root: AbsPathBuf,
        manifest_path: ManifestPath,
        pro_dir: ProDir,
    ) -> FlycheckHandle {
        let name = manifest_path.derive_name().to_owned();
        let actor = FlycheckActor::new(id, sender, config, workspace_root, manifest_path, pro_dir);
        let (sender, receiver) = unbounded::<StateChange>();
        let thread =
            stdx::thread::Builder::new(stdx::thread::ThreadIntent::Worker, format!("Flycheck{id}"))
                .spawn(move || actor.run(receiver))
                .expect("failed to spawn thread");
        FlycheckHandle { id, sender, _thread: thread, name }
    }

    /// Schedule a re-start of the vip check worker
    pub(crate) fn restart_workspace(&self) {
        self.sender.send(StateChange::Restart).unwrap();
    }

    /// Stop this cargo check worker.
    pub(crate) fn cancel(&self) {
        self.sender.send(StateChange::Cancel).unwrap();
    }

    pub(crate) fn id(&self) -> usize {
        self.id
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub(crate) enum FlycheckMessage {
    /// Request adding a diagnostic with fixes included to a file
    AddDiagnostic { id: usize, diagnostic: VipBuilderDiagnostic },

    /// Request clearing all previous diagnostics
    ClearDiagnostics { id: usize },

    /// Request check progress notification to client
    Progress {
        /// Flycheck instance ID
        id: usize,
        progress: Progress,
    },
}

#[derive(Debug)]
pub(crate) enum Progress {
    DidStart,
    DidCompileFile(String),
    DidFinish(io::Result<()>),
    DidCancel,
    DidFailToRestart(String),
}

enum StateChange {
    Restart,
    Cancel,
}

/// A [`FlycheckActor`] is a single check instance of a workspace.
struct FlycheckActor {
    /// The workspace id of this flycheck instance.
    id: usize,

    sender: Sender<FlycheckMessage>,
    config: FlycheckConfig,
    manifest_path: ManifestPath,
    /// Either the workspace root of the workspace we are flychecking,
    /// or the project root of the project.
    root: Arc<AbsPathBuf>,
    pro_dir: ProDir,
    /// CargoHandle exists to wrap around the communication needed to be able to
    /// run `cargo check` without blocking. Currently the Rust standard library
    /// doesn't provide a way to read sub-process output without blocking, so we
    /// have to wrap sub-processes output handling in a thread and pass messages
    /// back over a channel.
    command_handle: Option<CommandHandle<VipCheckMessage>>,
    /// The receiver side of the channel mentioned above.
    command_receiver: Option<Receiver<VipCheckMessage>>,
    diagnostics_received: DiagnosticsReceived,
}

#[derive(PartialEq)]
enum DiagnosticsReceived {
    Yes,
    No,
    YesAndClearedForAll,
}

enum Event {
    RequestStateChange(StateChange),
    CheckEvent(Option<VipCheckMessage>),
}

impl FlycheckActor {
    fn new(
        id: usize,
        sender: Sender<FlycheckMessage>,
        config: FlycheckConfig,
        workspace_root: AbsPathBuf,
        manifest_path: ManifestPath,
        pro_dir: ProDir,
    ) -> FlycheckActor {
        tracing::info!(%id, ?workspace_root, "Spawning flycheck");
        FlycheckActor {
            id,
            sender,
            config,
            root: Arc::new(workspace_root),
            manifest_path,
            pro_dir,
            command_handle: None,
            command_receiver: None,
            diagnostics_received: DiagnosticsReceived::No,
        }
    }

    fn report_progress(&self, progress: Progress) {
        self.send(FlycheckMessage::Progress { id: self.id, progress });
    }

    fn next_event(&self, inbox: &Receiver<StateChange>) -> Option<Event> {
        let Some(command_receiver) = &self.command_receiver else {
            return inbox.recv().ok().map(Event::RequestStateChange);
        };

        // Biased to give restarts a preference so check outputs don't block a restart or stop
        select_biased! {
            recv(inbox) -> msg => msg.ok().map(Event::RequestStateChange),
            recv(command_receiver) -> msg => Some(Event::CheckEvent(msg.ok())),
        }
    }

    fn run(mut self, inbox: Receiver<StateChange>) {
        'event: while let Some(event) = self.next_event(&inbox) {
            match event {
                Event::RequestStateChange(StateChange::Cancel) => {
                    tracing::debug!(flycheck_id = self.id, "flycheck cancelled");
                    self.cancel_check_process();
                }
                Event::RequestStateChange(StateChange::Restart) => {
                    // Cancel the previously spawned process
                    self.cancel_check_process();
                    while let Ok(restart) = inbox.recv_timeout(Duration::from_millis(50)) {
                        // restart chained with a stop, so just cancel
                        if let StateChange::Cancel = restart {
                            continue 'event;
                        }
                    }

                    let Some(command) = self.check_command() else {
                        continue;
                    };

                    let formatted_command = format!("{command:?}");

                    tracing::debug!(?command, "will restart flycheck");
                    let (sender, receiver) = unbounded();
                    match CommandHandle::spawn(command, VipCheckParser, sender) {
                        Ok(command_handle) => {
                            tracing::debug!(command = formatted_command, "did restart flycheck");
                            self.command_handle = Some(command_handle);
                            self.command_receiver = Some(receiver);
                            self.report_progress(Progress::DidStart);
                        }
                        Err(error) => {
                            self.report_progress(Progress::DidFailToRestart(format!(
                                "Failed to run the following command: {formatted_command} error={error}"
                            )));
                        }
                    }
                }
                Event::CheckEvent(None) => {
                    tracing::debug!(flycheck_id = self.id, "flycheck finished");

                    // Watcher finished
                    let command_handle = self.command_handle.take().unwrap();
                    self.command_receiver.take();
                    let formatted_handle = format!("{command_handle:?}");

                    let res = command_handle.join();
                    if let Err(error) = &res {
                        tracing::error!(
                            "Flycheck failed to run the following command: {}, error={}",
                            formatted_handle,
                            error
                        );
                    }
                    if self.diagnostics_received == DiagnosticsReceived::No {
                        tracing::trace!(flycheck_id = self.id, "clearing diagnostics");
                        // We finished without receiving any diagnostics.
                        // Clear everything for good measure
                        self.send(FlycheckMessage::ClearDiagnostics { id: self.id });
                    }
                    self.clear_diagnostics_state();

                    self.report_progress(Progress::DidFinish(res));
                }
                Event::CheckEvent(Some(message)) => match message {
                    VipCheckMessage::FileCompiledMsg { file_name } => {
                        self.report_progress(Progress::DidCompileFile(file_name));
                        if self.diagnostics_received != DiagnosticsReceived::YesAndClearedForAll {
                            self.diagnostics_received = DiagnosticsReceived::YesAndClearedForAll;
                            tracing::trace!(flycheck_id = self.id, "clearing diagnostics");
                            self.send(FlycheckMessage::ClearDiagnostics { id: self.id });
                        }
                    }
                    VipCheckMessage::Diagnostic(msg) => {
                        tracing::trace!(
                            flycheck_id = self.id,
                            message = msg.to_string(),
                            "diagnostic received"
                        );
                        if self.diagnostics_received == DiagnosticsReceived::No {
                            self.diagnostics_received = DiagnosticsReceived::Yes;
                        }
                        if self.diagnostics_received != DiagnosticsReceived::YesAndClearedForAll {
                            self.diagnostics_received = DiagnosticsReceived::YesAndClearedForAll;
                            self.send(FlycheckMessage::ClearDiagnostics { id: self.id });
                        }

                        self.send(FlycheckMessage::AddDiagnostic { id: self.id, diagnostic: msg });
                    }
                },
            }
        }
        // If we rerun the thread, we need to discard the previous check results first
        self.cancel_check_process();
    }

    fn cancel_check_process(&mut self) {
        if let Some(command_handle) = self.command_handle.take() {
            tracing::debug!(
                command = ?command_handle,
                "did  cancel flycheck"
            );
            command_handle.cancel();
            self.command_receiver.take();
            self.report_progress(Progress::DidCancel);
        }
        self.clear_diagnostics_state();
    }

    fn clear_diagnostics_state(&mut self) {
        self.diagnostics_received = DiagnosticsReceived::No;
    }

    /// Construct a `Command` object for checking the user's code. If the user
    /// has specified a custom command with placeholders that we cannot fill,
    /// return None.
    fn check_command(&self) -> Option<Command> {
        match &self.config {
            FlycheckConfig::VipBuilderCommand { options } => {
                let mut cmd = toolchain::command(self.pro_dir.vip_builder_path()?, &*self.root);
                options.apply_on_command(&mut cmd, self.manifest_path.as_ref());

                Some(cmd)
            }
        }
    }

    #[track_caller]
    fn send(&self, check_task: FlycheckMessage) {
        self.sender.send(check_task).unwrap();
    }
}

enum VipCheckMessage {
    FileCompiledMsg { file_name: String },
    Diagnostic(VipBuilderDiagnostic),
}

struct VipCheckParser;

impl VipCliParser<VipCheckMessage> for VipCheckParser {
    fn from_line(&self, line: &str, error: &mut String) -> Option<VipCheckMessage> {
        let diagnostic: Result<VipBuilderDiagnostic, _> = line.parse();
        if let Ok(diagnostic) = diagnostic {
            return Some(VipCheckMessage::Diagnostic(diagnostic));
        } else if let Some(file_name) = toolchain::as_file_compiled_msg(line) {
            return Some(VipCheckMessage::FileCompiledMsg { file_name: file_name.to_owned() });
        }

        error.push_str(line);
        error.push('\n');
        None
    }

    fn from_eof(&self) -> Option<VipCheckMessage> {
        None
    }
}
