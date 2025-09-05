//! Implementation of the LSP for rust-analyzer.
//!
//! This crate takes Rust-specific analysis results from ide and translates
//! into LSP types.
//!
//! It also is the root of all state. `world` module defines the bulk of the
//! state, and `main_loop` module defines the rules for modifying it.
//!
//! The `cli` submodule implements some batch-processing analysis, primarily as
//! a debugging aid.

pub mod cli;
mod command;
mod diagnostics;
mod flycheck;
mod line_index;
mod main_loop;
mod mem_docs;
mod op_queue;
mod reload;
mod task_pool;
#[cfg(test)]
mod tests;
mod version;
mod handlers {
    pub(crate) mod dispatch;
    pub(crate) mod notification;
    pub(crate) mod request;
}
pub mod tracing {
    pub mod config;
    pub mod json;
    pub use config::Config;
    pub mod hprof;
}
pub mod config;
mod global_state;
pub mod lsp;

use self::lsp::ext as lsp_ext;
pub use crate::{lsp::capabilities::server_capabilities, main_loop::main_loop, version::version};
use serde::de::DeserializeOwned;
use std::{env, path::PathBuf};

pub fn from_json<T: DeserializeOwned>(
    what: &'static str,
    json: &serde_json::Value,
) -> anyhow::Result<T> {
    serde_json::from_value(json.clone())
        .map_err(|e| anyhow::format_err!("Failed to deserialize {what}: {e}; {json}"))
}

pub const VIPFMT_PATH: &str = "VIPFMT_PATH";

/// The vipfmt executable is currently located in the same directory as the vip-analyzer executable.
/// This function returns the path to the vipfmt executable.
pub(crate) fn vipfmt_path() -> anyhow::Result<PathBuf> {
    let vipfmt = env::var(VIPFMT_PATH)
        .ok()
        .map(PathBuf::from)
        .or_else(|| {
            std::env::current_exe()
                .ok()
                .map(|p| p.with_file_name(format!("vipfmt{}", std::env::consts::EXE_SUFFIX)))
        })
        .filter(|p| p.exists());

    vipfmt.ok_or_else(|| {
        anyhow::anyhow!(
            "Failed to find vipfmt executable. Try setting env:{} or passing --vipfmt-path",
            VIPFMT_PATH
        )
    })
}
