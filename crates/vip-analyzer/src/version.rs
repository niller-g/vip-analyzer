//! Code for representing vip-analyzer's release version number.

use std::fmt;

/// Cargo's version.
pub struct VersionInfo {
    pub version: &'static str,
}

impl fmt::Display for VersionInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.version)
    }
}

pub const fn version() -> VersionInfo {
    let version = match option_env!("CFG_RELEASE") {
        Some(x) => x,
        None => "0.0.0",
    };

    VersionInfo { version }
}
