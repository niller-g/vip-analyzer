//! See <https://github.com/matklad/cargo-xtask/>.
//!
//! This binary defines various auxiliary build commands, which are not
//! expressible with just `cargo`. Notably, it provides tests via `cargo test -p xtask`
//! for code generation and `cargo xtask install` for installation of
//! rust-analyzer server and client.
//!
//! This binary is integrated into the `cargo` command line by using an alias in
//! `.cargo/config`.

#![expect(clippy::print_stdout)]

mod codegen;
mod dist;
mod flags;
mod install;
mod util;
mod verify;

use std::{env, path::PathBuf};
use xshell::{Shell, cmd};

fn main() -> anyhow::Result<()> {
    let flags = flags::Xtask::from_env_or_exit();

    let sh = &Shell::new()?;
    match flags.subcommand {
        flags::XtaskCmd::Baseline(flags::Baseline) => baseline(sh),
        flags::XtaskCmd::Codegen(cmd) => cmd.run(sh),
        flags::XtaskCmd::Dist(cmd) => cmd.run(sh),
        flags::XtaskCmd::Install(cmd) => cmd.run(sh),
        flags::XtaskCmd::TestAll(cmd) => cmd.run(sh),
        flags::XtaskCmd::Verify(cmd) => cmd.run(sh),
    }
}

/// Returns the path to the root directory of `vip-analyzer` project.
fn project_root() -> PathBuf {
    let dir =
        env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned());
    PathBuf::from(dir).parent().unwrap().to_owned()
}

fn baseline(sh: &Shell) -> anyhow::Result<()> {
    let original_value = env::var("UPDATE_EXPECT").ok();
    unsafe {
        env::set_var("UPDATE_EXPECT", "1");
    }
    cmd!(sh, "cargo test").run()?;
    unsafe {
        match original_value {
            Some(val) => env::set_var("UPDATE_EXPECT", val),
            None => env::remove_var("UPDATE_EXPECT"),
        }
    }
    Ok(())
}

impl flags::TestAll {
    pub(crate) fn run(self, sh: &Shell) -> anyhow::Result<()> {
        let original_slow_tests = env::var("RUN_SLOW_TESTS").ok();
        let original_expect = self.baseline.then_some(env::var("UPDATE_EXPECT").ok());
        unsafe {
            env::set_var("RUN_SLOW_TESTS", "1");
        }
        if self.baseline {
            unsafe {
                env::set_var("UPDATE_EXPECT", "1");
            }
        }

        cmd!(sh, "cargo test").run()?;

        unsafe {
            match original_slow_tests {
                Some(val) => env::set_var("RUN_SLOW_TESTS", val),
                None => env::remove_var("RUN_SLOW_TESTS"),
            }
            match original_expect {
                Some(Some(val)) => env::set_var("UPDATE_EXPECT", val),
                Some(None) => env::remove_var("UPDATE_EXPECT"),
                None => {}
            }
        }
        Ok(())
    }
}

fn date_iso(sh: &Shell) -> anyhow::Result<String> {
    let res = cmd!(sh, "date -u +%d-%m-%Y").read()?;
    Ok(res)
}
