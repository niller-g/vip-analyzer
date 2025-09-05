use crate::{flags, project_root};
use xshell::{Shell, cmd};
use {crate::flags::TestAll, anyhow::bail};

impl flags::Verify {
    pub(crate) fn run(self, sh: &Shell) -> anyhow::Result<()> {
        sh.change_dir(project_root());

        cmd!(sh, "cargo clippy --all-features -- -D warnings").run()?;
        TestAll::run(TestAll { baseline: false }, sh)?;
        cmd!(sh, "cargo fmt --all -- --check").run()?;

        if cmd!(sh, "typos --version").run().is_err() {
            bail!("Please install `typos` by running `cargo install typos-cli`");
        } else {
            cmd!(sh, "typos").run()?;
        }

        if cfg!(windows) {
            cmd!(sh, "visual-prolog/ProDir/bin64/vipBuilder.exe visual-prolog/vipfmt/vipfmt.vipprj /platform x64 /options 'profile:off nodebug warnings:aserror'").run()?;
        }

        let _dir = sh.push_dir("./editors/code");
        if cfg!(windows) {
            cmd!(sh, "cmd.exe /c npm run typecheck").run()?;
            cmd!(sh, "cmd.exe /c npm run lint").run()?;
            cmd!(sh, "cmd.exe /c npm run format:check").run()?;
            cmd!(sh, "cmd.exe /c npm test").run()?;
        } else {
            cmd!(sh, "npm run typecheck").run()?;
            cmd!(sh, "npm run lint").run()?;
            cmd!(sh, "npm run format:check").run()?;
            cmd!(sh, "npm test").run()?;
        }

        Ok(())
    }
}
