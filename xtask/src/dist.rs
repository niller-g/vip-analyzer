use std::{
    fs::File,
    io::{self, BufWriter},
    path::{Path, PathBuf},
};

use flate2::{Compression, write::GzEncoder};
use time::OffsetDateTime;
use xshell::{Shell, cmd};
use zip::{DateTime, ZipWriter, write::SimpleFileOptions};

use crate::{
    date_iso,
    flags::{self, Platform},
    project_root,
    util::detect_target,
};

const VERSION_STABLE: &str = "0.3";
const VERSION_NIGHTLY: &str = "0.4";
const VERSION_DEV: &str = "0.5"; // keep this one in sync with `package.json`

impl flags::Dist {
    pub(crate) fn run(self, sh: &Shell) -> anyhow::Result<()> {
        let stable = sh.var("GITHUB_REF").unwrap_or_default().as_str() == "refs/heads/release";

        let project_root = project_root();
        let target = Target::get(&project_root, self.platform, sh);
        let dist = project_root.join("dist");
        sh.remove_path(&dist)?;
        sh.create_dir(&dist)?;

        if let Some(patch_version) = self.client_patch_version {
            let version = if stable {
                format!("{VERSION_STABLE}.{patch_version}")
            } else {
                // A hack to make VS Code prefer nightly over stable.
                format!("{VERSION_NIGHTLY}.{patch_version}")
            };

            dist_server(sh, &format!("{version}-standalone"), &target, &version)?;
            let release_tag = if stable { date_iso(sh)? } else { "nightly".to_owned() };
            dist_client(sh, &version, &release_tag, &target)?;
        } else {
            dist_server(sh, "0.0.0-standalone", &target, "0.0.0")?;
        }
        Ok(())
    }
}

fn dist_client(
    sh: &Shell,
    version: &str,
    release_tag: &str,
    target: &Target,
) -> anyhow::Result<()> {
    let bundle_path = Path::new("editors").join("code").join("server");
    if sh.path_exists(&bundle_path) {
        sh.remove_path(&bundle_path)?
    }
    sh.create_dir(&bundle_path)?;
    sh.copy_file(&target.server_path, &bundle_path)?;
    for fmt_file in &target.get_fmt_binaries() {
        sh.copy_file(fmt_file, &bundle_path)?;
    }
    if let Some(symbols_path) = &target.symbols_path {
        sh.copy_file(symbols_path, &bundle_path)?;
    }

    let _d = sh.push_dir("./editors/code");

    let mut patch = Patch::new(sh, "./package.json")?;
    patch
        .replace(
            &format!(r#""version": "{VERSION_DEV}.0-dev""#),
            &format!(r#""version": "{version}""#),
        )
        .replace(r#""releaseTag": null"#, &format!(r#""releaseTag": "{release_tag}""#))
        .replace(r#""title": "$generated-start""#, "")
        .replace(r#""title": "$generated-end""#, "")
        .replace(r#""enabledApiProposals": [],"#, r#""#);
    patch.commit(sh)?;

    if option_env!("CI").is_none() {
        println!("Building VSIX");
        println!("cwd={}", sh.current_dir().display());

        if cfg!(windows) {
            cmd!(sh, "cmd.exe /c npx vsce package -o '../../dist/vip-analyzer.vsix' --pre-release")
                .run()?;
        } else {
            cmd!(sh, "npx vsce package -o \"../../dist/vip-analyzer.vsix\" --pre-release").run()?;
        }
    }

    Ok(())
}

fn dist_server(sh: &Shell, release: &str, target: &Target, version: &str) -> anyhow::Result<()> {
    let _e = sh.push_env("CFG_RELEASE", release);
    let _e = sh.push_env("CARGO_PROFILE_RELEASE_LTO", "thin");

    // Uncomment to enable debug info for releases. Note that:
    //   * debug info is split on windows and macs, so it does nothing for those platforms,
    //   * on Linux, this blows up the binary size from 8MB to 43MB, which is unreasonable.
    // let _e = sh.push_env("CARGO_PROFILE_RELEASE_DEBUG", "1");

    let target_name = &target.name;
    cmd!(sh, "cargo build --manifest-path ./crates/vip-analyzer/Cargo.toml --bin vip-analyzer --target {target_name} --release").run()?;

    let dst = Path::new("dist").join(&target.artifact_name);

    let mut extra_files = Vec::new();
    if cfg!(windows) {
        if project_root().join("visual-prolog/ProDir").exists() {
            dist_vipfmt(sh, version, target)?;
        }
        extra_files.extend(target.get_fmt_binaries());
    }
    if let Some(symbols_path) = &target.symbols_path {
        extra_files.push(symbols_path.clone());
    }

    if target_name.contains("-windows-") {
        zip(&target.server_path, extra_files, &dst.with_extension("zip"))?;
    } else {
        gzip(&target.server_path, &dst.with_extension("gz"))?;
    }

    Ok(())
}

fn dist_vipfmt(sh: &Shell, version: &str, target: &Target) -> anyhow::Result<()> {
    let vip_dir = project_root().join("visual-prolog");
    let vipfmt = vip_dir.join("vipfmt/vipfmt.vipprj");
    let vip_builder = vip_dir.join("ProDir/bin64/vipBuilder.exe");
    let mut version_file = Patch::new(sh, vip_dir.join("vipfmt/main.version"))?;
    version_file.replace(r#"file_version="0.0.0.0""#, &format!(r#"file_version="{version}.0""#));
    version_file
        .replace(r#"product_version="0.0.0.0""#, &format!(r#"product_version="{version}.0""#));
    version_file.commit(sh)?;

    let p = target.platform.clone().unwrap_or_default().to_string();
    // The vipCompiler crashes when `nodebug` is used, even though it is desired to use that flag.
    // cmd!(sh, "{vip_builder} /platform {p} {vipfmt} /options 'profile:off nodebug warnings:aserror'").run()?;
    cmd!(sh, "{vip_builder} /platform {p} {vipfmt} /options 'profile:off warnings:aserror'")
        .run()?;

    Ok(())
}

fn gzip(src_path: &Path, dest_path: &Path) -> anyhow::Result<()> {
    let mut encoder = GzEncoder::new(File::create(dest_path)?, Compression::best());
    let mut input = io::BufReader::new(File::open(src_path)?);
    io::copy(&mut input, &mut encoder)?;
    encoder.finish()?;
    Ok(())
}

fn zip(src_path: &Path, extra_files: Vec<PathBuf>, dest_path: &Path) -> anyhow::Result<()> {
    let file = File::create(dest_path)?;
    let mut writer = ZipWriter::new(BufWriter::new(file));
    writer.start_file(
        src_path.file_name().unwrap().to_str().unwrap(),
        SimpleFileOptions::default()
            .last_modified_time(
                DateTime::try_from(OffsetDateTime::from(std::fs::metadata(src_path)?.modified()?))
                    .unwrap(),
            )
            .unix_permissions(0o755)
            .compression_method(zip::CompressionMethod::Deflated)
            .compression_level(Some(9)),
    )?;
    let mut input = io::BufReader::new(File::open(src_path)?);
    io::copy(&mut input, &mut writer)?;
    for file in extra_files {
        writer.start_file(
            file.file_name().unwrap().to_str().unwrap(),
            SimpleFileOptions::default()
                .last_modified_time(
                    DateTime::try_from(OffsetDateTime::from(
                        std::fs::metadata(src_path)?.modified()?,
                    ))
                    .unwrap(),
                )
                .compression_method(zip::CompressionMethod::Deflated)
                .compression_level(Some(9)),
        )?;
        let mut input = io::BufReader::new(File::open(file)?);
        io::copy(&mut input, &mut writer)?;
    }
    writer.finish()?;
    Ok(())
}

struct Target {
    name: String,
    server_path: PathBuf,
    symbols_path: Option<PathBuf>,
    artifact_name: String,
    platform: Option<Platform>,
}

impl Target {
    fn get(project_root: &Path, platform: Option<Platform>, sh: &Shell) -> Self {
        let name = detect_target(sh);
        let (name, _libc_suffix) = match name.split_once('.') {
            Some((l, r)) => (l.to_owned(), Some(r.to_owned())),
            None => (name, None),
        };
        let out_path = project_root.join("target").join(&name).join("release");
        let (exe_suffix, symbols_path) = if name.contains("-windows-") {
            (".exe".into(), Some(out_path.join("vip_analyzer.pdb")))
        } else {
            (String::new(), None)
        };
        let server_path = out_path.join(format!("vip-analyzer{exe_suffix}"));

        let artifact_name = format!("vip-analyzer-{name}{exe_suffix}");
        Self { name, server_path, symbols_path, artifact_name, platform }
    }

    fn get_fmt_binaries(&self) -> Vec<PathBuf> {
        let fmt_bin_dir = project_root().join(format!(
            "visual-prolog/vipfmt/exe{suffix}",
            suffix = self.platform.as_ref().unwrap_or(&Platform::X64).vip_dir_suffix()
        ));

        let mut fmt_binaries = Vec::new();
        for bin in fmt_bin_dir.read_dir().unwrap() {
            let bin = bin.unwrap();
            fmt_binaries.push(bin.path());
        }

        fmt_binaries
    }
}

struct Patch {
    path: PathBuf,
    original_contents: String,
    contents: String,
}

impl Patch {
    fn new(sh: &Shell, path: impl Into<PathBuf>) -> anyhow::Result<Patch> {
        let path = path.into();
        let contents = sh.read_file(&path)?;
        Ok(Patch { path, original_contents: contents.clone(), contents })
    }

    fn replace(&mut self, from: &str, to: &str) -> &mut Patch {
        assert!(self.contents.contains(from));
        self.contents = self.contents.replace(from, to);
        self
    }

    fn commit(&self, sh: &Shell) -> anyhow::Result<()> {
        sh.write_file(&self.path, &self.contents)?;
        Ok(())
    }
}

impl Drop for Patch {
    fn drop(&mut self) {
        // FIXME: find a way to bring this back
        let _ = &self.original_contents;
        // write_file(&self.path, &self.original_contents).unwrap();
    }
}
