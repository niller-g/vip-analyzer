use test_utils::skip_slow_tests;
use walkdir::WalkDir;
use {
    crate::{grammar, tests::parse},
    std::borrow::Cow,
};
use {
    std::path::{Path, PathBuf},
    test_utils::project_root,
};

#[test]
fn vip_examples() {
    if skip_slow_tests() {
        return;
    }
    let folder = project_root().join("visual-prolog/examples");
    for file in vip_files(folder.as_std_path()) {
        // println!("Checking {}", file.display());
        let (_, errors) = parse(grammar::source_file, &read_bytes(&file));
        assert!(errors.is_empty(), "errors in '{}':\n{errors:?}", file.display());
    }
}

#[test]
fn pfc() {
    if skip_slow_tests() {
        return;
    }
    let folder = project_root().join("visual-prolog/ProDir/pfc");
    for file in vip_files(folder.as_std_path()) {
        // println!("Checking {}", file.display());
        let (_, errors) = parse(grammar::source_file, &read_bytes(&file));
        assert!(errors.is_empty(), "errors in '{}':\n{errors:?}", file.display());
    }
}

fn read_bytes(path: &Path) -> String {
    let bytes = std::fs::read(path).expect("File not found");
    stdx::to_utf8_else_utf16(&bytes, true).map(Cow::into_owned).expect("Invalid byte sequence")
}

fn vip_files(path: &Path) -> impl Iterator<Item = PathBuf> {
    WalkDir::new(path).into_iter().filter_map(|e| e.ok().map(|e| e.path().to_path_buf())).filter(
        |entry| {
            entry
                .extension()
                .map(|ext| {
                    ext.to_str()
                        .map(|ext_str| matches!(ext_str, "pro" | "i" | "cl" | "ph" | "pack"))
                        .unwrap_or_default()
                })
                .unwrap_or_default()
        },
    )
}
