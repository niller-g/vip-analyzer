use {
    crate::{SourceFile, SyntaxError},
    expect_test::expect_file,
    std::{
        borrow::Cow,
        fs::{self},
        path::{Path, PathBuf},
    },
    test_utils::project_root,
};

#[test]
fn validation_tests() {
    dir_tests(&test_data_dir(), &["parser/validation"], "rast", |text, path| {
        let parse = SourceFile::parse(text);
        let errors = parse.errors();
        let dbg_dump = parse.debug_dump();
        assert_errors_are_present(&errors, path, &dbg_dump);
        dbg_dump
    });
}

fn test_data_dir() -> PathBuf {
    project_root().into_std_path_buf().join("crates/syntax/test_data")
}

fn assert_errors_are_present(errors: &[SyntaxError], path: &Path, dbg_dump: &str) {
    assert!(
        !errors.is_empty(),
        "There should be errors in the file {:?}\n{dbg_dump}",
        path.display()
    );
}

/// Calls callback `f` with input code and file paths for each vip file in `test_data_dir`
/// subdirectories defined by `paths`.
///
/// If the content of the matching output file differs from the output of `f()`
/// the test will fail.
///
/// If there is no matching output file it will be created and filled with the
/// output of `f()`, but the test will fail.
fn dir_tests<F>(test_data_dir: &Path, paths: &[&str], outfile_extension: &str, f: F)
where
    F: Fn(&str, &Path) -> String,
{
    for (path, input_code) in collect_vip_files(test_data_dir, paths) {
        let actual = f(&input_code, &path);
        let path = path.with_extension(outfile_extension);
        expect_file![path].assert_eq(&actual)
    }
}

/// Collects all vip files from `dir` subdirectories defined by `paths`.
fn collect_vip_files(root_dir: &Path, paths: &[&str]) -> Vec<(PathBuf, String)> {
    paths
        .iter()
        .flat_map(|path| {
            let path = root_dir.to_owned().join(path);
            vip_files_in_dir(&path).into_iter()
        })
        .map(|path| {
            let text = read_bytes(&path);
            (path, text)
        })
        .collect()
}

/// Collects paths to all vip files from `dir` in a sorted `Vec<PathBuf>`.
fn vip_files_in_dir(dir: &Path) -> Vec<PathBuf> {
    let mut acc = Vec::new();
    for file in fs::read_dir(dir).unwrap() {
        let file = file.unwrap();
        let path = file.path();
        let ext = path.extension().unwrap_or_default().to_str().unwrap();
        if matches!(ext, "pro" | "i" | "cl" | "ph" | "pack") {
            acc.push(path);
        }
    }
    acc.sort();
    acc
}

fn read_bytes(path: &Path) -> String {
    let bytes = std::fs::read(path).expect("File not found");
    stdx::to_utf8_else_utf16(&bytes, true).map(Cow::into_owned).expect("Invalid byte sequence")
}
