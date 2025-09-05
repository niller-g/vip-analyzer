#[cfg(test)]
mod test_fixtures;

use crate::{LexedStr, grammar};
#[cfg(test)]
use expect_test::expect_file;
use std::fmt::Write;
#[cfg(test)]
use std::{
    fs,
    path::{Path, PathBuf},
};

#[path = "../test_data/generated/runner.rs"]
mod runner;

#[test]
fn lex_ok() {
    for case in TestCase::list("lexer/ok") {
        let _guard = stdx::panic_context::enter(format!("{:?}", case.path));
        let actual = lex(&case.text);
        expect_file![case.rast].assert_eq(&actual)
    }
}

#[test]
fn lex_err() {
    for case in TestCase::list("lexer/err") {
        let _guard = stdx::panic_context::enter(format!("{:?}", case.path));
        let actual = lex(&case.text);
        expect_file![case.rast].assert_eq(&actual)
    }
}

#[cfg(test)]
fn lex(text: &str) -> String {
    let lexed = LexedStr::new(text);

    let mut res = String::new();
    for i in 0..lexed.len() {
        let kind = lexed.kind(i);
        let text = lexed.text(i);
        let error = lexed.error(i);

        let error = error.map(|err| format!(" error: {err}")).unwrap_or_default();
        writeln!(res, "{kind:?} {text:?}{error}").unwrap();
    }
    res
}

#[test]
fn parse_ok() {
    for case in TestCase::list("parser/ok") {
        let _guard = stdx::panic_context::enter(format!("{:?}", case.path));
        let (actual, errors) = parse(grammar::source_file, &case.text);
        assert!(errors.is_empty(), "errors in an OK file {}:\n{errors:?}", case.path.display());
        expect_file![case.rast].assert_eq(&actual);
    }
}

#[test]
fn parse_err() {
    for case in TestCase::list("parser/err") {
        let _guard = stdx::panic_context::enter(format!("{:?}", case.path));
        let (actual, errors) = parse(grammar::source_file, &case.text);
        assert!(!errors.is_empty(), "no errors in an ERR file {}:\n{actual}", case.path.display());
        expect_file![case.rast].assert_eq(&actual)
    }
}

#[cfg(test)]
#[macro_export]
macro_rules! test_parser {
    ($func:expr, $text:expr) => {
        $crate::tests::parse(
            |p| {
                let m = p.start();
                let _ = $func(p);
                m.complete(p, $crate::SyntaxKind::SOURCE_FILE);
            },
            $text,
        )
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! parse_ok {
    ($func:expr, $text:expr) => {
        let (actual, errors) = $crate::test_parser!($func, $text);
        assert!(errors.is_empty(), "unexpected errors:\n{errors:?}");
    };
    ($func:expr, $text:expr, $expect:expr) => {
        let (actual, errors) = $crate::test_parser!($func, $text);
        assert!(errors.is_empty(), "unexpected errors:\n{errors:?}");
        $expect.assert_eq(&actual);
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! parse_err {
    ($func:expr, $text:expr) => {
        let (actual, errors) = $crate::test_parser!($func, $text);
        assert!(!errors.is_empty(), "no errors when expected\n{actual}");
    };
    ($func:expr, $text:expr, $expect:expr) => {
        let (actual, errors) = $crate::test_parser!($func, $text);
        assert!(!errors.is_empty(), "no errors when expected\n{actual}");
        $expect.assert_eq(&actual);
    };
}

#[cfg(feature = "rast-parser")]
pub fn parse_rast(text: &str) -> String {
    parse(grammar::source_file, text).0
}

#[cfg(any(test, feature = "rast-parser"))]
pub(crate) fn parse(
    entry_point: fn(&'_ mut crate::parser::Parser<'_>),
    text: &str,
) -> (String, Vec<String>) {
    let lexed = LexedStr::new(text);
    let input = lexed.to_input();
    let output = crate::parse(entry_point, &input);

    let mut buf = String::new();
    let mut errors = Vec::new();
    let mut indent = String::new();
    let mut depth = 0;
    let mut len = 0;
    lexed.intersperse_trivia(&output, &mut |step| match step {
        crate::StrStep::Token { kind, text } => {
            assert!(depth > 0);
            len += text.len();
            writeln!(buf, "{indent}{kind:?} {text:?}").unwrap();
        }
        crate::StrStep::Enter { kind } => {
            assert!(depth > 0 || len == 0);
            depth += 1;
            writeln!(buf, "{indent}{kind:?}").unwrap();
            indent.push_str("  ");
        }
        crate::StrStep::Exit => {
            assert!(depth > 0);
            depth -= 1;
            indent.pop();
            indent.pop();
        }
        crate::StrStep::Error { msg, pos } => {
            assert!(depth > 0);
            errors.push(format!("error {pos}: {msg}\n"))
        }
    });
    assert_eq!(
        len,
        text.len(),
        "didn't parse all text.\nParsed:\n{}\n\nAll:\n{}\n",
        &text[..len],
        text
    );

    for (token, msg) in lexed.errors() {
        let pos = lexed.text_start(token);
        errors.push(format!("error {pos}: {msg}\n"));
    }
    for e in &errors {
        buf.push_str(e);
    }

    (buf, errors)
}

#[cfg(test)]
#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct TestCase {
    path: PathBuf,
    rast: PathBuf,
    text: String,
}
#[cfg(test)]
impl TestCase {
    const fn ext() -> &'static str {
        "pro"
    }

    fn list(path: &'static str) -> Vec<TestCase> {
        let crate_root_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
        let test_data_dir = crate_root_dir.join("test_data");
        let dir = test_data_dir.join(path);

        let mut res = Vec::new();
        let read_dir = fs::read_dir(&dir)
            .unwrap_or_else(|err| panic!("can't `read_dir` {}: {err}", dir.display()));
        for file in read_dir {
            let file = file.unwrap();
            let path = file.path();
            if path.extension().unwrap_or_default() == TestCase::ext() {
                let rast = path.with_extension("rast");
                let text = fs::read_to_string(&path).unwrap();
                res.push(TestCase { path, rast, text });
            }
        }
        res.sort();
        res
    }
}

#[cfg(test)]
#[track_caller]
fn run_and_expect_no_errors(path: &str) {
    let path = PathBuf::from(path);
    let text = std::fs::read_to_string(&path).unwrap();
    let (actual, errors) = parse(grammar::source_file, &text);
    assert!(errors.is_empty(), "errors in an OK file {}:\n{errors:?}", path.display());
    let mut p = PathBuf::from("..");
    p.push(path);
    p.set_extension("rast");
    expect_file![p].assert_eq(&actual)
}

#[cfg(test)]
#[track_caller]
fn run_and_expect_errors(path: &str) {
    let path = PathBuf::from(path);
    let text = std::fs::read_to_string(&path).unwrap();
    let (actual, errors) = parse(grammar::source_file, &text);
    assert!(!errors.is_empty(), "no errors in an ERR file {}:\n{actual}", path.display());
    let mut p = PathBuf::from("..");
    p.push(path);
    p.set_extension("rast");
    expect_file![p].assert_eq(&actual)
}
