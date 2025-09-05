//! Defines `Fixture` -- a convenient way to describe the initial state of
//! rust-analyzer database from a single string.
//!
//! Fixtures are strings containing rust source code with optional metadata.
//! A fixture without metadata is parsed into a single source file.
//! Use this to test functionality local to one file.
//!
//! Simple Example:
//!
//! ```ignore
//! r#"
//! fn main() {
//!     println!("Hello World")
//! }
//! "#
//! ```
//!
//! Metadata can be added to a fixture after a `//-` comment.
//! The basic form is specifying filenames,
//! which is also how to define multiple files in a single test fixture
//!
//! Example using two files in the same crate:
//!
//! ```ignore
//! "
//! //- /main.rs
//! mod foo;
//! fn main() {
//!     foo::bar();
//! }
//!
//! //- /foo.rs
//! pub fn bar() {}
//! "
//! ```
//!
//! Example using two crates with one file each, with one crate depending on the other:
//!
//! ```ignore
//! r#"
//! //- /main.rs crate:a deps:b
//! fn main() {
//!     b::foo();
//! }
//! //- /lib.rs crate:b
//! pub fn b() {
//!     println!("Hello World")
//! }
//! "#
//! ```
//!
//! Metadata allows specifying all settings and variables
//! that are available in a real rust project. See [`Fixture`]
//! for the syntax.
//!
//! Example using some available metadata:
//!
//! ```ignore
//! "
//! //- /lib.rs crate:foo deps:bar,baz cfg:foo=a,bar=b env:OUTDIR=path/to,OTHER=foo
//! fn insert_source_code_here() {}
//! "
//! ```
//!
//!

use {rustc_hash::FxHashMap, stdx::trim_indent};

#[derive(Debug, Eq, PartialEq)]
pub struct Fixture {
    /// Specifies the path for this file. It must start with "/".
    pub path: String,
    // /// Specifies dependencies of this crate. This must be used with `crate` meta.
    // ///
    // /// Syntax: `deps:hir-def,ide-assists`
    // pub deps: Vec<String>,
    /// Specifies environment variables.
    ///
    /// Syntax: `env:PATH=/bin,RUST_LOG=debug`
    pub env: FxHashMap<String, String>,
    /// Actual file contents. All meta comments are stripped.
    pub text: String,
}

pub struct FixtureWithProjectMeta {
    pub fixture: Vec<Fixture>,
}

impl FixtureWithProjectMeta {
    /// Parses text which looks like this:
    ///
    ///  ```text
    ///  //- some meta
    ///  line 1
    ///  line 2
    ///  //- other meta
    ///  ```
    pub fn parse(fixture: &str) -> Self {
        let fixture = trim_indent(fixture);
        let fixture = fixture.as_str();
        let mut res: Vec<Fixture> = Vec::new();

        for (ix, line) in fixture.split_inclusive('\n').enumerate() {
            if line.contains("//-") {
                assert!(
                    line.starts_with("//-"),
                    "Metadata line {ix} has invalid indentation. \
                     All metadata lines need to have the same indentation.\n\
                     The offending line: {line:?}"
                );
            }

            if let Some(line) = line.strip_prefix("//-") {
                let meta = Self::parse_meta_line(line);
                res.push(meta);
            } else {
                if matches!(line.strip_prefix("// "), Some(l) if l.trim().starts_with('/')) {
                    panic!("looks like invalid metadata line: {line:?}");
                }

                if let Some(entry) = res.last_mut() {
                    entry.text.push_str(line);
                }
            }
        }

        Self { fixture: res }
    }

    fn parse_meta_line(meta: &str) -> Fixture {
        let meta = meta.trim();
        let mut components = meta.split_ascii_whitespace();

        let path = components.next().expect("fixture meta must start with a path").to_owned();
        assert!(path.starts_with('/'), "fixture path does not start with `/`: {path:?}");

        let mut env = FxHashMap::default();

        for component in components {
            let (key, value) =
                component.split_once(':').unwrap_or_else(|| panic!("invalid meta line: {meta:?}"));
            match key {
                "env" => {
                    for key in value.split(',') {
                        if let Some((k, v)) = key.split_once('=') {
                            env.insert(k.into(), v.into());
                        }
                    }
                }
                _ => panic!("bad component: {component:?}"),
            }
        }

        Fixture { path, env, text: String::new() }
    }
}
