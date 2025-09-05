//! base_db defines basic database traits. The concrete DB is defined by ide.

pub use salsa;
pub use salsa_macros;

// FIXME: Rename this crate, base db is non descriptive
mod change;
mod input;

use std::{cell::RefCell, hash::BuildHasherDefault, panic, sync::Once};

use paths::{Utf8Path, Utf8PathBuf};
use salsa::{Durability, Setter};
use std::path::PathBuf;
use syntax::{Parse, SourceFile, SyntaxError, ast};
use triomphe::Arc;
use vfs::{AbsPath, AbsPathBuf};

pub use crate::{
    change::FileChange,
    input::{
        Env, ExtraProjectData, IncludedPathPrecedence, IncludedPaths, Project, ProjectBuilder,
        ProjectBuilderId, ProjectData, ProjectGraphBuilder, ProjectsIdMap, UniqueProjectData,
    },
};
pub use query_group::{self};
pub use span::{FilePosition, FileRange};
pub use vfs::{AnchoredPath, AnchoredPathBuf, FileId, VfsPath};

use dashmap::{DashMap, mapref::entry::Entry};
use path_slash::PathBufExt;
use rustc_hash::FxHasher;
pub use semver::{BuildMetadata, Prerelease, Version, VersionReq};

#[macro_export]
macro_rules! impl_intern_key {
    ($id:ident, $loc:ident) => {
        #[salsa_macros::interned(no_lifetime)]
        pub struct $id {
            pub loc: $loc,
        }

        // If we derive this salsa prints the values recursively, and this causes us to blow.
        impl ::std::fmt::Debug for $id {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                f.debug_tuple(stringify!($id))
                    .field(&format_args!("{:04x}", self.0.as_u32()))
                    .finish()
            }
        }
    };
}

pub const DEFAULT_FILE_TEXT_LRU_CAP: u16 = 16;
pub const DEFAULT_PARSE_LRU_CAP: u16 = 128;

#[derive(Debug, Default)]
pub struct Files {
    files: Arc<DashMap<vfs::FileId, FileText, BuildHasherDefault<FxHasher>>>,
    paths: DashMap<vfs::VfsPath, vfs::FileId, BuildHasherDefault<FxHasher>>,
}

impl Files {
    pub fn file_text(&self, file_id: vfs::FileId) -> FileText {
        match self.files.get(&file_id) {
            Some(text) => *text,
            None => {
                panic!("Unable to fetch file text for `vfs::FileId`: {file_id:?}; this is a bug")
            }
        }
    }

    pub fn set_file_text(
        &self,
        db: &mut dyn SourceDatabase,
        file_id: vfs::FileId,
        path: VfsPath,
        text: &str,
    ) {
        match self.files.entry(file_id) {
            Entry::Occupied(mut occupied) => {
                occupied.get_mut().set_text(db).to(Arc::from(text));
            }
            Entry::Vacant(vacant) => {
                let text = FileText::new(db, Arc::from(text), file_id);
                vacant.insert(text);
            }
        };
        self.paths.insert(path, file_id);
    }

    pub fn set_file_text_with_durability(
        &self,
        db: &mut dyn SourceDatabase,
        file_id: vfs::FileId,
        path: VfsPath,
        text: &str,
        durability: Durability,
    ) {
        match self.files.entry(file_id) {
            Entry::Occupied(mut occupied) => {
                occupied.get_mut().set_text(db).with_durability(durability).to(Arc::from(text));
            }
            Entry::Vacant(vacant) => {
                let text =
                    FileText::builder(Arc::from(text), file_id).durability(durability).new(db);
                vacant.insert(text);
            }
        };
        self.paths.insert(path, file_id);
    }

    pub fn file_for_path(&self, path: VfsPath) -> Option<FileId> {
        self.paths.get(&path).map(|it| *it)
    }
}

#[salsa_macros::input(debug)]
pub struct FileText {
    pub text: Arc<str>,
    pub file_id: vfs::FileId,
}

/// Database which stores all significant input facts: source code and project
/// model. Everything else in vip-analyzer is derived from these queries.
#[query_group::query_group]
pub trait RootQueryDb: SourceDatabase + salsa::Database {
    /// Parses the file into the syntax tree.
    #[salsa::invoke_interned(parse)]
    #[salsa::lru(128)]
    fn parse(&self, file_id: vfs::FileId) -> Parse<ast::SourceFile>;

    /// Returns the set of errors obtained from parsing the file including validation errors.
    #[salsa::transparent]
    fn parse_errors(&self, file_id: FileId) -> Option<&[SyntaxError]>;

    fn resolve_path(&self, path: String, project: Project) -> Option<vfs::FileId>;

    #[salsa::transparent]
    fn resolve_paths(&self, path: AnchoredPath<'_>) -> Vec<vfs::FileId>;

    /// Returns the projects.
    ///
    /// **Warning**: do not use this query in `hir-*` crates! It kills incrementality across crate metadata modifications.
    #[salsa::input]
    fn all_projects(&self) -> Arc<Box<[Project]>>;
}

/// The mapping from [`UniqueProjectData`] to their [`Project`] input.
#[derive(Debug, Default)]
pub struct ProjectsMap(DashMap<UniqueProjectData, Project, BuildHasherDefault<FxHasher>>);

#[salsa_macros::db]
pub trait SourceDatabase: salsa::Database {
    /// Text of the file.
    fn file_text(&self, file_id: vfs::FileId) -> FileText;

    fn set_file_text(&mut self, file_id: vfs::FileId, path: VfsPath, text: &str);

    fn set_file_text_with_durability(
        &mut self,
        file_id: vfs::FileId,
        path: VfsPath,
        text: &str,
        durability: Durability,
    );

    fn file_for_path(&self, file_id: VfsPath) -> Option<FileId>;

    #[doc(hidden)]
    fn projects_map(&self) -> Arc<ProjectsMap>;
}

fn parse(db: &dyn RootQueryDb, file_id: vfs::FileId) -> Parse<ast::SourceFile> {
    let _p = tracing::info_span!("parse", ?file_id).entered();
    let text = db.file_text(file_id).text(db);
    SourceFile::parse(&text)
}

fn parse_errors(db: &dyn RootQueryDb, file_id: vfs::FileId) -> Option<&[SyntaxError]> {
    #[salsa_macros::interned(no_lifetime)]
    struct InternedFileId {
        pub file_id: vfs::FileId,
    }

    #[salsa_macros::tracked(return_ref)]
    fn parse_errors(db: &dyn RootQueryDb, file_id: InternedFileId) -> Option<Box<[SyntaxError]>> {
        let errors = db.parse(file_id.file_id(db)).errors();
        match &*errors {
            [] => None,
            [..] => Some(errors.into()),
        }
    }
    parse_errors(db, InternedFileId::new(db, file_id)).as_ref().map(|it| &**it)
}

fn resolve_path(db: &dyn RootQueryDb, path: String, project: Project) -> Option<vfs::FileId> {
    let project = project.extra_data(db);
    let project_root = project.project_root().as_path()?;
    let includes = project.included_paths().filter_map(VfsPath::as_path);
    let path = absolutize_project_path(path, includes, project_root)?;
    let path = VfsPath::from(path);

    db.file_for_path(path)
}

fn resolve_paths(db: &dyn RootQueryDb, path: AnchoredPath<'_>) -> Vec<vfs::FileId> {
    let _p = tracing::info_span!("resolve_path", ?path).entered();

    let mut res = db
        .all_projects()
        .iter()
        .filter_map(|project| db.resolve_path(path.path.into(), *project))
        .collect::<Vec<_>>();

    res.sort();
    res.dedup();
    res
}

/// Make a path absolute by resolving it against the project context.
///
/// 1. If the path is absolute, return it as is.
/// 2. If the path is relative, try to resolve it against the included paths.
/// 3. If the path is relative and no included paths are found, resolve it against the project root.
pub fn absolutize_project_path<'a, P, A>(
    path: P,
    included_paths: impl Iterator<Item = A>,
    project_root: &AbsPath,
) -> Option<AbsPathBuf>
where
    P: AsRef<str>,
    A: Into<&'a AbsPath>,
{
    let path = PathBuf::from_backslash(path);
    let path = Utf8PathBuf::try_from(path).ok()?;
    if path.is_absolute() {
        try_canonical(&path)
    } else {
        for dir in included_paths {
            if let Some(p) = try_canonical(dir.into().absolutize(&path).as_ref()) {
                return Some(p);
            }
        }
        try_canonical(project_root.absolutize(&path).as_ref())
    }
}

fn try_canonical(utf8_path: &Utf8Path) -> Option<AbsPathBuf> {
    let path = canonicalize_utf8(utf8_path).ok()?;
    let mut components = path.components();
    let utf8_buf = match components.next() {
        Some(paths::Utf8Component::Prefix(first)) => match first.kind() {
            paths::Utf8Prefix::Verbatim(_)
            | paths::Utf8Prefix::VerbatimUNC(..)
            | paths::Utf8Prefix::VerbatimDisk(_) => {
                let first = first.as_str().trim_start_matches(r"\\?\");
                Utf8PathBuf::from(first).join(components.as_path())
            }
            _ => path,
        },
        _ => path,
    };

    AbsPathBuf::try_from(utf8_buf).ok()
}

#[cfg(target_os = "windows")]
fn canonicalize_utf8(utf8_path: &Utf8Path) -> anyhow::Result<Utf8PathBuf> {
    let utf8_path = utf8_path.canonicalize_utf8()?;
    Ok(utf8_path)
}

#[cfg(not(target_os = "windows"))]
fn canonicalize_utf8(utf8_path: &Utf8Path) -> anyhow::Result<Utf8PathBuf> {
    use std::{fs, io, path::Component, path::Path};

    /// Given a possibly‐lowercase path, returns a PathBuf with each component
    /// replaced by the actual on-disk name (when found).
    fn with_actual_casing<P: AsRef<Path>>(p: P) -> io::Result<PathBuf> {
        let p = p.as_ref();
        let mut corrected = if p.is_absolute() {
            PathBuf::from(Component::RootDir.as_os_str())
        } else {
            PathBuf::new()
        };

        for comp in p.components() {
            match comp {
                Component::RootDir | Component::Prefix(_) => {
                    // Windows prefixes or “/” on Unix
                    corrected.push(comp.as_os_str());
                }
                Component::Normal(os_name) => {
                    // list parent directory
                    let parent =
                        if corrected.as_os_str().is_empty() { Path::new(".") } else { &corrected };
                    let want = os_name.to_string_lossy().to_lowercase();
                    let mut found = None;

                    for entry in fs::read_dir(parent)? {
                        let entry = entry?;
                        let file_name = entry.file_name();
                        if file_name.to_string_lossy().to_lowercase() == want {
                            found = Some(file_name);
                            break;
                        }
                    }

                    // if we found the real name, use it; otherwise fall back to original
                    corrected.push(found.unwrap_or_else(|| os_name.to_os_string()));
                }
                Component::CurDir | Component::ParentDir => {
                    corrected.push(comp.as_os_str());
                }
            }
        }

        Ok(corrected)
    }

    let path = with_actual_casing(utf8_path.as_std_path())?;
    let uft8_path = Utf8PathBuf::try_from(path)?;
    Ok(uft8_path)
}

#[must_use]
#[non_exhaustive]
pub struct DbPanicContext;

impl Drop for DbPanicContext {
    fn drop(&mut self) {
        Self::with_ctx(|ctx| assert!(ctx.pop().is_some()));
    }
}

impl DbPanicContext {
    pub fn enter(frame: String) -> DbPanicContext {
        #[expect(clippy::print_stderr, reason = "already panicking anyway")]
        fn set_hook() {
            let default_hook = panic::take_hook();
            panic::set_hook(Box::new(move |panic_info| {
                default_hook(panic_info);
                if let Some(backtrace) = salsa::Backtrace::capture() {
                    eprintln!("{backtrace:#}");
                }
                DbPanicContext::with_ctx(|ctx| {
                    if !ctx.is_empty() {
                        eprintln!("additional context:");
                        for (idx, frame) in ctx.iter().enumerate() {
                            eprintln!("{idx:>4}: {frame}\n");
                        }
                    }
                });
            }));
        }

        static SET_HOOK: Once = Once::new();
        SET_HOOK.call_once(set_hook);

        Self::with_ctx(|ctx| ctx.push(frame));
        DbPanicContext
    }

    fn with_ctx(f: impl FnOnce(&mut Vec<String>)) {
        thread_local! {
            static CTX: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
        }
        CTX.with(|ctx| f(&mut ctx.borrow_mut()));
    }
}
