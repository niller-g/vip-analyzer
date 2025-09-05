use syntax::ast::{self, HasItems, HasStringSeq};
use vfs::AbsPath;
use {super::Project, base_db::absolutize_project_path, ide_db::FxHashSet, vfs::FileId};
use {base_db::RootQueryDb, vfs::AbsPathBuf};

// #[derive(Clone)]
#[derive(Default)]
pub struct ProjectFiles {
    /// All the files that are part of the project.
    pub files: FxHashSet<FileId>,
}

impl ProjectFiles {
    #[inline]
    pub fn load_files(
        db: &dyn RootQueryDb,
        project: &Project,
        load: &mut impl FnMut(&AbsPath) -> FileId,
    ) -> FxHashSet<FileId> {
        Self::load_files_with(db, project, load, |_| {})
    }

    pub fn load_files_with(
        db: &dyn RootQueryDb,
        project: &Project,
        load: &mut impl FnMut(&AbsPath) -> FileId,
        write_err: impl Fn(String),
    ) -> FxHashSet<FileId> {
        let dirs: Vec<_> = project.included_paths().iter().cloned().collect();
        let project_root = project.project_root();

        let mut project_files = ProjectFiles::default();

        for package in project.packages() {
            let file_id = load(&package);
            if project_files.files.insert(file_id) {
                project_files.walk_file(db, &dirs, project_root, &package, load, &write_err);
            }
        }

        project_files.files
    }

    fn walk_file(
        &mut self,
        db: &dyn RootQueryDb,
        included_dirs: &Vec<AbsPathBuf>,
        project_root: &AbsPath,
        item_path: &AbsPath,
        load: &mut impl FnMut(&AbsPath) -> FileId,
        write_err: &impl Fn(String),
    ) {
        let file_id = load(item_path);
        let source_file: syntax::SourceFile = db.parse(file_id).tree();

        for item in source_file.items_recursively() {
            let directive_path = match item {
                ast::Item::IncludeItem(include) => match include.string_value() {
                    Some(path) => path,
                    None => continue,
                },
                _ => continue,
            };

            let item_path = match absolutize_project_path(
                &directive_path,
                included_dirs.iter().map(|p| p.as_path()),
                project_root,
            ) {
                Some(item_path) => item_path,
                None => {
                    let msg = format!("Failed to find project path for: '{directive_path}'");
                    self.error_msg(msg, write_err);
                    continue;
                }
            };

            let file_id = load(&item_path);
            if self.files.insert(file_id) {
                self.walk_file(db, included_dirs, project_root, &item_path, load, write_err);
            }
        }
    }

    #[inline]
    fn error_msg(&self, msg: String, err_msg: &impl Fn(String)) {
        tracing::error!(msg);
        err_msg(msg);
    }
}
