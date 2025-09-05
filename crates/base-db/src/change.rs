//! Defines a unit of change that can applied to the database to get the next
//! state. Changes are transactional.

use {
    crate::{RootQueryDb, input::ProjectsIdMap},
    std::fmt,
    vfs::VfsPath,
};

use salsa::Durability;
use vfs::FileId;

use crate::ProjectGraphBuilder;

/// Encapsulate a bunch of raw `.set` calls on the database.
#[derive(Default)]
pub struct FileChange {
    pub files_changed: Vec<(FileId, VfsPath, Option<String>)>,
    pub project_graph: Option<ProjectGraphBuilder>,
}

impl fmt::Debug for FileChange {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut d = fmt.debug_struct("Change");
        if !self.files_changed.is_empty() {
            d.field("files_changed", &self.files_changed.len());
        }
        if self.project_graph.is_some() {
            d.field("project_graph", &self.project_graph);
        }
        d.finish()
    }
}

impl FileChange {
    pub fn change_file(&mut self, file_id: FileId, path: VfsPath, new_text: Option<String>) {
        self.files_changed.push((file_id, path, new_text))
    }

    pub fn set_project_graph(&mut self, graph: ProjectGraphBuilder) {
        self.project_graph = Some(graph);
    }

    pub fn apply(self, db: &mut dyn RootQueryDb) -> Option<ProjectsIdMap> {
        let _p = tracing::info_span!("FileChange::apply").entered();
        for (file_id, path, text) in self.files_changed {
            // XXX: can't actually remove the file, just reset the text
            let text = text.unwrap_or_default();
            db.set_file_text_with_durability(file_id, path, &text, Durability::LOW);
        }

        if let Some(project_graph) = self.project_graph {
            return Some(project_graph.set_in_db(db));
        }
        None
    }
}
