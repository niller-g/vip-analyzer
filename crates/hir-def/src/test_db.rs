//! Database used for testing `hir_def`.

use {
    base_db::{FileText, Project, ProjectGraphBuilder, ProjectsMap, RootQueryDb, SourceDatabase},
    core::{fmt, panic},
    salsa::{AsDynDatabase, Durability},
    std::sync::Mutex,
    triomphe::Arc,
};

#[salsa_macros::db]
#[derive(Clone)]
pub(crate) struct TestDB {
    storage: salsa::Storage<Self>,
    files: Arc<base_db::Files>,
    projects_map: Arc<ProjectsMap>,
    events: Arc<Mutex<Option<Vec<salsa::Event>>>>,
}

impl Default for TestDB {
    fn default() -> Self {
        let mut this = Self {
            storage: Default::default(),
            files: Default::default(),
            projects_map: Default::default(),
            events: Default::default(),
        };
        this.set_all_projects(Arc::new(Box::new([])));
        ProjectGraphBuilder::default().set_in_db(&mut this);
        this
    }
}

#[salsa_macros::db]
impl salsa::Database for TestDB {
    fn salsa_event(&self, event: &dyn std::ops::Fn() -> salsa::Event) {
        let mut events = self.events.lock().unwrap();
        if let Some(events) = &mut *events {
            let event = event();
            events.push(event);
        }
    }
}

impl fmt::Debug for TestDB {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TestDB").finish()
    }
}

impl panic::RefUnwindSafe for TestDB {}

#[salsa_macros::db]
impl SourceDatabase for TestDB {
    fn file_text(&self, file_id: base_db::FileId) -> FileText {
        self.files.file_text(file_id)
    }

    fn set_file_text(&mut self, file_id: base_db::FileId, path: base_db::VfsPath, text: &str) {
        let files = Arc::clone(&self.files);
        files.set_file_text(self, file_id, path, text);
    }

    fn set_file_text_with_durability(
        &mut self,
        file_id: base_db::FileId,
        path: base_db::VfsPath,
        text: &str,
        durability: Durability,
    ) {
        let files = Arc::clone(&self.files);
        files.set_file_text_with_durability(self, file_id, path, text, durability);
    }

    fn file_for_path(&self, path: base_db::VfsPath) -> Option<base_db::FileId> {
        self.files.file_for_path(path)
    }

    fn projects_map(&self) -> Arc<ProjectsMap> {
        self.projects_map.clone()
    }
}

impl TestDB {
    pub(crate) fn fetch_test_project(&self) -> Project {
        let all_projects = self.all_projects();
        all_projects
            .iter()
            .find(|&project| project.extra_data(self).project_name.as_str() == "vs_test_fixture")
            .cloned()
            .unwrap_or(all_projects[0])
    }

    pub(crate) fn log(&self, f: impl FnOnce()) -> Vec<salsa::Event> {
        *self.events.lock().unwrap() = Some(Vec::new());
        f();
        self.events.lock().unwrap().take().unwrap()
    }

    pub(crate) fn log_executed(&self, f: impl FnOnce()) -> Vec<String> {
        let events = self.log(f);
        events
            .into_iter()
            .filter_map(|e| match e.kind {
                // This is pretty horrible, but `Debug` is the only way to inspect
                // QueryDescriptor at the moment.
                salsa::EventKind::WillExecute { database_key } => {
                    let ingredient = self
                        .as_dyn_database()
                        .ingredient_debug_name(database_key.ingredient_index());
                    Some(ingredient.to_string())
                }
                _ => None,
            })
            .collect()
    }
}
