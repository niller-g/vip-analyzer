//! Loads a VIP project into a static instance of analysis, without support
//! for incorporating changes.

use crate::{ManifestPath, Project};
use base_db::FileChange;
use crossbeam_channel::unbounded;
use ide_db::RootDatabase;
use itertools::Itertools;
use std::{borrow::Cow, path::Path};
use vfs::{
    AbsPath, AbsPathBuf,
    loader::{Handle, LoadingProgress},
};

#[derive(Default)]
pub struct WorkspaceFolders {
    pub load: Vec<vfs::loader::Entry>,
    pub watch: Vec<usize>,
}

impl WorkspaceFolders {
    fn insert(&mut self, entry: vfs::loader::Entry) {
        self.watch.push(self.load.len());
        self.load.push(entry);
    }

    pub fn all_directories(projects: &[Project]) -> impl Iterator<Item = AbsPathBuf> {
        projects
            .iter()
            .flat_map(|project| project.included_paths().iter().cloned().collect::<Vec<_>>())
            .unique()
            .sorted()
    }

    pub fn new(projects: &[Project]) -> WorkspaceFolders {
        let mut res = WorkspaceFolders::default();
        let vip_exts = vec![
            "pro".into(),
            "i".into(),
            "cl".into(),
            "ph".into(),
            "pack".into(),
            "vipprj".into(),
        ];

        let excludes =
            projects.iter().flat_map(|p| p.project_sibling_excludes()).collect::<Vec<_>>();

        for ws_dir in Self::all_directories(projects) {
            let dirs = vfs::loader::Directories {
                extensions: vip_exts.clone(),
                include: vec![ws_dir],
                exclude: excludes.clone(),
            };
            res.insert(vfs::loader::Entry::Directories(dirs));
        }

        #[cfg(debug_assertions)]
        {
            for idx in res.watch.iter() {
                assert!(*idx < res.load.len());
            }
        }

        res
    }
}

/// Loads a workspace at the given path.
/// Mainly used for testing purposes.
pub fn load_workspace_at(
    root: &Path,
    progress: &dyn Fn(String),
) -> anyhow::Result<(RootDatabase, vfs::Vfs)> {
    let root = AbsPathBuf::assert_utf8(std::env::current_dir()?.join(root));
    let root = ManifestPath::discover_single(&root)?;
    let project = Project::load(root, Default::default(), progress)?;

    let (sender, receiver) = unbounded();
    let mut vfs = vfs::Vfs::default();
    let mut loader = {
        let loader = vfs_notify::NotifyHandle::spawn(sender);
        Box::new(loader)
    };

    let mut project_graph = base_db::ProjectGraphBuilder::default();
    let (project_data, extra_data) = project
        .as_project_data(&mut |path: &AbsPath| {
            let contents = loader.load_sync(path);
            let path = vfs::VfsPath::from(path.to_path_buf());
            vfs.set_file_contents(path.clone(), contents);
            vfs.file_id(&path)
        })
        .ok_or_else(|| anyhow::anyhow!("Failed to load project data"))?;
    project_graph.add_project_root(project_data, extra_data);
    project_graph.shrink_to_fit();

    let project_folders = WorkspaceFolders::new(&[project]);
    loader.set_config(vfs::loader::Config {
        load: project_folders.load,
        watch: vec![], // FIXME: nothing here since we only support that the client does the watching
        version: 0,
    });

    let mut db = RootDatabase::new(None);
    let mut analysis_change = FileChange::default();

    // wait until Vfs has loaded all roots
    for task in receiver {
        match task {
            vfs::loader::Message::Progress { n_done, .. } => {
                if n_done == LoadingProgress::Finished {
                    break;
                }
            }
            vfs::loader::Message::Loaded { files } | vfs::loader::Message::Changed { files } => {
                let _p =
                    tracing::info_span!("workspace::load_workspace_at/LoadedChanged").entered();
                for (path, contents) in files {
                    vfs.set_file_contents(path.into(), contents);
                }
            }
        }
    }
    let changes = vfs.take_changes();
    for (_, file) in changes {
        if let vfs::Change::Create(v, _) | vfs::Change::Modify(v, _) = file.change {
            analysis_change.change_file(
                file.file_id,
                vfs.file_path(file.file_id).clone(),
                stdx::to_utf8_else_utf16(&v, true).ok().map(Cow::into_owned),
            );
        }
    }
    analysis_change.set_project_graph(project_graph);
    db.apply_change(analysis_change);

    Ok((db, vfs))
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        base_db::RootQueryDb,
        test_utils::{project_root, skip_slow_tests},
    };

    #[test]
    fn load_example_project() {
        if skip_slow_tests() || !test_utils::project_root().join("visual-prolog/ProDir").exists() {
            return;
        }

        let (db, _) = load_workspace_at(
            project_root().join("visual-prolog/examples/dummyProject").as_ref(),
            &|_| {},
        )
        .unwrap();
        assert_eq!(db.all_projects().len(), 1);
    }
}
