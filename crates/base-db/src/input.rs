//! This module specifies the input to vip-analyzer. In some sense, this is
//! **the** most important module, because all other fancy stuff is strictly
//! derived from this input.
//!
//! Note that neither this module, nor any other part of the analyzer's core do
//! actual IO. See `vfs` and `project_model` in the `vip-analyzer` crate for how
//! actual IO is done and lowered to input.

use {
    crate::RootQueryDb,
    dashmap::mapref::entry::Entry,
    intern::Symbol,
    itertools::Itertools,
    la_arena::{Arena, Idx},
    rustc_hash::{FxBuildHasher, FxHashMap},
    salsa::{Durability, Setter},
    std::{fmt, ops},
    triomphe::Arc,
    vfs::{FileId, VfsPath},
};

type FxIndexSet<T> = indexmap::IndexSet<T, FxBuildHasher>;

#[derive(Clone, Default)]
pub struct ProjectGraphBuilder {
    arena: Arena<ProjectBuilder>,
}

pub type ProjectBuilderId = Idx<ProjectBuilder>;

impl ops::Index<ProjectBuilderId> for ProjectGraphBuilder {
    type Output = ProjectBuilder;

    fn index(&self, index: ProjectBuilderId) -> &Self::Output {
        &self.arena[index]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectBuilder {
    pub basic: ProjectData,
    pub extra: ExtraProjectData,
}

impl fmt::Debug for ProjectGraphBuilder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.arena.iter().map(|(id, data)| (u32::from(id.into_raw()), data)))
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UniqueProjectData {
    root_file_id: FileId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProjectData {
    /// The project *.vipprj file
    manifest_id: FileId,
}
impl ProjectData {
    pub fn new(manifest_id: FileId) -> ProjectData {
        ProjectData { manifest_id }
    }

    pub fn manifest_id(&self) -> &FileId {
        &self.manifest_id
    }
}

/// Project data unrelated to to its identity.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExtraProjectData {
    pub project_name: Symbol,
    /// The directory of the project. Usually the directory containing the `*.vipprj`.
    project_root: VfsPath,
    /// The included paths (in the `*.vipprj` file) in the project.
    included_paths: IncludedPaths<VfsPath>,
    /// The packages listed in the project `*.vipprj` manifest.
    packages: Vec<FileId>,
}
impl ExtraProjectData {
    pub fn new(
        project_name: &str,
        project_root: VfsPath,
        included_paths: IncludedPaths<VfsPath>,
        packages: Vec<FileId>,
    ) -> ExtraProjectData {
        ExtraProjectData {
            project_name: Symbol::intern(project_name),
            project_root,
            included_paths,
            packages,
        }
    }

    pub fn project_root(&self) -> &VfsPath {
        &self.project_root
    }

    pub fn included_paths(&self) -> impl Iterator<Item = &VfsPath> + '_ {
        self.included_paths.iter()
    }

    pub fn packages(&self) -> impl Iterator<Item = &FileId> + '_ {
        self.packages.iter()
    }
}

/// A number determining the precedence of an included folder.
/// Lower numbers have precedence over higher numbers. I.e. they should be considered first.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IncludedPathPrecedence(usize);
impl From<usize> for IncludedPathPrecedence {
    fn from(value: usize) -> Self {
        IncludedPathPrecedence(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncludedPaths<P: Eq> {
    paths: Vec<(P, IncludedPathPrecedence)>,
}
impl<P: std::cmp::Eq + std::hash::Hash> IncludedPaths<P> {
    pub fn new(paths: Vec<(P, IncludedPathPrecedence)>) -> IncludedPaths<P> {
        IncludedPaths { paths }
    }

    pub fn iter(&self) -> impl Iterator<Item = &P> {
        self.paths
            .iter()
            .unique_by(|(p, _)| p)
            .sorted_by(|(_, p1), (_, p2)| p1.cmp(p2))
            .map(|(p, _)| p)
    }

    pub fn map<M: std::cmp::Eq + std::hash::Hash>(self, f: impl Fn(P) -> M) -> IncludedPaths<M> {
        IncludedPaths::new(self.paths.into_iter().map(|(p, prec)| (f(p), prec)).collect())
    }
}

pub type ProjectsIdMap = FxHashMap<ProjectBuilderId, Project>;

#[salsa_macros::input]
#[derive(Debug)]
pub struct Project {
    #[return_ref]
    pub data: ProjectData,
    /// Project data that is not needed for analysis.
    ///
    /// This is split into a separate field to increase incrementality.
    #[return_ref]
    pub extra_data: ExtraProjectData,
}

impl ProjectGraphBuilder {
    pub fn add_project_root(
        &mut self,
        basic: ProjectData,
        extra: ExtraProjectData,
    ) -> ProjectBuilderId {
        self.arena.alloc(ProjectBuilder { basic, extra })
    }

    pub fn set_in_db(self, db: &mut dyn RootQueryDb) -> ProjectsIdMap {
        let mut all_projects =
            FxIndexSet::with_capacity_and_hasher(self.arena.len(), FxBuildHasher);
        let mut visited = FxHashMap::default();
        let old_all_projects = db.all_projects();

        let projects_map = db.projects_map();
        for (project_id, project) in self.arena.into_iter() {
            if visited.contains_key(&project_id) {
                continue;
            }
            let unique_project_data = UniqueProjectData { root_file_id: project.basic.manifest_id };
            let project_input = match projects_map.0.entry(unique_project_data) {
                Entry::Occupied(entry) => {
                    let old_project = *entry.get();
                    if project.basic != *old_project.data(db) {
                        old_project
                            .set_data(db)
                            .with_durability(Durability::MEDIUM)
                            .to(project.basic);
                    }
                    if project.extra != *old_project.extra_data(db) {
                        old_project
                            .set_extra_data(db)
                            .with_durability(Durability::MEDIUM)
                            .to(project.extra);
                    }
                    old_project
                }
                Entry::Vacant(entry) => {
                    let input = Project::builder(project.basic, project.extra)
                        .durability(Durability::MEDIUM)
                        .new(db);
                    entry.insert(input);
                    input
                }
            };
            all_projects.insert(project_input);
            visited.insert(project_id, project_input);
        }

        if old_all_projects.len() != all_projects.len()
            || old_all_projects.iter().any(|&project| all_projects.contains(&project))
        {
            db.set_all_projects_with_durability(
                Arc::new(Vec::from_iter(all_projects).into_boxed_slice()),
                Durability::MEDIUM,
            );
        }

        visited
    }

    pub fn iter(&self) -> impl Iterator<Item = ProjectBuilderId> + '_ {
        self.arena.iter().map(|(idx, _)| idx)
    }

    pub fn shrink_to_fit(&mut self) {
        self.arena.shrink_to_fit();
    }
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Env {
    entries: FxHashMap<String, String>,
}
impl Extend<(String, String)> for Env {
    fn extend<T: IntoIterator<Item = (String, String)>>(&mut self, iter: T) {
        self.entries.extend(iter);
    }
}

impl FromIterator<(String, String)> for Env {
    fn from_iter<T: IntoIterator<Item = (String, String)>>(iter: T) -> Self {
        Env { entries: FromIterator::from_iter(iter) }
    }
}

impl Env {
    pub fn set(&mut self, env: &str, value: impl Into<String>) {
        self.entries.insert(env.to_owned(), value.into());
    }

    pub fn get(&self, env: &str) -> Option<String> {
        self.entries.get(env).cloned()
    }

    pub fn extend_from_other(&mut self, other: &Env) {
        self.entries.extend(other.entries.iter().map(|(x, y)| (x.to_owned(), y.to_owned())));
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn insert(&mut self, k: impl Into<String>, v: impl Into<String>) -> Option<String> {
        self.entries.insert(k.into(), v.into())
    }
}

impl From<Env> for Vec<(String, String)> {
    fn from(env: Env) -> Vec<(String, String)> {
        let mut entries: Vec<_> = env.entries.into_iter().collect();
        entries.sort();
        entries
    }
}

impl<'a> IntoIterator for &'a Env {
    type Item = (&'a String, &'a String);
    type IntoIter = std::collections::hash_map::Iter<'a, String, String>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.iter()
    }
}

impl fmt::Debug for Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct EnvDebug<'s>(Vec<(&'s String, &'s String)>);

        impl fmt::Debug for EnvDebug<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_map().entries(self.0.iter().copied()).finish()
            }
        }
        f.debug_struct("Env")
            .field("entries", &{
                let mut entries: Vec<_> = self.entries.iter().collect();
                entries.sort();
                EnvDebug(entries)
            })
            .finish()
    }
}
