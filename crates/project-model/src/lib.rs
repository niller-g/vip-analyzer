//! Roughly, the things we do here are:
//!
//! * Project discovery (where's the relevant *.vipprj for the current dir).
//! * Obtaining the dta in the *.vipprj xml file.
//! * Storing IDE variables
//! * Deduplicating source roots when multiple projects are opened in the same workspace.

mod manifest_path;
mod pro_dir;
mod project;
mod workspace;

pub use crate::{
    manifest_path::ManifestPath,
    pro_dir::ProDir,
    project::{
        FileLoader, IdeVariables, ManifestVars, Project, VariablePath, project_files::ProjectFiles,
    },
    workspace::{WorkspaceFolders, load_workspace_at},
};
