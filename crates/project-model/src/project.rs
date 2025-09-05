pub(crate) mod project_files;

use base_db::{IncludedPaths, ProjectData, absolutize_project_path};
use paths::{AbsPath, AbsPathBuf, Utf8PathBuf};
use rustc_hash::FxHashMap;
use serde::Deserialize;
use std::{fs, path::PathBuf};
use vfs::VfsPath;
use {
    crate::{ManifestPath, pro_dir::ProDir},
    vfs::FileId,
};
use {anyhow::bail, base_db::ExtraProjectData};

pub type FileLoader<'a> = &'a mut dyn for<'b> FnMut(&'b AbsPath) -> Option<FileId>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IdeVariables {
    map: FxHashMap<ManifestPath, ManifestVars>,
}
impl IdeVariables {
    pub fn new(map: FxHashMap<ManifestPath, ManifestVars>) -> IdeVariables {
        IdeVariables { map }
    }

    pub fn get(&self, manifest: &ManifestPath, key: &str) -> Option<&str> {
        self.map.get(manifest)?.get(key).map(String::as_str)
    }

    pub fn manifest_vars(&self, manifest: &ManifestPath) -> Option<&ManifestVars> {
        self.map.get(manifest)
    }
}
pub type ManifestVars = FxHashMap<String, String>;
const PRO_DIR: &str = "ProDir";
const PRO_DIR_LOWER: &str = "prodir";
const NODE_MODULES: &str = "node_modules";

/// The data related to a VIP project.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Project {
    manifest_path: ManifestPath,
    meta_data: ProjectMetaData,
    manifest_vars: ManifestVars,
    pro_dir: ProDir,
}
impl Project {
    pub fn load(
        manifest_path: ManifestPath,
        manifest_vars: ManifestVars,
        progress: &dyn Fn(String),
    ) -> anyhow::Result<Project> {
        progress(manifest_path.to_string());
        let vipprj = fs::read_to_string(manifest_path.to_path_buf())?;

        let Ok(meta_data) = ProjectMetaData::try_from(vipprj) else {
            bail!("Failed to parse project data")
        };

        progress("Finding ProDir".to_owned());
        let pro_dir =
            // Honor if variables are explicitly set, otherwise try to infer them
            match manifest_vars.get(PRO_DIR).or_else(|| manifest_vars.get(PRO_DIR_LOWER)) {
                Some(pro_dir) => {
                    ProDir::find_exact_pro_dir(pro_dir, manifest_vars.get(NODE_MODULES))
                        .or_else(|| ProDir::discover_pro_dir(manifest_path.parent().as_ref()))
                }
                None => ProDir::discover_pro_dir(manifest_path.parent().as_ref()),
            }
            .ok_or_else(|| anyhow::format_err!("Could not find ProDir folder"))?;

        let project = Project { manifest_path, meta_data, manifest_vars, pro_dir };

        Ok(project)
    }

    /// The files which are added to the *.vipprj file.
    /// This usually is *.pack files.
    pub(crate) fn packages(&self) -> impl Iterator<Item = AbsPathBuf> + '_ {
        self.meta_data
            .item_set
            .items
            .iter()
            .filter(|item| item.filename.path.ends_with(".pack"))
            .filter_map(|item| self.resolve_path(&item.filename))
    }

    /// The folders which are included in the project.
    /// These folders are the ones which are discoverable through include directives such as `#include @"file.ph"`.
    /// The return type contains the index of the folder and the path to the folder. Lower indices have precedence.
    pub(crate) fn included_paths(&self) -> IncludedPaths<AbsPathBuf> {
        let included =
            self.meta_data.include_directories.directories.iter().enumerate().filter_map(
                move |(i, dir)| {
                    self.expand_path_variable(dir)
                        .and_then(|p| self.relative_project_path(p.into()))
                        .map(|p| (p, i.into()))
                },
            );

        IncludedPaths::new(included.collect())
    }

    fn relative_project_path(&self, path: PathBuf) -> Option<AbsPathBuf> {
        let path = Utf8PathBuf::try_from(path).ok()?;
        if path.is_absolute() {
            AbsPathBuf::try_from(path).ok().and_then(|p| std::fs::metadata(&p).is_ok().then_some(p))
        } else {
            let path = self.project_root().absolutize(&path);
            std::fs::metadata(&path).is_ok().then_some(path)
        }
    }

    pub fn manifest_path(&self) -> &ManifestPath {
        &self.manifest_path
    }

    pub fn project_root(&self) -> &AbsPath {
        self.manifest_path.parent()
    }

    pub fn pro_dir(&self) -> &ProDir {
        &self.pro_dir
    }

    pub(crate) fn project_sibling_excludes(&self) -> Vec<AbsPathBuf> {
        let directories = [
            &self.meta_data.debug_directory,
            &self.meta_data.final_directory,
            &self.meta_data.intermediate_directory,
            &self.meta_data.library_directory,
        ];

        let mut excludes = vec![self.project_root().join(".git")];
        for dir in directories.into_iter().filter_map(|d| d.as_ref()) {
            if let Some(mut resolved_dir) = self.resolve_path(dir) {
                excludes.push(resolved_dir.clone());

                // Include the same directory with "64" appended
                if let Some(parent) = resolved_dir.parent() {
                    resolved_dir = parent.join(format!("{}64", resolved_dir.file_name().unwrap()));
                    excludes.push(resolved_dir);
                }
            }
        }
        excludes
    }

    pub fn resolve_path(&self, var_path: &VariablePath) -> Option<AbsPathBuf> {
        Self::expand_path_variable(self, var_path).and_then(|p| {
            absolutize_project_path(
                p,
                self.included_paths().iter().map(|p| p.as_path()),
                self.project_root(),
            )
        })
    }

    /// Returns the expanded path of a variable path.
    /// If the variable path contains a variable, the variable is expanded.
    /// If the variable path does not contain a variable, the path is returned as is.
    /// If the variable path contains a variable which is not found in the manifest variables, [`None`] is returned.
    fn expand_path_variable(&self, var_path: &VariablePath) -> Option<String> {
        if let Some(variable) = &var_path.variable {
            let mut expanded_var: String = self
                .manifest_vars
                .get(variable)
                .map(|v| v.into())
                // Honor if variables are explicitly set, otherwise try to infer them
                .or_else(|| match variable.to_lowercase().as_str() {
                    PRO_DIR_LOWER => Some(self.pro_dir.path().to_string()),
                    NODE_MODULES => self.pro_dir.node_modules().map(|p| p.to_string()),
                    _ => None,
                })?;

            if !expanded_var.ends_with('/')
                && !expanded_var.ends_with('\\')
                && !var_path.path.starts_with('/')
                && !var_path.path.starts_with('\\')
            {
                expanded_var.push(std::path::MAIN_SEPARATOR);
            }

            expanded_var.push_str(&var_path.path);

            Some(expanded_var)
        } else {
            Some(var_path.path.clone())
        }
    }

    pub fn as_project_data(&self, load: FileLoader<'_>) -> Option<(ProjectData, ExtraProjectData)> {
        let manifest_id = load(self.manifest_path.path())?;
        Some((
            ProjectData::new(manifest_id),
            ExtraProjectData::new(
                self.manifest_path.derive_name(),
                VfsPath::from(self.project_root().to_path_buf()),
                self.included_paths().map(|path| VfsPath::from(path.clone())),
                self.packages().filter_map(|p| load(&p)).collect::<Vec<_>>(),
            ),
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariablePath {
    variable: Option<String>,
    path: String,
}
impl VariablePath {
    fn new(var_path: String) -> VariablePath {
        match Self::strip_path_var(&var_path) {
            Some((var, path)) => {
                VariablePath { variable: Some(var.to_owned()), path: path.to_owned() }
            }
            None => VariablePath { variable: None, path: var_path },
        }
    }

    /// try to strip prefixes like `$(ProDir)` or `$(node_modules)` where the library name can be arbitrary
    fn strip_path_var(path: &str) -> Option<(&str, &str)> {
        if path.len() < 2 {
            return None;
        }

        if &path[..2] == "$(" {
            let end = path[2..].find(')').map(|end| 2 + end)?;

            Some((&path[2..end], &path[end + 1..]))
        } else {
            None
        }
    }
}
impl From<&str> for VariablePath {
    fn from(path: &str) -> Self {
        VariablePath::new(path.to_owned())
    }
}
impl std::fmt::Display for VariablePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(var) = &self.variable {
            write!(f, "$({}){}", var, self.path)
        } else {
            write!(f, "{}", self.path)
        }
    }
}
impl<'de> serde::Deserialize<'de> for VariablePath {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let var_path = String::deserialize(deserializer)?;
        Ok(VariablePath::new(var_path))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename = "projectData-map")]
struct ProjectMetaData {
    pub(crate) debug_directory: Option<VariablePath>,
    pub(crate) final_directory: Option<VariablePath>,
    pub(crate) intermediate_directory: Option<VariablePath>,
    pub(crate) library_directory: Option<VariablePath>,
    #[serde(rename = "include_directory-list")]
    include_directories: IncludeDirectoryList,
    #[serde(rename = "item-set")]
    item_set: Items,
}

impl TryFrom<String> for ProjectMetaData {
    type Error = serde_xml_rs::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        serde_xml_rs::from_str(&value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct IncludeDirectoryList {
    #[serde(rename = "include_directory")]
    pub(crate) directories: Vec<VariablePath>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct Items {
    #[serde(rename = "item-map")]
    pub(crate) items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct Item {
    filename: VariablePath,
    resource_id: Option<String>,
}

#[cfg(test)]
mod tests {
    use expect_test::expect_file;
    use {super::*, crate::WorkspaceFolders};

    const MANIFEST: &str = r#"<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<projectData-map additionalCompilerOptions="/w:652+ /warn:667+ /warn:231+ /w:665+" auto-formatting="true" debug_directory="deb" description="Visual Prolog Compiler" final_directory="..\..\..\ProDir\Bin2" intermediate_directory="obj" library_directory="Lib" pattern="Console application" rules="Console" version="6" vipId="&lt;&lt;\ProDir\">
  <include_directory-list>
    <include_directory>.</include_directory>
    <include_directory>..\..</include_directory>
    <include_directory>..\..\commonTools\</include_directory>
    <include_directory>..\..\..\ProDir</include_directory>
  </include_directory-list>
  <item-set key="filename">
    <item-map filename="$(ProDir)pfc\cryptography\cryptography.pack" />
    <item-map filename="$(ProDir)pfc\web\json\json.pack" />
    <item-map filename="$(ProDir)pfc\windowsApi\com_api\standardInterfaces\import\standardInterfaces_import.pack" />
    <item-map filename="..\..\..\ProDir\lib\boostRegex.lib" />
    <item-map filename="..\..\..\ProDir\pfc\environment\environment.pack" />
    <item-map filename="..\..\..\ProDir\pfc\event\event.pack" />
    <item-map filename="..\..\..\ProDir\vip\cpuRegisters\cpuRegisters.pack" />
    <item-map filename="cutsRewrite\cutsRewrite.pack" />
    <item-map filename="y2eTranslator\y2yEmbeddedSyntax\y2yEmbeddedSyntax.pack" />
    <item-map filename="yTreeChecker\yTreeChecker.pack" />
    <item-map filename="..\..\_icons\visual.ico" resource_id="application_icon" />
    <item-map filename="..\..\commonTools\version\vipVersion.version" resource_id="idm_76" />
  </item-set>
  <setting-set key="target">
    <setting-map target="Win32">
      <var-list>
        <var-map name="Target.Machine" value="x86" />
        <var-map name="Target.DirPrefix" />
        <var-map name="Target.LibPrefix" />
        <var-map name="Target.StackSize" value="2097152" />
      </var-list>
    </setting-map>
    <setting-map target="x64">
      <var-list>
        <var-map name="Target.Machine" value="x64" />
        <var-map name="Target.DirPrefix" value="64" />
        <var-map name="Target.LibPrefix" value=".x64" />
        <var-map name="Target.StackSize" value="3145728" />
      </var-list>
    </setting-map>
  </setting-set>
</projectData-map>
"#;

    #[test]
    fn test_project_meta_data_1() {
        let meta_data = ProjectMetaData::try_from(MANIFEST.to_owned()).unwrap();
        expect_file![r#"../test_data/project_meta_data_1.txt"#]
            .assert_eq(&format!("{:#?}", meta_data));
    }

    #[test]
    fn test_project_meta_data_2() {
        let file = test_utils::project_root()
            .join("visual-prolog/examples/dummyProject/dummyProject.vipprj");

        let meta_data = ProjectMetaData::try_from(fs::read_to_string(file).unwrap()).unwrap();

        expect_file![r#"../test_data/project_meta_data_2.txt"#]
            .assert_eq(&format!("{:#?}", meta_data));
    }

    #[test]
    fn find_lib() {
        let path = "src/workspace.rs".into();
        let var_path = VariablePath::new(path);
        assert!(var_path.variable.is_none());
        assert_eq!(var_path.path, "src/workspace.rs");

        let path = "$(ProDir)src/workspace.rs".into();
        let var_path = VariablePath::new(path);
        assert_eq!(var_path.variable, Some("ProDir".into()));
        assert_eq!(var_path.path, "src/workspace.rs");

        let path = "$(node_modules)src/workspace.rs".into();
        let var_path = VariablePath::new(path);
        assert_eq!(var_path.variable, Some("node_modules".into()));
        assert_eq!(var_path.path, "src/workspace.rs");

        let path = "Lib".into();
        let var_path = VariablePath::new(path);
        assert_eq!(var_path.variable, None);
        assert_eq!(var_path.path, "Lib");
    }

    #[test]
    fn xml() {
        let file: AbsPathBuf = AbsPathBuf::try_from(
            test_utils::project_root()
                .join("visual-prolog/examples/dummyProject/dummyProject.vipprj"),
        )
        .unwrap();

        let manifest = ManifestPath::try_from(file).unwrap();
        let mut manifest_vars = ManifestVars::default();
        manifest_vars.insert("ProDir".into(), "../../ProDir".into());

        if test_utils::project_root().join("visual-prolog/ProDir").exists() {
            let workspace = Project::load(manifest, manifest_vars, &|_| {}).unwrap();
            workspace.resolve_path(&VariablePath::new("$(ProDir)vip".into())).unwrap();
            WorkspaceFolders::all_directories(&[workspace]).for_each(|path| {
                fs::metadata(path).unwrap();
            });
        }
    }
}
