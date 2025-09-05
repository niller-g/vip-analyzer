use std::{fmt, path::Path};

use paths::{Utf8Path, Utf8PathBuf};
use walkdir::WalkDir;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProDir {
    /// The path to the ProDir folder.
    path: Utf8PathBuf,
    /// The path to the `node_modules` folder that contains the ProDir folder.
    /// It is common to download the ProDir folder as a dependency in a `node_modules` folder.
    node_modules: Option<Utf8PathBuf>,
}
impl ProDir {
    const BIN64_DIR: &'static str = "bin64";
    const NODE_MODULES_DIR: &'static str = "node_modules";

    /// Algorithm:
    /// 1) Find the closest node_modules folder. Firstly looking in the current directory, then looking up the file tree relative to the current directory.
    /// 2) If a `node_modules` folder is found, check if it contains a ProDir folder (max depth +2). Otherwise, continue looking up the file tree.
    /// 3) If no ProDir folder is found, look for any ProDir folder (max depth +3). Starting in the current directory and going up the file tree relative to the current directory.
    pub(crate) fn discover_pro_dir(look_from: &Utf8Path) -> Option<Self> {
        let mut current_dir: &Utf8Path =
            if look_from.is_dir() { look_from } else { look_from.parent()? };
        loop {
            let node_modules = current_dir.join(Self::NODE_MODULES_DIR);
            if node_modules.exists() {
                // Iterate through subdirectories of `node_modules` to find the ProDir folder.
                for entry in WalkDir::new(&node_modules).max_depth(2) {
                    let entry = entry.ok()?;
                    if Self::is_pro_dir(entry.path()) {
                        return Utf8PathBuf::from_path_buf(entry.path().to_path_buf())
                            .map(|path| ProDir { path, node_modules: Some(node_modules) })
                            .ok();
                    }
                }
            }

            current_dir = match current_dir.parent() {
                Some(p) => p,
                None => break,
            };
        }

        let mut current_dir: &Utf8Path = look_from;
        loop {
            for entry in WalkDir::new(current_dir).max_depth(3) {
                let entry = entry.ok()?;
                if Self::is_pro_dir(entry.path()) {
                    return Utf8PathBuf::from_path_buf(entry.path().to_path_buf())
                        .map(|path| ProDir { path, node_modules: None })
                        .ok();
                }
            }

            current_dir = match current_dir.parent() {
                Some(p) => p,
                None => break,
            };
        }

        None
    }

    pub(crate) fn find_exact_pro_dir<T: AsRef<str>>(
        pro_dir: &str,
        node_modules: Option<T>,
    ) -> Option<Self> {
        let pro_dir = Path::new(pro_dir);
        if Self::is_pro_dir(pro_dir) {
            let node_modules = match node_modules {
                Some(node_modules) => {
                    let node_modules = Utf8PathBuf::from(node_modules.as_ref());
                    node_modules
                        .exists()
                        .then_some(node_modules)
                        .or_else(|| Self::discover_node_modules(pro_dir, 3))
                }
                None => Self::discover_node_modules(pro_dir, 3),
            };

            Utf8PathBuf::from_path_buf(pro_dir.to_path_buf())
                .map(|path| ProDir { path, node_modules })
                .ok()
        } else {
            None
        }
    }

    /// Find the closest `node_modules` folder relative to the given path looking up the file tree.
    fn discover_node_modules(look_from: &Path, search_height: usize) -> Option<Utf8PathBuf> {
        let mut current_dir = look_from;
        for _ in 0..search_height {
            let node_modules = current_dir.join(Self::NODE_MODULES_DIR);
            if node_modules.exists() {
                return Utf8PathBuf::from_path_buf(node_modules).ok();
            }

            current_dir = match current_dir.parent() {
                Some(p) => p,
                None => break,
            };
        }

        None
    }

    /// A ProDir folder is a folder that contains a file at `appData\intelliSpeed.prodb`.
    /// I.e. `node_modules\@pdc_vip\visual-prolog\` is a ProDir folder if it contains `node_modules\@pdc_vip\visual-prolog\appData\intelliSpeed.prodb`.
    fn is_pro_dir(path: &Path) -> bool {
        path.join("appData/intelliSpeed.prodb").exists()
    }

    pub fn node_modules(&self) -> Option<&Utf8Path> {
        self.node_modules.as_deref()
    }

    pub fn has_node_modules(&self) -> bool {
        self.node_modules.is_some()
    }

    pub fn path(&self) -> &Utf8Path {
        &self.path
    }

    pub fn vip_builder_path(&self) -> Option<Utf8PathBuf> {
        let path = self.path.join(Self::BIN64_DIR).join("vipBuilder.exe");
        path.exists().then_some(path)
    }

    #[deprecated(note = "Use `vipfmt` instead.")]
    pub fn vip_pp_path(&self) -> Option<Utf8PathBuf> {
        let path = self.path.join(Self::BIN64_DIR).join("vipPP.exe");
        path.exists().then_some(path)
    }
}
impl fmt::Display for ProDir {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.path)
    }
}

#[test]
fn find_pro_dir() {
    if test_utils::project_root().join("visual-prolog/ProDir").exists() {
        let path = test_utils::project_root().join("visual-prolog/examples/dummyProject");
        let pro_dir = ProDir::discover_pro_dir(&path).unwrap();
        assert!(pro_dir.path().exists());
        assert!(pro_dir.vip_builder_path().unwrap().exists());
    }
}
