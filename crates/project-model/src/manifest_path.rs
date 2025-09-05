//! See [`ManifestPath`].
use std::{
    borrow::Borrow,
    fmt,
    fs::{ReadDir, read_dir},
    io, ops,
    path::PathBuf,
};

use anyhow::bail;
use paths::{AbsPath, AbsPathBuf, Utf8Path, Utf8PathBuf};
use rustc_hash::FxHashSet;
use serde::{Deserialize, Serialize};

/// More or less [`AbsPathBuf`] with non-None parent.
///
/// We use it to store path to Cargo.toml, as we frequently use the parent dir
/// as a working directory to spawn various commands, and its nice to not have
/// to `.unwrap()` everywhere.
///
/// This could have been named `AbsNonRootPathBuf`, as we don't enforce that
/// this stores manifest files in particular, but we only use this for manifests
/// at the moment in practice.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub struct ManifestPath {
    #[serde(serialize_with = "serialize_abs_pathbuf")]
    #[serde(deserialize_with = "deserialize_abs_pathbuf")]
    file: AbsPathBuf,
}
fn deserialize_abs_pathbuf<'de, D>(de: D) -> std::result::Result<AbsPathBuf, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    let path = String::deserialize(de)?;

    AbsPathBuf::try_from(path.as_ref())
        .map_err(|err| serde::de::Error::custom(format!("invalid path name: {err:?}")))
}
fn serialize_abs_pathbuf<S>(path: &AbsPathBuf, se: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let path: &Utf8Path = path.as_ref();
    se.serialize_str(path.as_str())
}

impl TryFrom<AbsPathBuf> for ManifestPath {
    type Error = ();

    fn try_from(file: AbsPathBuf) -> Result<Self, Self::Error> {
        if !Self::is_vip_manifest(&file) {
            return Err(());
        }
        if file.parent().is_none() || file.file_stem().is_none() {
            Err(())
        } else {
            Ok(ManifestPath { file })
        }
    }
}
impl TryFrom<&AbsPath> for ManifestPath {
    type Error = ();

    fn try_from(file: &AbsPath) -> Result<Self, Self::Error> {
        ManifestPath::try_from(file.to_path_buf())
    }
}
impl TryFrom<PathBuf> for ManifestPath {
    type Error = ();

    fn try_from(file: PathBuf) -> Result<Self, Self::Error> {
        let utf8 = Utf8PathBuf::from_path_buf(file).map_err(|_| ())?;
        let abs = AbsPathBuf::try_from(utf8).map_err(|_| ())?;
        ManifestPath::try_from(abs)
    }
}

const MANIFEST_EXT: &str = "vipprj";

impl ManifestPath {
    // Shadow `parent` from `Deref`.
    pub fn parent(&self) -> &AbsPath {
        self.file.parent().unwrap()
    }

    pub fn canonicalize(&self) -> ! {
        (**self).canonicalize()
    }

    pub fn path(&self) -> &AbsPath {
        &self.file
    }

    pub fn derive_name(&self) -> &str {
        self.file.file_stem().unwrap()
    }

    pub fn is_vip_manifest(file: &AbsPathBuf) -> bool {
        file.extension() == Some(MANIFEST_EXT)
    }

    pub fn discover_single(path: &AbsPath) -> anyhow::Result<ManifestPath> {
        let mut candidates = ManifestPath::discover(path)?;
        let res = match candidates.pop() {
            None => bail!("no projects"),
            Some(it) => it,
        };

        if !candidates.is_empty() {
            bail!("more than one project");
        }
        Ok(res)
    }

    pub fn discover(path: &AbsPath) -> io::Result<Vec<ManifestPath>> {
        return {
            match ManifestPath::try_from(path) {
                Ok(it) => Ok(vec![it]),
                Err(_) => Ok(find_in_child_dir(read_dir(path)?, 3)),
            }
        };

        /// Look for *.vipprj files in the directory and children up to a certain depth.
        fn find_in_child_dir(entities: ReadDir, depth: usize) -> Vec<ManifestPath> {
            if depth == 0 {
                return Vec::new();
            }

            entities
                .filter_map(Result::ok)
                .flat_map(|e| {
                    let path = e.path();
                    if path.is_dir() {
                        read_dir(path)
                            .map(|dir| find_in_child_dir(dir, depth - 1))
                            .unwrap_or_default()
                    } else {
                        ManifestPath::try_from(path).ok().map_or_else(Vec::new, |mp| vec![mp])
                    }
                })
                .collect()
        }
    }

    pub fn discover_all(paths: &[AbsPathBuf]) -> Vec<ManifestPath> {
        let mut res = paths
            .iter()
            .filter_map(|it| ManifestPath::discover(it.as_ref()).ok())
            .flatten()
            .collect::<FxHashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();
        res.sort();
        res
    }
}

impl From<ManifestPath> for AbsPathBuf {
    fn from(it: ManifestPath) -> AbsPathBuf {
        it.file
    }
}

impl fmt::Display for ManifestPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.file, f)
    }
}

impl ops::Deref for ManifestPath {
    type Target = AbsPath;

    fn deref(&self) -> &Self::Target {
        &self.file
    }
}

impl AsRef<AbsPath> for ManifestPath {
    fn as_ref(&self) -> &AbsPath {
        self.file.as_ref()
    }
}

impl AsRef<std::path::Path> for ManifestPath {
    fn as_ref(&self) -> &std::path::Path {
        self.file.as_ref()
    }
}

impl AsRef<std::ffi::OsStr> for ManifestPath {
    fn as_ref(&self) -> &std::ffi::OsStr {
        self.file.as_ref()
    }
}

impl Borrow<AbsPath> for ManifestPath {
    fn borrow(&self) -> &AbsPath {
        self.file.borrow()
    }
}
