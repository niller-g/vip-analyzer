use super::Name;
use crate::nameres::ScopeRef;
use intern::sym;
use syntax::ast;

/// This namespace will always have a leading `\` and no trailing `\`
///
/// Special case: `\`
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct RootNs(Name);
impl RootNs {
    pub fn make_root_ns(ns: String) -> RootNs {
        assert!(!ns.is_empty());
        if ns.starts_with('\\') {
            RootNs(Name::new(ns.as_str()))
        } else {
            RootNs(Name::new(format!("\\{}", ns).as_str()))
        }
    }

    pub fn try_from_scope(scope: &ScopeRef) -> Option<RootNs> {
        if scope.generics_count != 0 {
            return None;
        }

        let ns = match &scope.ns_path {
            NsPath::Root(root_path) => RootNs::make_root_ns(format!(
                "{}{}",
                root_path.0.as_str().to_owned(),
                scope.name.as_str()
            )),
            NsPath::Relative(ns_path) => RootNs::make_root_ns(format!(
                "\\{}{}",
                ns_path.0.as_str().to_owned(),
                scope.name.as_str()
            )),
        };

        Some(ns)
    }
}
impl From<NsPath> for RootNs {
    fn from(value: NsPath) -> Self {
        match value {
            NsPath::Root(ns_path) => RootNs(Name::new(ns_path.0.as_str().trim_end_matches("\\"))),
            NsPath::Relative(ns_path) => RootNs(Name::new(
                format!("\\{}", ns_path.0.as_str().trim_end_matches("\\")).as_str(),
            )),
        }
    }
}

/// Root namespace paths start from `\` while relative namespace paths will not.
///
/// Special cases: `\`, `` (empty)
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum NsPath {
    Root(RootNsPath),
    Relative(RelativeNsPath),
}
impl NsPath {
    pub const EMPTY: NsPath = NsPath::Relative(RelativeNsPath::EMPTY);

    pub fn try_from_scope(scope: &ScopeRef) -> Option<NsPath> {
        if scope.generics_count != 0 {
            return None;
        }

        let ns = match &scope.ns_path {
            NsPath::Root(root_path) => NsPath::Root(RootNsPath::from(
                format!("{}{}\\", root_path.0.as_str(), scope.name.as_str()).as_str(),
            )),
            NsPath::Relative(path) => NsPath::Relative(RelativeNsPath::from(
                format!("{}{}\\", path.0.as_str(), scope.name.as_str()).as_str(),
            )),
        };

        Some(ns)
    }
}
impl From<ast::NamespacePath> for NsPath {
    fn from(value: ast::NamespacePath) -> Self {
        let s = value.to_string();
        if s.starts_with('\\') {
            NsPath::Root(s.as_str().into())
        } else {
            NsPath::Relative(s.as_str().into())
        }
    }
}
impl From<Option<ast::NamespacePath>> for NsPath {
    fn from(value: Option<ast::NamespacePath>) -> Self {
        match value {
            Some(value) => value.into(),
            None => NsPath::EMPTY,
        }
    }
}
impl From<Option<RootNs>> for RootNsPath {
    fn from(value: Option<RootNs>) -> Self {
        match &value {
            Some(value) => value.into(),
            None => RootNsPath(Name(sym::backslash)),
        }
    }
}

/// This namespace will always have a leading `\` and a trailing `\`
///
/// Special case: `\`
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct RootNsPath(Name);
impl RootNsPath {
    pub(crate) fn ends_with(&self, other: &RelativeNsPath) -> bool {
        self.0.as_str().ends_with(other.0.as_str())
    }
}
impl From<&str> for RootNsPath {
    fn from(value: &str) -> Self {
        assert!(value.starts_with("\\") && value.ends_with("\\"));
        RootNsPath(Name::new(value))
    }
}
impl From<&RootNs> for RootNsPath {
    fn from(value: &RootNs) -> Self {
        if value.0 == Name(sym::backslash) {
            RootNsPath(Name(sym::backslash))
        } else {
            assert!(!value.0.as_str().ends_with("\\"));
            RootNsPath::from(format!("{}\\", value.0.as_str()).as_str())
        }
    }
}
impl From<NsPath> for RootNsPath {
    fn from(value: NsPath) -> Self {
        match value {
            NsPath::Root(ns_path) => ns_path,
            NsPath::Relative(ns_path) => {
                RootNsPath::from(format!("\\{}", ns_path.0.as_str()).as_str())
            }
        }
    }
}
impl From<&RelativeNsPath> for RootNsPath {
    fn from(value: &RelativeNsPath) -> Self {
        RootNsPath::from(format!("\\{}", value.0.as_str()).as_str())
    }
}

/// This namespace will never have a leading `\` but will have a trailing `\`
///
/// Special case: `` (empty)
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct RelativeNsPath(Name);
impl RelativeNsPath {
    pub const EMPTY: RelativeNsPath = RelativeNsPath(Name(sym::__empty));

    pub(crate) fn prefix(&self, prefix_root_path: RootNsPath) -> RootNsPath {
        RootNsPath::from(format!("{}{}", prefix_root_path.0.as_str(), self.0.as_str()).as_str())
    }
}
impl From<&str> for RelativeNsPath {
    fn from(value: &str) -> Self {
        assert!(!value.starts_with("\\"));
        assert!(value.ends_with("\\") || value.is_empty());
        RelativeNsPath(Name::new(value))
    }
}
