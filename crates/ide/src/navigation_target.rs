//! See [`NavigationTarget`].

use std::fmt;
use syntax::{AstNode, SmolStr, TextRange, ToSmolStr, ast::HasScopeNameDecl};
use {either::Either, ide_db::classify::Definition};
use {
    hir::{HasSource, InFile},
    ide_db::classify::Declaration,
};
use {
    ide_db::{FileId, RootDatabase, documentation::Documentation},
    syntax::ast::HasType,
};

/// `NavigationTarget` represents an element in the editor's UI which you can
/// click on to navigate to a particular piece of code.
///
/// Typically, a `NavigationTarget` corresponds to some element in the source
/// code, like a function or a struct, but this is not strictly required.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NavigationTarget {
    pub file_id: FileId,
    /// Range which encompasses the whole element.
    ///
    /// Should include body, doc comments, attributes, etc.
    ///
    /// Clients should use this range to answer "is the cursor inside the
    /// element?" question.
    pub full_range: TextRange,
    /// A "most interesting" range within the `full_range`.
    ///
    /// Typically, `full_range` is the whole syntax node, including doc
    /// comments, and `focus_range` is the range of the identifier.
    ///
    /// Clients should place the cursor on this range when navigating to this target.
    ///
    /// This range must be contained within [`Self::full_range`].
    pub focus_range: Option<TextRange>,
    pub name: SmolStr,
    pub container_name: Option<SmolStr>,
    pub description: Option<String>,
    pub docs: Option<Documentation>,
    /// In addition to a `name` field, a `NavigationTarget` may also be aliased
    /// In such cases we want a `NavigationTarget` to be accessible by its alias
    pub alias: Option<SmolStr>,
}

impl fmt::Debug for NavigationTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("NavigationTarget");
        macro_rules! opt {
            ($($name:ident)*) => {$(
                if let Some(it) = &self.$name {
                    f.field(stringify!($name), it);
                }
            )*}
        }
        f.field("file_id", &self.file_id).field("full_range", &self.full_range);
        opt!(focus_range);
        f.field("name", &self.name);
        opt!(container_name description docs);
        f.finish()
    }
}

#[expect(unused)]
pub(crate) trait ToNav {
    fn to_nav(&self, db: &RootDatabase) -> NavigationTarget;
}

pub trait TryToNav {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget>;
}

impl<T: TryToNav, U: TryToNav> TryToNav for Either<T, U> {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        match self {
            Either::Left(it) => it.try_to_nav(db),
            Either::Right(it) => it.try_to_nav(db),
        }
    }
}

impl NavigationTarget {
    pub fn focus_or_full_range(&self) -> TextRange {
        self.focus_range.unwrap_or(self.full_range)
    }

    #[cfg(test)]
    #[expect(unused)]
    pub(crate) fn debug_render(&self) -> String {
        let mut buf = format!("{} {:?} {:?}", self.name, self.file_id, self.full_range);
        if let Some(focus_range) = self.focus_range {
            buf.push_str(&format!(" {focus_range:?}"))
        }
        if let Some(container_name) = &self.container_name {
            buf.push_str(&format!(" {container_name}"))
        }
        buf
    }

    pub(crate) fn from_syntax(
        file_id: FileId,
        name: SmolStr,
        focus_range: Option<TextRange>,
        full_range: TextRange,
    ) -> NavigationTarget {
        NavigationTarget {
            file_id,
            name,
            full_range,
            focus_range,
            container_name: None,
            description: None,
            docs: None,
            alias: None,
        }
    }
}

impl TryToNav for Declaration {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        match self {
            Declaration::ClassItem(it) => it.try_to_nav(db),
            Declaration::InterfaceItem(it) => it.try_to_nav(db),
            Declaration::Constant(it) => it.try_to_nav(db),
            Declaration::Domain(it) => it.try_to_nav(db),
            Declaration::Functor(it) => it.try_to_nav(db),
            Declaration::FactFunctor(it) => it.try_to_nav(db),
            Declaration::ClassFactFunctor(it) => it.try_to_nav(db),
            Declaration::FactVar(it) => it.try_to_nav(db),
            Declaration::ClassFactVar(it) => it.try_to_nav(db),
            Declaration::Predicate(it) => it.try_to_nav(db),
            Declaration::ClassPredicate(it) => it.try_to_nav(db),
            Declaration::Constructor(it) => it.try_to_nav(db),
            Declaration::Property(it) => it.try_to_nav(db),
            Declaration::ClassProperty(it) => it.try_to_nav(db),
        }
    }
}

impl TryToNav for Definition {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        match self {
            Definition::ImplementItem(it) => it.try_to_nav(db),
            Definition::Clause(it) => it.try_to_nav(db),
        }
    }
}

impl TryToNav for hir::ClassItem {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let scope_name_decl = value.scope_name_decl();
        let name =
            scope_name_decl.as_ref().map(|name| name.to_string().to_smolstr()).unwrap_or_default();
        let focus = scope_name_decl.map(|s| s.syntax().text_range());

        Some(NavigationTarget::from_syntax(file_id, name, focus, value.syntax().text_range()))
    }
}
impl TryToNav for hir::ImplementItem {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let scope_name_decl = value.scope_name_decl();
        let name =
            scope_name_decl.as_ref().map(|name| name.to_string().to_smolstr()).unwrap_or_default();
        let focus = scope_name_decl.map(|s| s.syntax().text_range());

        Some(NavigationTarget::from_syntax(file_id, name, focus, value.syntax().text_range()))
    }
}
impl TryToNav for hir::InterfaceItem {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let scope_name_decl = value.scope_name_decl();
        let name =
            scope_name_decl.as_ref().map(|name| name.to_string().to_smolstr()).unwrap_or_default();
        let focus = scope_name_decl.map(|s| s.syntax().text_range());

        Some(NavigationTarget::from_syntax(file_id, name, focus, value.syntax().text_range()))
    }
}

impl TryToNav for hir::Clause {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let name = value.ident_token().map(|it| it.to_smolstr()).unwrap_or_default();
        let focus = value.ident_token().map(|s| s.text_range());
        Some(NavigationTarget::from_syntax(file_id, name, focus, value.syntax().text_range()))
    }
}

impl TryToNav for hir::Domain {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let name = value.ident_token().map(|it| it.to_smolstr()).unwrap_or_default();
        let text_range = value.syntax().text_range();
        Some(NavigationTarget::from_syntax(file_id, name, Some(text_range), text_range))
    }
}

impl TryToNav for hir::Constant {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let name = value.ident_token().map(|it| it.to_smolstr()).unwrap_or_default();
        let text_range = value.syntax().text_range();
        Some(NavigationTarget::from_syntax(file_id, name, Some(text_range), text_range))
    }
}

impl TryToNav for hir::Predicate {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let name = value.ident_token().map(|it| it.to_smolstr()).unwrap_or_default();
        let text_range = value.syntax().text_range();
        Some(NavigationTarget::from_syntax(file_id, name, Some(text_range), text_range))
    }
}

impl TryToNav for hir::ClassPredicate {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let name = value.ident_token().map(|it| it.to_smolstr()).unwrap_or_default();
        let text_range = value.syntax().text_range();
        Some(NavigationTarget::from_syntax(file_id, name, Some(text_range), text_range))
    }
}

impl TryToNav for hir::Constructor {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let name = value.ident_token().map(|it| it.to_smolstr()).unwrap_or_default();
        let text_range = value.syntax().text_range();
        Some(NavigationTarget::from_syntax(file_id, name, Some(text_range), text_range))
    }
}
impl TryToNav for hir::FactFunctor {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let name = value.ident_token().map(|it| it.to_smolstr()).unwrap_or_default();
        let text_range = value.syntax().text_range();
        Some(NavigationTarget::from_syntax(file_id, name, Some(text_range), text_range))
    }
}
impl TryToNav for hir::FactVar {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let name = value.ident_token().map(|it| it.to_smolstr()).unwrap_or_default();
        let text_range = value.syntax().text_range();
        Some(NavigationTarget::from_syntax(file_id, name, Some(text_range), text_range))
    }
}
impl TryToNav for hir::ClassFactFunctor {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let name = value.ident_token().map(|it| it.to_smolstr()).unwrap_or_default();
        let text_range = value.syntax().text_range();
        Some(NavigationTarget::from_syntax(file_id, name, Some(text_range), text_range))
    }
}
impl TryToNav for hir::ClassFactVar {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let name = value.ident_token().map(|it| it.to_smolstr()).unwrap_or_default();
        let text_range = value.syntax().text_range();
        Some(NavigationTarget::from_syntax(file_id, name, Some(text_range), text_range))
    }
}
impl TryToNav for hir::Functor {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let name = value.ty().map(|it| it.to_string());
        let name = name.map(|it| it.to_smolstr()).unwrap_or_default();
        let text_range = value.syntax().text_range();
        Some(NavigationTarget::from_syntax(file_id, name, Some(text_range), text_range))
    }
}

impl TryToNav for hir::Property {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let name = value.ident_token().map(|it| it.to_smolstr()).unwrap_or_default();
        let text_range = value.syntax().text_range();
        Some(NavigationTarget::from_syntax(file_id, name, Some(text_range), text_range))
    }
}

impl TryToNav for hir::ClassProperty {
    fn try_to_nav(&self, db: &RootDatabase) -> Option<NavigationTarget> {
        let InFile { file_id, value } = self.source(db)?;
        let name = value.ident_token().map(|it| it.to_smolstr()).unwrap_or_default();
        let text_range = value.syntax().text_range();
        Some(NavigationTarget::from_syntax(file_id, name, Some(text_range), text_range))
    }
}
