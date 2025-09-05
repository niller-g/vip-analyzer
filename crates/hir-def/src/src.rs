//! Utilities for mapping between hir IDs and the surface syntax.

use std::borrow::Borrow;

use crate::{ItemTreeLoc, db::DefDatabase, item_tree::ItemTreeNode};
use span::FileId;
use syntax::{AstNode, AstPtr, SyntaxNode};

/// `InFile<T>` stores a value of `T` inside a particular file/syntax tree.
/// Example: `InFile<SyntaxNode>` -- syntax node in a file
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct InFile<T> {
    pub file_id: FileId,
    pub value: T,
}

impl<T> InFile<T> {
    pub fn new(file_id: FileId, value: T) -> Self {
        Self { file_id, value }
    }

    pub fn with_value<U>(&self, value: U) -> InFile<U> {
        InFile::new(self.file_id, value)
    }

    pub fn as_ref(&self) -> InFile<&T> {
        self.with_value(&self.value)
    }

    pub fn borrow<U>(&self) -> InFile<&U>
    where
        T: Borrow<U>,
    {
        self.with_value(self.value.borrow())
    }

    pub fn map<F: FnOnce(T) -> U, U>(self, f: F) -> InFile<U> {
        InFile::new(self.file_id, f(self.value))
    }
}

impl<T: Clone> InFile<&T> {
    pub fn cloned(&self) -> InFile<T> {
        self.with_value(self.value.clone())
    }
}

impl<N: AstNode> InFile<N> {
    pub fn syntax(&self) -> InFile<SyntaxNode> {
        self.with_value(self.value.syntax().clone())
    }
}

impl<N: AstNode> InFile<&N> {
    pub fn syntax_ref(&self) -> InFile<&SyntaxNode> {
        self.with_value(self.value.syntax())
    }
}

pub trait HasSource {
    type Value: AstNode;
    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value> {
        let InFile { file_id, value } = self.ast_ptr(db);
        InFile::new(file_id, value.to_node(&db.parse(file_id).syntax_node()))
    }
    fn ast_ptr(&self, db: &dyn DefDatabase) -> InFile<AstPtr<Self::Value>>;
}

impl<T> HasSource for T
where
    T: ItemTreeLoc,
    T::Id: ItemTreeNode,
{
    type Value = <T::Id as ItemTreeNode>::Source;
    fn ast_ptr(&self, db: &dyn DefDatabase) -> InFile<AstPtr<Self::Value>> {
        let id = self.item_tree_id();
        let file_id = id.file_id();
        let trees = db.file_item_trees(file_id);
        let tree = &trees[id.tree_id()];
        let ast_id_map = db.ast_id_map(file_id);
        let node = &tree[id.value];

        InFile::new(file_id, ast_id_map.get(node.ast_id()))
    }
}
