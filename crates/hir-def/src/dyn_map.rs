//! This module defines a `DynMap` -- a container for heterogeneous maps.
//!
//! This means that `DynMap` stores a bunch of hash maps inside, and those maps
//! can be of different types.
//!
//! It is used like this:
//!
//! ```ignore
//! # use hir_def::dyn_map::DynMap;
//! # use hir_def::dyn_map::Key;
//! // keys define submaps of a `DynMap`
//! const STRING_TO_U32: Key<String, u32> = Key::new();
//! const U32_TO_VEC: Key<u32, Vec<bool>> = Key::new();
//!
//! // Note: concrete type, no type params!
//! let mut map = DynMap::new();
//!
//! // To access a specific map, index the `DynMap` by `Key`:
//! map[STRING_TO_U32].insert("hello".to_string(), 92);
//! let value = map[U32_TO_VEC].get(92);
//! assert!(value.is_none());
//! ```
//!
//! This is a work of fiction. Any similarities to Kotlin's `BindingContext` are
//! a coincidence.

pub mod keys {
    use crate::{
        ClassFactDbId, ClassFactFunctorId, ClassFactVarId, ClassPredicateId, ClassPropertyId,
        ClauseId, ConstantId, ConstructorId, DomainId, ExternalPredicateResolutionId, FactDbId,
        FactFunctorId, FactVarId, GuardClauseId, InterfaceDelegateId, InterfaceResolutionId,
        PredicateDelegateId, PredicateFromId, PredicateId, PredicateResolutionId, PropertyFromId,
        PropertyId,
    };
    use rustc_hash::FxHashMap;
    use std::marker::PhantomData;
    use syntax::{AstNode, AstPtr, ast};
    use {
        super::{DynMap, Policy},
        crate::FunctorId,
    };

    pub type Key<K, V> = crate::dyn_map::Key<AstPtr<K>, V, AstPtrPolicy<K, V>>;

    macro_rules! impl_keys {
    ($($name:ident: $ast_type:path, $id_type:ty),* $(,)?) => {
        $(pub const $name: Key<$ast_type, $id_type> = Key::new();)+
    };
}

    impl_keys![
        PREDICATE: ast::Predicate, PredicateId,
        CLASS_PREDICATE: ast::ClassPredicate, ClassPredicateId,
        CONSTRUCTOR: ast::Constructor, ConstructorId,
        FACT_DB: ast::FactsSection, FactDbId,
        CLASS_FACT_DB: ast::ClassFactsSection, ClassFactDbId,
        FACT_FUNCTOR: ast::FactFunctor, FactFunctorId,
        FACT_VAR: ast::FactVar, FactVarId,
        CLASS_FACT_FUNCTOR: ast::ClassFactFunctor, ClassFactFunctorId,
        CLASS_FACT_VAR: ast::ClassFactVar, ClassFactVarId,
        PROPERTY: ast::Property, PropertyId,
        CLASS_PROPERTY: ast::ClassProperty, ClassPropertyId,
        CONSTANT: ast::Constant, ConstantId,
        DOMAIN: ast::Domain, DomainId,
        FUNCTOR: ast::Functor, FunctorId,
        CLAUSE: ast::Clause, ClauseId,
        GUARD_CLAUSE: ast::GuardClause, GuardClauseId,
        PREDICATE_DELEGATE: ast::PredicateDelegate, PredicateDelegateId,
        INTERFACE_DELEGATE: ast::InterfaceDelegate, InterfaceDelegateId,
        PREDICATE_RESOLUTION: ast::PredicateResolution, PredicateResolutionId,
        INTERFACE_RESOLUTION: ast::InterfaceResolution, InterfaceResolutionId,
        EXTERNAL_PREDICATE_RESOLUTION: ast::ExternalPredicateResolution, ExternalPredicateResolutionId,
        PREDICATE_FROM: ast::Arity, PredicateFromId,
        PROPERTY_FROM: ast::Arity, PropertyFromId,
    ];

    /// XXX: AST Nodes and SyntaxNodes have identity equality semantics: nodes are
    /// equal if they point to exactly the same object.
    ///
    /// In general, we do not guarantee that we have exactly one instance of a
    /// syntax tree for each file. We probably should add such guarantee, but, for
    /// the time being, we will use identity-less AstPtr comparison.
    pub struct AstPtrPolicy<AST, ID> {
        _phantom: PhantomData<(AST, ID)>,
    }

    impl<AST: AstNode + 'static, ID: 'static> Policy for AstPtrPolicy<AST, ID> {
        type K = AstPtr<AST>;
        type V = ID;
        fn insert(map: &mut DynMap, key: AstPtr<AST>, value: ID) {
            map.map
                .entry::<FxHashMap<AstPtr<AST>, ID>>()
                .or_insert_with(Default::default)
                .insert(key, value);
        }
        fn get<'a>(map: &'a DynMap, key: &AstPtr<AST>) -> Option<&'a ID> {
            map.map.get::<FxHashMap<AstPtr<AST>, ID>>()?.get(key)
        }
        fn is_empty(map: &DynMap) -> bool {
            map.map.get::<FxHashMap<AstPtr<AST>, ID>>().is_none_or(|it| it.is_empty())
        }
    }
}

use std::{
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use rustc_hash::FxHashMap;
use stdx::anymap::Map;

pub struct Key<K, V, P = (K, V)> {
    _phantom: PhantomData<(K, V, P)>,
}

impl<K, V, P> Key<K, V, P> {
    pub(crate) const fn new() -> Key<K, V, P> {
        Key { _phantom: PhantomData }
    }
}

impl<K, V, P> Copy for Key<K, V, P> {}

impl<K, V, P> Clone for Key<K, V, P> {
    fn clone(&self) -> Key<K, V, P> {
        *self
    }
}

pub trait Policy {
    type K;
    type V;

    fn insert(map: &mut DynMap, key: Self::K, value: Self::V);
    fn get<'a>(map: &'a DynMap, key: &Self::K) -> Option<&'a Self::V>;
    fn is_empty(map: &DynMap) -> bool;
}

impl<K: Hash + Eq + 'static, V: 'static> Policy for (K, V) {
    type K = K;
    type V = V;
    fn insert(map: &mut DynMap, key: K, value: V) {
        map.map.entry::<FxHashMap<K, V>>().or_insert_with(Default::default).insert(key, value);
    }
    fn get<'a>(map: &'a DynMap, key: &K) -> Option<&'a V> {
        map.map.get::<FxHashMap<K, V>>()?.get(key)
    }
    fn is_empty(map: &DynMap) -> bool {
        map.map.get::<FxHashMap<K, V>>().is_none_or(|it| it.is_empty())
    }
}

#[derive(Default)]
pub struct DynMap {
    pub(crate) map: Map,
}

#[repr(transparent)]
pub struct KeyMap<KEY> {
    map: DynMap,
    _phantom: PhantomData<KEY>,
}

impl<P: Policy> KeyMap<Key<P::K, P::V, P>> {
    pub fn insert(&mut self, key: P::K, value: P::V) {
        P::insert(&mut self.map, key, value)
    }
    pub fn get(&self, key: &P::K) -> Option<&P::V> {
        P::get(&self.map, key)
    }

    pub fn is_empty(&self) -> bool {
        P::is_empty(&self.map)
    }
}

impl<P: Policy> Index<Key<P::K, P::V, P>> for DynMap {
    type Output = KeyMap<Key<P::K, P::V, P>>;
    fn index(&self, _key: Key<P::K, P::V, P>) -> &Self::Output {
        // Safe due to `#[repr(transparent)]`.
        unsafe { std::mem::transmute::<&DynMap, &KeyMap<Key<P::K, P::V, P>>>(self) }
    }
}

impl<P: Policy> IndexMut<Key<P::K, P::V, P>> for DynMap {
    fn index_mut(&mut self, _key: Key<P::K, P::V, P>) -> &mut Self::Output {
        // Safe due to `#[repr(transparent)]`.
        unsafe { std::mem::transmute::<&mut DynMap, &mut KeyMap<Key<P::K, P::V, P>>>(self) }
    }
}
