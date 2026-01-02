use crate::{
    ClassId, InterfaceId, ItemDefId, Lookup, PredicateId, PropertyId,
    db::DefDatabase,
    hir::PropertyFlowPattern,
    hir::{MemberDeclaration, Name, TermType},
    item_scope::{self, ItemScope, PredicateResolution},
    nameres::{DefMap, ScopeRef},
};
use std::ops::RangeInclusive;
use stdx::itertools::Itertools;

pub struct MemberDeclarations<'db> {
    db: &'db dyn DefDatabase,
    def_map: &'db DefMap,
    scopes: Vec<ItemScope>,
    allow_object_ctx: bool,
    name_filter: Option<Name>,
    arity_filter: RangeInclusive<u8>,
    term_type: TermType,
}
impl<'db> MemberDeclarations<'db> {
    pub const fn empty(db: &'db dyn DefDatabase, def_map: &'db DefMap) -> Self {
        Self {
            db,
            def_map,
            scopes: Vec::new(),
            allow_object_ctx: true,
            name_filter: None,
            arity_filter: 0..=u8::MAX,
            term_type: TermType::ExprStmt,
        }
    }

    // TODO: it is to simple to just return one here. It should potentially return multiple members.
    fn walk_predicate_from(
        &self,
        scope: &InterfaceId,
        visited: &mut Vec<InterfaceId>,
    ) -> Option<PredicateId> {
        if visited.contains(scope) {
            return None;
        } else {
            visited.push(*scope);
        }

        self.def_map[scope].declarations().iter().find_map(move |def_id| match def_id {
            ItemDefId::PredicateId(id) => self
                .match_member(
                    &MemberDeclaration::Predicate(*id),
                    self.name_filter.as_ref(),
                    &self.arity_filter,
                    &self.term_type,
                )
                .then_some(*id),
            ItemDefId::PredicateFromId(id) => {
                let loc = id.lookup(self.db);
                let item = &self.db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
                let pred_from = &item[loc.id.value];

                self.db
                    .available_scopes(
                        ItemScope::from(scope)
                            .absolutize_interface(self.def_map, pred_from.from.clone())?
                            .into(),
                    )
                    .all_supports()
                    .find_map(|support| self.walk_predicate_from(&support, visited))
            }
            _ => None,
        })
    }

    // TODO: it is to simple to just return one here. It should potentially return multiple members.
    fn walk_property_from(
        &self,
        scope: &InterfaceId,
        visited: &mut Vec<InterfaceId>,
    ) -> Option<PropertyId> {
        if visited.contains(scope) {
            return None;
        } else {
            visited.push(*scope);
        }

        self.def_map[scope].declarations().iter().find_map(move |def_id| match def_id {
            ItemDefId::PropertyId(id) => self
                .match_member(
                    &MemberDeclaration::Property(*id),
                    self.name_filter.as_ref(),
                    &self.arity_filter,
                    &self.term_type,
                )
                .then_some(*id),
            ItemDefId::PropertyFromId(id) => {
                let loc = id.lookup(self.db);
                let item = &self.db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
                let item = &item[loc.id.value];

                self.db
                    .available_scopes(
                        ItemScope::from(scope)
                            .absolutize_interface(self.def_map, item.from.clone())?
                            .into(),
                    )
                    .all_supports()
                    .find_map(|support| self.walk_property_from(&support, visited))
            }
            _ => None,
        })
    }

    fn collect_local_members<'b>(
        &self,
        scope: &'b ItemScope,
    ) -> impl Iterator<Item = MemberDeclaration> + use<'_, 'b> {
        let (declarations, implicit_object_ctx) = match scope {
            ItemScope::Implement(id) => (self.def_map[id].declarations(), true),
            ItemScope::Class(id) => (self.def_map[id].declarations(), false),
            ItemScope::Interface(id) => (self.def_map[id].declarations(), true),
        };
        let match_implicit_ctx = self.allow_object_ctx || !implicit_object_ctx;

        declarations.iter().filter_map(move |def_id| {
            let member = match def_id {
                // Declared in class -> static member
                // Declared in implement or interface -> object member
                ItemDefId::PredicateId(id) if match_implicit_ctx => {
                    MemberDeclaration::Predicate(*id)
                }
                ItemDefId::PropertyId(id) if match_implicit_ctx => MemberDeclaration::Property(*id),

                // Static members: Allow in class and object context.
                ItemDefId::ClassPredicateId(id) => MemberDeclaration::ClassPredicate(*id),
                ItemDefId::ConstructorId(id) => MemberDeclaration::Constructor(*id),
                ItemDefId::ClassFactFunctorId(id) => MemberDeclaration::ClassFactFunctor(*id),
                ItemDefId::ClassFactVarId(id) => MemberDeclaration::ClassFactVar(*id),
                ItemDefId::ConstantId(id) => MemberDeclaration::Constant(*id),
                ItemDefId::FunctorId(id) => MemberDeclaration::Functor(*id),
                ItemDefId::ClassPropertyId(id) => MemberDeclaration::ClassProperty(*id),

                // Object members: Allow in object context only.
                ItemDefId::FactFunctorId(id) if self.allow_object_ctx => {
                    MemberDeclaration::FactFunctor(*id)
                }
                ItemDefId::FactVarId(id) if self.allow_object_ctx => {
                    MemberDeclaration::FactVar(*id)
                }
                ItemDefId::PredicateFromId(id) if self.allow_object_ctx => {
                    let loc = id.lookup(self.db);
                    let item = &self.db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
                    let item = &item[loc.id.value];
                    let source = scope.absolutize_interface(self.def_map, item.from.clone())?;
                    self.walk_predicate_from(&source, &mut vec![loc.container])
                        .map(MemberDeclaration::Predicate)?
                }
                ItemDefId::PropertyFromId(id) if self.allow_object_ctx => {
                    let loc = id.lookup(self.db);
                    let item = &self.db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
                    let item = &item[loc.id.value];
                    let source = scope.absolutize_interface(self.def_map, item.from.clone())?;
                    self.walk_property_from(&source, &mut vec![loc.container])
                        .map(MemberDeclaration::Property)?
                }
                _ => return None,
            };
            Some(member)
        })
    }

    fn match_member(
        &self,
        member: &MemberDeclaration,
        name_filter: Option<&Name>,
        arity_filter: &RangeInclusive<u8>,
        term_type_filter: &TermType,
    ) -> bool {
        match declaration_signatures(self.db, member) {
            DeclarationSignatures::One(sig) => {
                Self::match_signature(&sig, name_filter, arity_filter, term_type_filter)
            }
            DeclarationSignatures::Two(sig1, sig2) => {
                Self::match_signature(&sig1, name_filter, arity_filter, term_type_filter)
                    || Self::match_signature(&sig2, name_filter, arity_filter, term_type_filter)
            }
        }
    }

    fn match_signature(
        signature: &DeclarationSignature,
        expected_name: Option<&Name>,
        expected_arity: &RangeInclusive<u8>,
        expected_term_type: &TermType,
    ) -> bool {
        if let Some(expected) = expected_name
            && *expected != signature.name
        {
            return false;
        }

        if !signature.overlapping_arity(expected_arity) {
            return false;
        }

        if !expected_term_type.compatible_with(&signature.term_type) {
            return false;
        }

        true
    }

    pub fn iter(&self) -> impl Iterator<Item = MemberDeclaration> {
        self.scopes.iter().unique().flat_map(|scope| self.collect_local_members(scope)).filter(
            |member| {
                self.match_member(
                    member,
                    self.name_filter.as_ref(),
                    &self.arity_filter,
                    &self.term_type,
                )
            },
        )
    }

    pub fn first(&self) -> Option<MemberDeclaration> {
        self.iter().next()
    }
}

pub(crate) struct DeclarationFinder<'db> {
    db: &'db dyn DefDatabase,
    def_map: &'db DefMap,
    self_scope: &'db ItemScope,
    include_self_scope: bool,
    explicit_scopes: Vec<ItemScope>,
    allow_object_ctx: bool,
    name_filter: Option<Name>,
    arity_filter: RangeInclusive<u8>,
    term_type: TermType,
}
impl<'db> DeclarationFinder<'db> {
    pub(crate) fn new(
        db: &'db dyn DefDatabase,
        def_map: &'db DefMap,
        self_scope: &'db ItemScope,
    ) -> Self {
        Self {
            db,
            def_map,
            self_scope,
            include_self_scope: false,
            explicit_scopes: Vec::new(),
            allow_object_ctx: self_scope.is_object_ctx(def_map),
            name_filter: None,
            arity_filter: 0..=u8::MAX,
            term_type: TermType::ExprStmt,
        }
    }

    pub(crate) fn with_self_scope(mut self) -> Self {
        self.include_self_scope = true;
        self
    }

    pub(crate) fn with_members_from(mut self, scope: impl Into<Option<ScopeRef>>) -> Self {
        if let Some(scope) = scope.into()
            && let Some(scopes) = self.self_scope.absolutize(self.def_map, scope)
        {
            let (cl, i) = scopes.left_and_right();
            if let Some(cl) = cl {
                self.explicit_scopes.push(ItemScope::Class(cl));
            }
            if let Some(i) = i {
                self.explicit_scopes.push(ItemScope::Interface(i));
            }
        }
        self
    }

    #[expect(dead_code)]
    pub(crate) fn allow_object_ctx(mut self, allow_object_ctx: bool) -> Self {
        self.allow_object_ctx = allow_object_ctx;
        self
    }

    pub(crate) fn filter_name(mut self, name: Name) -> Self {
        self.name_filter = Some(name);
        self
    }

    pub(crate) fn filter_arity(mut self, arity: RangeInclusive<u8>) -> Self {
        self.arity_filter = arity;
        self
    }

    pub(crate) fn filter_term_type(mut self, term_type: TermType) -> Self {
        self.term_type = term_type;
        self
    }

    fn intersect_arity_range(
        left: RangeInclusive<u8>,
        right: RangeInclusive<u8>,
    ) -> RangeInclusive<u8> {
        let start = *left.start().max(right.start());
        let end = *left.end().min(right.end());

        assert!(start <= end);
        start..=end
    }

    pub(crate) fn build(self) -> MemberDeclarations<'db> {
        let resolutions = self.self_scope.predicate_resolutions(self.db);
        let matching_resolution = resolutions.iter().find(|res| self.match_resolution(res));

        if let Some(resolved) = matching_resolution {
            let object_scopes = |class: &ClassId| {
                self.def_map[class]
                    .construction_interface()
                    .map(|interface| {
                        self.db.available_scopes(ItemScope::from(interface)).iter().collect()
                    })
                    .unwrap_or_default()
            };

            match &resolved.from {
                item_scope::PredicateFrom::Class(class) => {
                    return MemberDeclarations {
                        db: self.db,
                        def_map: self.def_map,
                        scopes: object_scopes(class),
                        allow_object_ctx: true,
                        name_filter: self.name_filter,
                        arity_filter: Self::intersect_arity_range(
                            self.arity_filter,
                            resolved.arity.arg_count(),
                        ),
                        term_type: self.term_type.most_specific(resolved.arity.term_type()),
                    };
                }
                item_scope::PredicateFrom::ClassAndRename { class, new_name } => {
                    return MemberDeclarations {
                        db: self.db,
                        def_map: self.def_map,
                        allow_object_ctx: true,
                        scopes: object_scopes(class),
                        name_filter: Some(new_name.clone()),
                        arity_filter: self.arity_filter,
                        term_type: self.term_type.most_specific(resolved.arity.term_type()),
                    };
                }
            };
        }

        let scopes = if self.include_self_scope {
            self.db.available_scopes(*self.self_scope).iter().chain(self.explicit_scopes).collect()
        } else {
            self.explicit_scopes
        };
        MemberDeclarations {
            db: self.db,
            def_map: self.def_map,
            scopes,
            allow_object_ctx: self.allow_object_ctx,
            name_filter: self.name_filter,
            arity_filter: self.arity_filter,
            term_type: self.term_type,
        }
    }

    fn match_resolution(&self, resolution: &PredicateResolution) -> bool {
        MemberDeclarations::match_signature(
            &DeclarationSignature::new(
                resolution.arity.name.clone(),
                resolution.arity.arg_count(),
                resolution.arity.term_type(),
            ),
            self.name_filter.as_ref(),
            &self.arity_filter,
            &self.term_type,
        )
    }
}

pub(crate) struct DeclarationSignature {
    name: Name,
    arity: RangeInclusive<u8>,
    term_type: TermType,
}
impl DeclarationSignature {
    fn new(name: Name, arity: RangeInclusive<u8>, term_type: TermType) -> Self {
        Self { name, arity, term_type }
    }

    fn overlapping_arity(&self, other: &RangeInclusive<u8>) -> bool {
        self.arity.start() <= other.end() && self.arity.end() >= other.start()
    }
}

pub(crate) enum DeclarationSignatures {
    One(DeclarationSignature),
    Two(DeclarationSignature, DeclarationSignature),
}
impl DeclarationSignatures {
    pub(crate) fn match_name(&self, name: &Name) -> bool {
        match self {
            DeclarationSignatures::One(sig) => &sig.name == name,
            DeclarationSignatures::Two(sig1, sig2) => &sig1.name == name || &sig2.name == name,
        }
    }

    pub(crate) fn overlapping_arity(&self, other: &RangeInclusive<u8>) -> bool {
        match self {
            DeclarationSignatures::One(sig) => sig.overlapping_arity(other),
            DeclarationSignatures::Two(sig1, sig2) => {
                sig1.overlapping_arity(other) || sig2.overlapping_arity(other)
            }
        }
    }

    pub(crate) fn compatible_term_type(&self, term_type: &TermType) -> bool {
        match self {
            DeclarationSignatures::One(sig) => sig.term_type.compatible_with(term_type),
            DeclarationSignatures::Two(sig1, sig2) => {
                sig1.term_type.compatible_with(term_type)
                    || sig2.term_type.compatible_with(term_type)
            }
        }
    }
}
impl From<DeclarationSignature> for DeclarationSignatures {
    fn from(sig: DeclarationSignature) -> Self {
        Self::One(sig)
    }
}

pub(crate) fn declaration_signatures(
    db: &dyn DefDatabase,
    member: &MemberDeclaration,
) -> DeclarationSignatures {
    match member {
        MemberDeclaration::Constant(id) => {
            let sig = db.constant_signature(*id);
            DeclarationSignature::new(sig.name.clone(), 0..=0, TermType::Expr).into()
        }
        MemberDeclaration::Predicate(id) => {
            let sig = db.predicate_signature(*id);
            DeclarationSignature::new(sig.name.clone(), sig.arity_range(), sig.term_type()).into()
        }
        MemberDeclaration::ClassPredicate(id) => {
            let sig = db.class_predicate_signature(*id);
            DeclarationSignature::new(sig.name.clone(), sig.arity_range(), sig.term_type()).into()
        }
        MemberDeclaration::Constructor(id) => {
            let sig = db.constructor_signature(*id);
            DeclarationSignature::new(sig.name.clone(), sig.arity_range(), TermType::ExprStmt) // <-- todo: this is slightly ambiguous since constructors can return implicitly
                .into()
        }
        MemberDeclaration::FactFunctor(id) => {
            let sig = db.fact_functor_signature(*id);
            DeclarationSignature::new(sig.name.clone(), sig.arg_count(), TermType::Stmt).into()
        }
        MemberDeclaration::FactVar(id) => {
            let sig = db.fact_var_signature(*id);
            DeclarationSignatures::Two(
                DeclarationSignature::new(sig.name.clone(), 0..=0, TermType::Expr),
                DeclarationSignature::new(sig.name.clone(), 1..=1, TermType::Stmt),
            )
        }
        MemberDeclaration::ClassFactFunctor(id) => {
            let sig = db.class_fact_functor_signature(*id);
            DeclarationSignature::new(sig.name.clone(), sig.arg_count(), TermType::Stmt).into()
        }
        MemberDeclaration::ClassFactVar(id) => {
            let sig = db.class_fact_var_signature(*id);
            DeclarationSignatures::Two(
                DeclarationSignature::new(sig.name.clone(), 0..=0, TermType::Expr),
                DeclarationSignature::new(sig.name.clone(), 1..=1, TermType::Stmt),
            )
        }
        MemberDeclaration::Functor(id) => {
            let sig = db.functor_signature(*id);
            DeclarationSignature::new(sig.name.clone(), sig.arg_count(), TermType::Expr).into()
        }
        MemberDeclaration::Property(id) => {
            let sig = db.property_signature(*id);
            match sig.flow_pattern {
                PropertyFlowPattern::In => {
                    DeclarationSignature::new(sig.name.clone(), 0..=1, TermType::Stmt).into()
                }
                PropertyFlowPattern::Out => {
                    DeclarationSignature::new(sig.name.clone(), 0..=0, TermType::Expr).into()
                }
                PropertyFlowPattern::InOut => DeclarationSignatures::Two(
                    DeclarationSignature::new(sig.name.clone(), 0..=0, TermType::Expr),
                    DeclarationSignature::new(sig.name.clone(), 0..=1, TermType::Stmt),
                ),
            }
        }
        MemberDeclaration::ClassProperty(id) => {
            let sig = db.class_property_signature(*id);
            match sig.flow_pattern {
                PropertyFlowPattern::In => {
                    DeclarationSignature::new(sig.name.clone(), 0..=1, TermType::Stmt).into()
                }
                PropertyFlowPattern::Out => {
                    DeclarationSignature::new(sig.name.clone(), 0..=0, TermType::Expr).into()
                }
                PropertyFlowPattern::InOut => DeclarationSignatures::Two(
                    DeclarationSignature::new(sig.name.clone(), 0..=0, TermType::Expr),
                    DeclarationSignature::new(sig.name.clone(), 0..=1, TermType::Stmt),
                ),
            }
        }
    }
}
