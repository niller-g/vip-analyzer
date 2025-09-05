use super::member_declaration::declaration_signatures;
use crate::{
    ItemContainerId, ItemDefId, Lookup,
    db::DefDatabase,
    hir::{MemberDeclaration, MemberDefinition, TermType},
    item_scope::ItemScope,
    nameres::DefMap,
};

// TODO: handle delegate qualifications
pub(crate) fn find_definition(
    db: &dyn DefDatabase,
    scope: ItemScope,
    declaration: &MemberDeclaration,
) -> Option<MemberDefinition> {
    let declaration_container = match declaration {
        MemberDeclaration::Predicate(id) => id.lookup(db).container,
        MemberDeclaration::ClassPredicate(id) => id.lookup(db).container.into(),
        MemberDeclaration::Constructor(id) => id.lookup(db).container.into(),
        MemberDeclaration::FactFunctor(id) => id.lookup(db).container.into(),
        MemberDeclaration::ClassFactFunctor(id) => id.lookup(db).container.into(),
        MemberDeclaration::Property(id) => id.lookup(db).container,
        MemberDeclaration::ClassProperty(id) => id.lookup(db).container.into(),
        MemberDeclaration::ClassFactVar(_)
        | MemberDeclaration::FactVar(_)
        | MemberDeclaration::Functor(_)
        | MemberDeclaration::Constant(_) => return None,
    };

    let scopes = db.available_scopes(scope);
    let def_map = scope.def_map(db);

    match declaration_container {
        ItemContainerId::ImplementId(implement_id) => {
            find_local_definition(db, def_map, &implement_id.into(), declaration)
        }
        ItemContainerId::ClassId(class_id) => {
            let implement = class_id.find_class_implementation(def_map)?;
            find_local_definition(db, def_map, &implement.into(), declaration)
        }
        ItemContainerId::InterfaceId(interface_id) => {
            let relevant_inherits = scopes.inherited.iter().filter(|(_, inherits)| {
                inherits.iter().any(|inherit| inherit.supports(&interface_id))
            });

            relevant_inherits
                .filter_map(|(inherit, _)| inherit.find_class_implementation(def_map))
                .find_map(|implement| {
                    let implement = implement.into();
                    find_local_definition(db, def_map, &implement, declaration)
                        .or_else(|| find_definition(db, implement, declaration))
                })
        }
    }
}

fn find_local_definition(
    db: &dyn DefDatabase,
    def_map: &DefMap,
    scope: &ItemScope,
    declaration: &MemberDeclaration,
) -> Option<MemberDefinition> {
    let declarations = declaration_signatures(db, declaration);

    collect_local_definition(db, def_map, scope).find(|def| match def {
        MemberDefinition::ClauseFamily(clauses) => {
            let Some(clause) = clauses.first() else {
                return false;
            };
            let def = db.clause_signature(*clause);
            declarations.match_name(&def.name)
                && declarations.overlapping_arity(&def.arity_range())
                && declarations.compatible_term_type(&TermType::from_bool(def.returns))
        }
    })
}

fn collect_local_definition(
    db: &dyn DefDatabase,
    def_map: &DefMap,
    scope: &ItemScope,
) -> impl Iterator<Item = MemberDefinition> {
    let mut items = match scope {
        ItemScope::Implement(id) => def_map[id].declarations(),
        ItemScope::Class(id) => def_map[id].declarations(),
        ItemScope::Interface(id) => def_map[id].declarations(),
    }
    .iter()
    .peekable();

    std::iter::from_fn(move || {
        loop {
            match items.next()? {
                ItemDefId::ClauseId(id) => {
                    let def = db.clause_signature(*id);
                    let sig = (&def.name, def.arg_count, def.ellipsis, def.returns);
                    let mut family = vec![*id];

                    while let Some(ItemDefId::ClauseId(id)) = items.peek() {
                        let next_def = db.clause_signature(*id);
                        let next_sig = (
                            &next_def.name,
                            next_def.arg_count,
                            next_def.ellipsis,
                            next_def.returns,
                        );
                        if sig == next_sig {
                            family.push(*id);
                            items.next();
                        } else {
                            break;
                        }
                    }
                    return Some(MemberDefinition::ClauseFamily(family));
                }
                _ => continue, // Skip everything non-clause and try the next item def
            }
        }
    })
}
