//! Various traits that are implemented by ast nodes.
//!
//! The implementations are usually trivial, and live in generated.rs

use super::AstChildren;
use crate::ast::{self, AstNode, support};
use std::{iter, option};

pub trait HasType: AstNode {
    #[inline]
    fn ty(&self) -> Option<ast::TypeKind> {
        support::child::<ast::Type>(self.syntax()).and_then(|t| t.type_kind())
    }
}

pub trait HasPredParams: AstNode {
    #[inline]
    fn pred_params(&self) -> Option<ast::PredParams> {
        support::child(self.syntax())
    }
}

pub trait HasPropertyFlow: AstNode {
    /// Returns a tuple of booleans indicating whether the property has
    /// input and/or output flow.
    fn in_out_flow(&self) -> (bool, bool) {
        let flow: Option<ast::Flow> = support::child(self.syntax());

        flow.and_then(|flow| flow.flow_arg_list())
            .map(|flow_list| {
                let mut in_ = false;
                let mut out = false;
                for flow_arg in flow_list.flow_args() {
                    if let ast::FlowArg::FlowDirection(direction) = flow_arg {
                        if direction.i_token().is_some() {
                            in_ = true;
                        } else if direction.o_token().is_some() {
                            out = true;
                        }
                    }
                }
                (in_, out)
            })
            .unwrap_or((true, true))
    }
}
impl HasPropertyFlow for ast::Property {}
impl HasPropertyFlow for ast::ClassProperty {}

pub trait HasFormalParamList: AstNode {
    /// Returns an iterator over the normal parameters and a boolean
    /// indicating whether there is an ellipsis at the end.
    fn params(&self) -> (impl Iterator<Item = ast::FormalParam> + '_, bool) {
        let formal_params: Option<ast::FormalParamList> = support::child(self.syntax());
        let ellipsis = formal_params.as_ref().is_some_and(|p| p.dotdotdot_token().is_some());

        (formal_params.into_iter().flat_map(|p| p.formal_params()), ellipsis)
    }
}

pub trait HasItems: AstNode {
    // /// Returns all items and recurses into conditional directives like `#if _ #then recursive_here #endif`.
    fn items_recursively(&self) -> impl Iterator<Item = ast::Item> {
        let rec_item: AstChildren<ast::RecItem> = support::children(self.syntax());

        rec_item.flat_map(|rec_item| match rec_item {
            ast::RecItem::Item(item) => vec![item],
            ast::RecItem::CondItem(cond_item) => cond_item
                .items_recursively()
                .chain(
                    cond_item
                        .else_item()
                        .into_iter()
                        .flat_map(|else_item| else_item.items_recursively().collect::<Vec<_>>()),
                )
                .collect(),
        })
    }
}

pub trait HasImplementSections: AstNode {
    /// Returns all implement sections and recurses into conditional directives like `#if _ #then recursive_here #endif`.
    fn implement_sections(&self) -> impl Iterator<Item = ast::ImplementSection> {
        let implement_sections: Option<ast::ImplementSections> = support::child(self.syntax());

        implement_sections.map(|s| s.list()).into_iter().flatten().flat_map(|s| match s {
            ast::RecImplementSection::ImplementSection(s) => vec![s],
            ast::RecImplementSection::CondImplementSection(cond) => cond
                .implement_sections()
                .chain(
                    cond.else_if_sections()
                        .flat_map(|s| s.implement_sections().collect::<Vec<_>>()),
                )
                .chain(
                    cond.else_section()
                        .into_iter()
                        .flat_map(|s| s.implement_sections().collect::<Vec<_>>()),
                )
                .collect::<Vec<_>>(),
        })
    }
}
pub trait HasClassSections: AstNode {
    /// Returns all class sections and recurses into conditional directives like `#if _ #then recursive_here #endif`.
    fn class_sections(&self) -> impl Iterator<Item = ast::ClassSection> {
        let class_sections: Option<ast::ClassSections> = support::child(self.syntax());

        class_sections.map(|s| s.list()).into_iter().flatten().flat_map(|s| match s {
            ast::RecClassSection::ClassSection(s) => vec![s],
            ast::RecClassSection::CondClassSection(cond) => cond
                .class_sections()
                .chain(cond.else_if_sections().flat_map(|s| s.class_sections().collect::<Vec<_>>()))
                .chain(
                    cond.else_section()
                        .into_iter()
                        .flat_map(|s| s.class_sections().collect::<Vec<_>>()),
                )
                .collect::<Vec<_>>(),
        })
    }
}
pub trait HasInterfaceSections: AstNode {
    /// Returns all interface sections and recurses into conditional directives like `#if _ #then recursive_here #endif`.
    fn interface_sections(&self) -> impl Iterator<Item = ast::InterfaceSection> {
        let interface_sections: Option<ast::InterfaceSections> = support::child(self.syntax());

        interface_sections.map(|s| s.list()).into_iter().flatten().flat_map(|s| match s {
            ast::RecInterfaceSection::InterfaceSection(s) => vec![s],
            ast::RecInterfaceSection::CondInterfaceSection(cond) => cond
                .interface_sections()
                .chain(
                    cond.else_if_sections()
                        .flat_map(|s| s.interface_sections().collect::<Vec<_>>()),
                )
                .chain(
                    cond.else_section()
                        .into_iter()
                        .flat_map(|s| s.interface_sections().collect::<Vec<_>>()),
                )
                .collect::<Vec<_>>(),
        })
    }
}

pub trait HasScopeNameDecl: AstNode {
    #[inline]
    fn scope_name_decl(&self) -> Option<ast::ScopeNameDecl> {
        support::child(self.syntax())
    }
}

pub trait HasSupportsQualifications: AstNode {
    fn supports_qualifications(&self) -> impl Iterator<Item = ast::ScopeRef> + '_;
}
impl HasSupportsQualifications for ast::ImplementItem {
    fn supports_qualifications(&self) -> impl Iterator<Item = ast::ScopeRef> + '_ {
        self.qualifications()
            .filter_map(|qual| match qual {
                ast::ImplementQualification::SupportsQualifications(qual) => Some(qual),
                ast::ImplementQualification::OpenQualifications(_)
                | ast::ImplementQualification::InheritsQualifications(_) => None,
            })
            .flat_map(|qual| qual.scope_refs())
    }
}
impl HasSupportsQualifications for ast::InterfaceItem {
    fn supports_qualifications(&self) -> impl Iterator<Item = ast::ScopeRef> + '_ {
        self.qualifications()
            .filter_map(|qual| match qual {
                ast::InterfaceQualification::SupportsQualifications(qual) => Some(qual),
                ast::InterfaceQualification::OpenQualifications(_) => None,
            })
            .flat_map(|qual| qual.scope_refs())
    }
}

pub trait HasOpenQualifications: AstNode {
    fn open_qualifications(&self) -> impl Iterator<Item = ast::OpenQualification> + '_;
}
impl HasOpenQualifications for ast::ImplementItem {
    fn open_qualifications(&self) -> impl Iterator<Item = ast::OpenQualification> + '_ {
        self.qualifications()
            .filter_map(|qual| match qual {
                ast::ImplementQualification::OpenQualifications(qual) => Some(qual),
                ast::ImplementQualification::SupportsQualifications(_)
                | ast::ImplementQualification::InheritsQualifications(_) => None,
            })
            .flat_map(|qual| qual.open_qualifications())
    }
}
impl HasOpenQualifications for ast::InterfaceItem {
    fn open_qualifications(&self) -> impl Iterator<Item = ast::OpenQualification> + '_ {
        self.qualifications()
            .filter_map(|qual| match qual {
                ast::InterfaceQualification::OpenQualifications(qual) => Some(qual),
                ast::InterfaceQualification::SupportsQualifications(_) => None,
            })
            .flat_map(|qual| qual.open_qualifications())
    }
}
impl HasOpenQualifications for ast::ClassItem {
    fn open_qualifications(&self) -> impl Iterator<Item = ast::OpenQualification> + '_ {
        self.qualifications().into_iter().flat_map(|qual| qual.open_qualifications())
    }
}

pub trait HasStringSeq: AstNode {
    #[inline]
    fn string_value(&self) -> Option<String> {
        support::child(self.syntax()).and_then(|s: ast::StringSeq| s.value())
    }
}

pub trait HasAttributes: AstNode {
    #[inline]
    fn attributes(&self) -> impl Iterator<Item = ast::Expr> {
        let attributes: Option<ast::AttributeList> = support::child(self.syntax());
        attributes.and_then(|a| a.exprs()).into_iter().flat_map(|s| s.exprs())
    }
}

macro_rules! impl_children_iter {
    ( $( ($section_type:ty, $fn_name:ident() where Item = $it:ty) ),+ $(,)?) => {
        $(
            impl IntoIterator for &$section_type {
                type Item = $it;
                type IntoIter = ast::AstChildren<Self::Item>;

                #[inline]
                fn into_iter(self) -> Self::IntoIter {
                    self.$fn_name()
                }
            }
        )*
    };
}
impl_children_iter![
    (ast::PredicatesSection, predicates() where Item = ast::Predicate),
    (ast::ClassPredicatesSection, class_predicates() where Item = ast::ClassPredicate),
    (ast::ConstructorsSection, constructors() where Item = ast::Constructor),
    (ast::PropertiesSection, properties() where Item = ast::Property),
    (ast::ClassPropertiesSection, class_properties() where Item = ast::ClassProperty),
    (ast::FactsSection, facts() where Item = ast::Fact),
    (ast::ClassFactsSection, class_facts() where Item = ast::ClassFact),
    (ast::ClausesSection, clauses() where Item = ast::ClauseKind),
    (ast::ConstantsSection, constants() where Item = ast::Constant),
    (ast::DomainsSection, domains() where Item = ast::Domain),
    (ast::ArityList, arities() where Item = ast::Arity),
    (ast::Exprs, exprs() where Item = ast::Expr),
];

macro_rules! impl_list_element_iter {
    ( $( ($section_type:ty, $get_elements:ident() -> $list_type:ty where Item = $element_type:ty) ),+ $(,)?) => {
        $(
            impl IntoIterator for &$section_type {
                type Item = $element_type;
                type IntoIter = iter::Flatten<option::IntoIter<ast::AstChildren<Self::Item>>>;

                #[inline]
                fn into_iter(self) -> Self::IntoIter {
                    let children: Option<$list_type> = support::child(self.syntax());
                    children.map(|s| s.$get_elements()).into_iter().flatten()
                }
            }
        )*
    };
}
impl_list_element_iter![
    (ast::DelegateSection, delegates() -> ast::Delegates where Item = ast::Delegate),
    (ast::ResolveSection, resolutions() -> ast::Resolutions where Item = ast::Resolution),
    (ast::PredicatesFromSection, arities() -> ast::ArityList where Item = ast::Arity),
    (ast::PropertiesFromSection, arities() -> ast::ArityList where Item = ast::Arity),
];
