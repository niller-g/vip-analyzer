use super::{
    ExprPtr, ExpressionStore, ExpressionStoreBuilder, ExpressionStoreSourceMap, StmtPtr, TypePtr,
    body::{Body, BodySourceMap},
};
use crate::{
    DefWithBodyId, Lookup,
    db::DefDatabase,
    hir::{
        AccessTerm, Annotation, ApplyArg, ApplyTerm, ArgAttribute, ElseIf, Expr, ExprId,
        ExprOrStmt, FlowArg, FlowPattern, FormalArg, Functor, FunctorArg, IfTerm, KeywordArg,
        MatchArm, Mode, Name, Params, RefTerm, Signature, Stmt, StmtId, TryHandler, VarName,
        type_ref::{TypeRef, TypeRefId},
    },
    nameres::ScopeRef,
    src::{HasSource, InFile},
};
use intern::Symbol;
use span::FileId;
use syntax::{
    AstNode, AstPtr, T,
    ast::{self, HasAttributes, HasFormalParamList, HasType},
};

pub(super) fn lower_body(db: &dyn DefDatabase, owner: DefWithBodyId) -> (Body, BodySourceMap) {
    let file_id = owner.file_id(db);
    let mut collector = ExprCollector::new(db, file_id);

    let (args, body, return_expr) = match owner {
        DefWithBodyId::ClauseId(clause_id) => {
            let c = clause_id.lookup(db);
            let src = c.source(db).value;

            (src.clause_args(), src.body_stmt(), src.expr())
        }
    };

    let args = args.map(|arg| collector.collect_expr(arg)).collect();
    let body_stmt = body.map(|stmt| collector.collect_stmt(stmt));
    let return_expr = return_expr.map(|expr| collector.collect_expr(expr));

    (
        Body { store: collector.store.finish(), args, body_stmt, return_expr },
        BodySourceMap { store: collector.source_map },
    )
}

pub(crate) fn lower_pred_params<P>(
    db: &dyn DefDatabase,
    file_id: FileId,
    param_owner: P,
) -> (ExpressionStore, ExpressionStoreSourceMap, Params)
where
    P: ast::HasPredParams + Clone,
{
    let mut collector = ExprCollector::new(db, file_id);
    let params = param_owner.pred_params();

    let Some(params) = params else {
        return (collector.store.finish(), collector.source_map, Default::default());
    };

    let params = match params {
        ast::PredParams::InlineParams(inline) => {
            let signature = collector.lower_inline_params(&inline, |collector| {
                inline
                    .return_param()
                    .map(|ret| collector.lower_type_opt(ret.type_arg().and_then(|ret| ret.ty())))
            });
            Params::Inline(signature)
        }
        ast::PredParams::RefTerm(ref_term) => match collector.lower_ref_term(&ref_term) {
            Some(ref_term) => Params::RefTerm(ref_term),
            None => Default::default(),
        },
    };

    (collector.store.finish(), collector.source_map, params)
}

pub(crate) fn lower_constructor_params(
    db: &dyn DefDatabase,
    file_id: FileId,
    constructor: ast::Constructor,
) -> (ExpressionStore, ExpressionStoreSourceMap, Signature) {
    let mut collector = ExprCollector::new(db, file_id);
    let params = constructor.inline_params();

    let Some(inline) = params else {
        return (collector.store.finish(), collector.source_map, Default::default());
    };

    let signature = collector.lower_inline_params(&inline, |collector| {
        let ret_type = lower_construction_ty(constructor).unwrap_or(TypeRef::Error);
        Some(collector.alloc_type_ref_desugared(ret_type))
    });

    (collector.store.finish(), collector.source_map, signature)
}

fn lower_construction_ty(constructor: ast::Constructor) -> Option<TypeRef> {
    let construction_ty = constructor.syntax().ancestors().find_map(|ancestor| {
        match ast::ClassItem::cast(ancestor) {
            Some(class) => class.scope_ref(),
            None => None,
        }
    })?;

    Some(TypeRef::Name(RefTerm::Scope(ScopeRef::from_ast(&construction_ty)?)))
}

fn annotation_from_args(
    formal_args: &[FormalArg],
    ellipsis: bool,
    suspending: bool,
    mode: Option<ast::Mode>,
) -> Annotation {
    let flow_args = formal_args
        .iter()
        .map(|formal_arg| {
            formal_arg
                .attributes
                .iter()
                .find_map(|attr| match attr {
                    ArgAttribute::In => Some(FlowArg::In),
                    ArgAttribute::Out => Some(FlowArg::Out),
                    _ => None,
                })
                .unwrap_or(FlowArg::In)
        })
        .collect();

    Annotation::new(
        suspending,
        mode.as_ref().map(Into::into),
        FlowPattern::new(flow_args, ellipsis),
    )
}

fn lower_annotations(
    annotations: impl Iterator<Item = (bool, Option<ast::Mode>, Option<Vec<ast::FlowPattern>>)>,
    args: &[FormalArg],
    ellipsis: bool,
) -> Vec<Annotation> {
    annotations
        .flat_map(|(suspending, mode, flow_patterns)| {
            lower_annotation(suspending, mode.as_ref(), flow_patterns, args, ellipsis)
        })
        .collect()
}

fn lower_annotation(
    suspending: bool,
    mode: Option<&ast::Mode>,
    flow_patterns: Option<Vec<ast::FlowPattern>>,
    args: &[FormalArg],
    ellipsis: bool,
) -> Vec<Annotation> {
    let mode = mode.map(Into::into);
    let flow_patterns = match flow_patterns {
        Some(flow_patterns) if !flow_patterns.is_empty() => flow_patterns
            .into_iter()
            .filter_map(|flow_pattern| match flow_pattern {
                ast::FlowPattern::AnyFlow(_) => Some(FlowPattern::AnyFlow),
                ast::FlowPattern::Flow(flow) => {
                    let (flow_args, ellipsis) = lower_flow_pattern(&flow)?;
                    Some(FlowPattern::new(flow_args, ellipsis))
                }
            })
            .collect(),
        _ => vec![FlowPattern::new(args.iter().map(|_| FlowArg::In).collect(), ellipsis)],
    };

    flow_patterns
        .into_iter()
        .map(|flow_pattern| Annotation::new(suspending, mode, flow_pattern))
        .collect()
}

impl From<&ast::Mode> for Mode {
    fn from(mode: &ast::Mode) -> Self {
        match mode.syntax().first_token() {
            Some(tok) => match tok.kind() {
                T![determ] => Mode::Determ,
                T![nondeterm] => Mode::Nondeterm,
                T![multi] => Mode::Multi,
                T![failure] => Mode::Failure,
                T![erroneous] => Mode::Erroneous,
                T![procedure] => Mode::Procedure,
                _ => Mode::default(),
            },
            _ => Mode::default(),
        }
    }
}

fn lower_flow_pattern(flow_pattern: &ast::Flow) -> Option<(Box<[FlowArg]>, bool)> {
    flow_pattern.flow_arg_list().map(|flow_arg_list| lower_flow_arg_list(flow_arg_list.flow_args()))
}

fn lower_flow_arg_list(
    flow_arg_list: impl Iterator<Item = ast::FlowArg>,
) -> (Box<[FlowArg]>, bool) {
    let mut ellipsis = false;

    let flow_args = flow_arg_list
        .filter_map(|flow_arg| match flow_arg {
            ast::FlowArg::Ellipsis(_) => {
                ellipsis = true;
                None
            }
            ast::FlowArg::FlowDirection(flow_dir) => {
                Some(if flow_dir.o_token().is_some() { FlowArg::Out } else { FlowArg::In })
            }
            ast::FlowArg::FunctorFlow(functor_flow) => {
                let functor_name = Name::new(functor_flow.ident_token()?.text());
                let (functor_flow_args, inner_ellipsis) =
                    lower_flow_pattern(&functor_flow.flow()?)?;
                ellipsis |= inner_ellipsis;
                Some(FlowArg::Compound { functor_name, functor_flow_args })
            }
            ast::FlowArg::ListFlow(list_flow) => {
                let flow_list = lower_list_flow(list_flow);
                Some(FlowArg::List(flow_list.into()))
            }
        })
        .collect();

    (flow_args, ellipsis)
}

fn lower_list_flow(list_flow: ast::ListFlow) -> Vec<FlowArg> {
    let Some(heads) = list_flow.flow_arg_list().map(|flow_arg_list| {
        let (flow_args, _) = lower_flow_arg_list(flow_arg_list.flow_args());
        flow_args.to_vec()
    }) else {
        return Vec::new();
    };

    let tail: Vec<FlowArg> = if let Some(tail) = list_flow.list_flow() {
        lower_list_flow(tail)
    } else if let Some(ident) = list_flow.ident_token() {
        let name = Name::new(ident.text());
        match name.as_str() {
            "i" => vec![FlowArg::In],
            "o" => vec![FlowArg::Out],
            _ => Vec::new(),
        }
    } else {
        Vec::new()
    };
    heads.into_iter().chain(tail).collect()
}

fn lower_arg_attribute(arg_attribute: ast::Expr) -> Option<ArgAttribute> {
    let (name, num) = match arg_attribute {
        ast::Expr::ApplyTerm(apply_term) => match apply_term.expr()? {
            ast::Expr::RefTerm(ref_term) => (
                ref_term.ident_token()?,
                apply_term.args().next().and_then(|arg| match arg {
                    ast::FunctorArg::Expr(ast::Expr::NumericLiteral(lit)) => {
                        // FIXME: This parsing of numeric literals should be done in a more correct way instead of manually here.
                        lit.int_number_token().and_then(|token| token.text().parse::<u32>().ok())
                    }
                    _ => None,
                }),
            ),
            _ => return None,
        },
        ast::Expr::RefTerm(ref_term) => (ref_term.ident_token()?, None),
        _ => return None,
    };
    let attr = match Name::new(name.text()).as_str() {
        "in" => ArgAttribute::In,
        "out" => ArgAttribute::Out,
        "formatstring" => ArgAttribute::FormatString,
        "inline" => ArgAttribute::Inline { size: num },
        "byvalue" => ArgAttribute::ByValue,
        "this" => ArgAttribute::This,
        _ => return None,
    };
    Some(attr)
}

pub(crate) fn lower_domain(
    db: &dyn DefDatabase,
    domain: InFile<ast::Domain>,
) -> (ExpressionStore, ExpressionStoreSourceMap, Box<[TypeRefId]>, TypeRefId) {
    let mut collector = ExprCollector::new(db, domain.file_id);

    let generics: Box<[TypeRefId]> =
        domain.value.generic_args().map(|t| collector.lower_type_opt(t.ty())).collect();

    let domain_def = domain
        .value
        .domain_def()
        .map(|domain_def| collector.lower_domain_def(domain_def, &generics))
        .unwrap_or_else(|| collector.alloc_error_type());

    (collector.store.finish(), collector.source_map, generics, domain_def)
}

pub(crate) fn lower_formal_params(
    db: &dyn DefDatabase,
    file_id: FileId,
    formal_params: impl Iterator<Item = ast::FormalParam>,
) -> (ExpressionStore, ExpressionStoreSourceMap, Box<[TypeRefId]>) {
    let mut expr_collector = ExprCollector::new(db, file_id);

    let types = formal_params.map(|p| expr_collector.lower_type_opt(p.ty())).collect();

    (expr_collector.store.finish(), expr_collector.source_map, types)
}

pub(crate) fn lower_functor(
    db: &dyn DefDatabase,
    functor: InFile<ast::Functor>,
) -> (ExpressionStore, ExpressionStoreSourceMap, Option<Functor>) {
    let mut expr_collector = ExprCollector::new(db, functor.file_id);
    let functor = expr_collector.lower_functor(&functor.value);
    (expr_collector.store.finish(), expr_collector.source_map, functor)
}

pub(crate) fn maybe_lower_type_ref(
    db: &dyn DefDatabase,
    type_ref: InFile<Option<ast::TypeKind>>,
) -> (ExpressionStore, ExpressionStoreSourceMap, Option<TypeRefId>) {
    let mut expr_collector = ExprCollector::new(db, type_ref.file_id);
    let ty_ref = type_ref.value.map(|node| expr_collector.lower_type(node));

    (expr_collector.store.finish(), expr_collector.source_map, ty_ref)
}

pub(crate) fn lower_type_ref(
    db: &dyn DefDatabase,
    type_ref: InFile<Option<ast::TypeKind>>,
) -> (ExpressionStore, ExpressionStoreSourceMap, TypeRefId) {
    let mut expr_collector = ExprCollector::new(db, type_ref.file_id);
    let type_ref = expr_collector.lower_type_opt(type_ref.value);
    (expr_collector.store.finish(), expr_collector.source_map, type_ref)
}

pub fn lower_ref_term(
    db: &dyn DefDatabase,
    ref_term: InFile<ast::RefTerm>,
) -> (ExpressionStore, ExpressionStoreSourceMap, Option<RefTerm>) {
    let mut expr_collector = ExprCollector::new(db, ref_term.file_id);
    let ref_term = expr_collector.lower_ref_term(&ref_term.value);
    (expr_collector.store.finish(), expr_collector.source_map, ref_term)
}

pub struct ExprCollector<'db> {
    #[expect(dead_code)]
    db: &'db dyn DefDatabase,
    file_id: FileId,
    pub store: ExpressionStoreBuilder,
    pub(crate) source_map: ExpressionStoreSourceMap,
}
impl<'db> ExprCollector<'db> {
    fn new(db: &'db dyn DefDatabase, file_id: FileId) -> ExprCollector<'db> {
        ExprCollector {
            db,
            file_id,
            store: ExpressionStoreBuilder::default(),
            source_map: ExpressionStoreSourceMap::default(),
        }
    }

    fn lower_inline_params(
        &mut self,
        inline_params: &ast::InlineParams,
        return_type: impl FnOnce(&mut ExprCollector<'db>) -> Option<TypeRefId>,
    ) -> Signature {
        let (formal_args, ellipsis) = inline_params.args();
        let params = self.lower_formal_args(formal_args);
        let ret_type = return_type(self);

        let annotations = lower_annotations(inline_params.annotations(), &params, ellipsis);
        let annotations = if annotations.is_empty() {
            vec![annotation_from_args(&params, ellipsis, false, None)]
        } else {
            annotations
        };
        debug_assert!(!annotations.is_empty());

        Signature::new(params, ret_type, annotations)
    }

    fn lower_formal_args(
        &mut self,
        formal_args: impl Iterator<Item = ast::FormalArg>,
    ) -> Box<[FormalArg]> {
        formal_args
            .map(|arg| {
                let attributes: Box<[_]> =
                    arg.attributes().filter_map(lower_arg_attribute).collect();
                let is_out_flow = attributes.iter().any(|attr| matches!(attr, ArgAttribute::Out));
                FormalArg::new(
                    self.lower_type_opt(arg.ty()),
                    if is_out_flow { None } else { arg.expr().map(|expr| self.collect_expr(expr)) }, // Out parameters are not allowed to have default values
                    attributes.into_iter(),
                )
            })
            .collect()
    }

    fn lower_formal_params(
        &mut self,
        formal_params: impl Iterator<Item = ast::FormalParam>,
    ) -> Box<[FormalArg]> {
        formal_params
            .map(|param| {
                FormalArg::new(
                    self.lower_type_opt(param.ty()),
                    None,
                    param.attributes().filter_map(lower_arg_attribute),
                )
            })
            .collect()
    }

    pub(crate) fn lower_functor(&mut self, functor: &ast::Functor) -> Option<Functor> {
        let name = functor.ty().and_then(|ty| match ty {
            ast::TypeKind::RefTerm(ref_term) => {
                assert!(ref_term.scope_ref().is_none());
                Some(Name::new(ref_term.ident_token()?.text()))
            }
            _ => None,
        })?;
        let params = functor
            .formal_arg_list()
            .into_iter()
            .flat_map(|list| list.formal_args())
            .map(|arg| {
                FormalArg::new(
                    self.lower_type_opt(arg.ty()),
                    arg.expr().map(|expr| self.collect_expr(expr)),
                    arg.attributes().filter_map(lower_arg_attribute),
                )
            })
            .collect();

        Some(Functor { name, params })
    }

    pub(crate) fn lower_ref_term(&mut self, ast: &ast::RefTerm) -> Option<RefTerm> {
        let scope_ref = ast.scope_ref().as_ref().and_then(ScopeRef::from_ast);
        let name = ast.ident_token().map(|ident| {
            let name = Name::new(ident.text());
            let generics = ast
                .generics()
                .into_iter()
                .flat_map(|generics| generics.type_args())
                .map(|t| self.lower_type_opt(t.ty()));
            (name, generics)
        });

        RefTerm::try_new(scope_ref, name)
    }

    pub(crate) fn lower_type(&mut self, node: ast::TypeKind) -> TypeRefId {
        let ty = match &node {
            ast::TypeKind::ListType(inner) => match inner.ty() {
                Some(ty) => TypeRef::List(self.lower_type(ty)),
                None => return self.alloc_error_type(),
            },
            ast::TypeKind::RefTerm(ref_term) => match self.lower_ref_term(ref_term) {
                Some(ref_term) => TypeRef::Name(ref_term),
                None => TypeRef::Error,
            },
            ast::TypeKind::ScopeType(scope_type) => match scope_type.var_token() {
                Some(token) => TypeRef::ScopeType(VarName::new(token.text())),
                None => TypeRef::Error,
            },
            ast::TypeKind::VarType(var_type) => match var_type.var_token() {
                Some(token) => match token.text() {
                    "_" => TypeRef::Wildcard,
                    text => TypeRef::VarType(VarName::new(text)),
                },
                None => TypeRef::Error,
            },
        };

        self.alloc_type_ref(ty, AstPtr::new(&node))
    }

    fn lower_domain_def(
        &mut self,
        domain_def: ast::DomainDef,
        _generics: &[TypeRefId],
    ) -> TypeRefId {
        match domain_def {
            ast::DomainDef::AliasDomain(alias) => self.lower_type_opt(alias.ty()),
            ast::DomainDef::FunctorDomain(ast) => {
                let functors =
                    ast.functors().filter_map(|functor| self.lower_functor(&functor)).collect();
                self.alloc_type_ref_desugared(TypeRef::Compound(functors))
            }
            ast::DomainDef::NumericDomain(_) => self.alloc_type_ref_desugared(TypeRef::Numeric),
            ast::DomainDef::PredicateDomain(domain) => {
                let (params, ellipsis) = domain.params();
                let params = self.lower_formal_params(params);
                let ret_type = domain
                    .return_param()
                    .and_then(|p| p.type_arg())
                    .map(|p| self.lower_type_opt(p.ty()));
                let (suspending, mode, flow_patterns) = domain.annotation();
                let annotations =
                    lower_annotation(suspending, mode.as_ref(), flow_patterns, &params, ellipsis);
                let annotations = if annotations.is_empty() {
                    vec![annotation_from_args(&params, ellipsis, suspending, mode)]
                } else {
                    annotations
                };
                debug_assert!(!annotations.is_empty());
                let signature = Signature::new(params, ret_type, annotations);
                self.alloc_type_ref_desugared(TypeRef::Fn(signature))
            }
        }
    }

    pub(crate) fn lower_type_opt(&mut self, node: Option<ast::TypeKind>) -> TypeRefId {
        match node {
            Some(node) => self.lower_type(node),
            None => self.alloc_error_type(),
        }
    }

    fn alloc_type_ref(&mut self, type_ref: TypeRef, node: TypePtr) -> TypeRefId {
        let id = self.store.types.alloc(type_ref);
        let src = InFile::new(self.file_id, node);
        self.source_map.types_map_back.insert(id, src);
        self.source_map.types_map.insert(src, id);
        id
    }

    fn alloc_type_ref_desugared(&mut self, type_ref: TypeRef) -> TypeRefId {
        self.store.types.alloc(type_ref)
    }

    fn alloc_error_type(&mut self) -> TypeRefId {
        self.store.types.alloc(TypeRef::Error)
    }

    fn collect_stmt(&mut self, stmt: ast::Stmt) -> StmtId {
        let syntax_ptr = AstPtr::new(&stmt);
        match stmt {
            ast::Stmt::BinStmt(bin_stmt) => {
                let rhs = self.collect_stmt_opt(bin_stmt.rhs());
                let lhs = self.collect_stmt_opt(bin_stmt.lhs());
                match bin_stmt.op() {
                    Some(op) => self.alloc_stmt(Stmt::BinStmt { lhs, op, rhs }, syntax_ptr),
                    None => self.missing_stmt(),
                }
            }
            ast::Stmt::AtomStmt(atom_stmt) => match atom_stmt {
                ast::AtomStmt::AccessTerm(access_term) => {
                    match self.collect_access_term(access_term) {
                        Ok(a) => self.alloc_stmt(Stmt::AccessTerm(a), syntax_ptr),
                        Err(_) => self.missing_stmt(),
                    }
                }
                ast::AtomStmt::ApplyTerm(apply_term) => {
                    let apply_term = self.collect_apply_term(apply_term);
                    self.alloc_stmt(Stmt::ApplyTerm(apply_term), syntax_ptr)
                }
                ast::AtomStmt::AssignStmt(assign_stmt) => {
                    let initializer = assign_stmt
                        .rhs()
                        .map(|rhs| match rhs {
                            ast::AssignRhs::Expr(expr) => self.collect_expr(expr),
                            ast::AssignRhs::Erroneous(_) => {
                                self.alloc_expr_desugared(Expr::Erroneous)
                            }
                        })
                        .unwrap_or_else(|| self.missing_expr());
                    let receiver = self.collect_expr_opt(assign_stmt.lhs());
                    self.alloc_stmt(Stmt::AssignStmt { receiver, initializer }, syntax_ptr)
                }
                ast::AtomStmt::CutStmt(_) => self.alloc_stmt(Stmt::Cut, syntax_ptr),
                ast::AtomStmt::ForeachStmt(foreach_stmt) => {
                    let iter = self.collect_stmt_opt(foreach_stmt.iteration_stmt());
                    let body = self.collect_stmt_opt(foreach_stmt.stmt());
                    self.alloc_stmt(Stmt::ForeachStmt { iter, body }, syntax_ptr)
                }
                ast::AtomStmt::IfTerm(if_term) => {
                    let if_term = self.collect_if_term(if_term);
                    self.alloc_stmt(Stmt::IfTerm(if_term), syntax_ptr)
                }
                ast::AtomStmt::InStmt(in_stmt) => {
                    let rhs = self.collect_expr_opt(in_stmt.rhs());
                    let lhs = self.collect_expr_opt(in_stmt.lhs());
                    self.alloc_stmt(Stmt::InStmt { lhs, rhs }, syntax_ptr)
                }
                ast::AtomStmt::MatchStmt(match_stmt) => {
                    let args = match_stmt
                        .args()
                        .map(|arg| match arg {
                            ast::ApplyArg::KeywordArg(keyword_arg) => {
                                self.collect_expr_opt(keyword_arg.expr())
                            }
                            ast::ApplyArg::Expr(expr) => self.collect_expr(expr),
                        })
                        .collect();
                    let arms = match_stmt
                        .case_arms()
                        .map(|case| MatchArm {
                            args: case
                                .exprs()
                                .into_iter()
                                .flat_map(|it| it.exprs())
                                .map(|it| self.collect_expr(it))
                                .collect(),
                            body: case
                                .clause_body()
                                .and_then(|it| it.stmt())
                                .map(|it| self.collect_stmt(it)),
                        })
                        .collect();
                    self.alloc_stmt(Stmt::MatchStmt { args, arms }, syntax_ptr)
                }
                ast::AtomStmt::ParenTerm(paren_term) => {
                    let src = InFile::new(self.file_id, syntax_ptr);
                    let inner = if let Some(inner) = paren_term.expr() {
                        ExprOrStmt::Expr(self.collect_expr(inner))
                    } else if let Some(inner) = paren_term.stmt() {
                        let inner = self.collect_stmt(inner);
                        self.source_map.stmt_map.insert(src, inner);
                        ExprOrStmt::Stmt(inner)
                    } else {
                        let inner = self.missing_stmt();
                        self.source_map.stmt_map.insert(src, inner);
                        ExprOrStmt::Stmt(inner)
                    };
                    self.alloc_stmt(Stmt::ParenTerm(inner), syntax_ptr)
                }
                ast::AtomStmt::RefTerm(ref_term) => self
                    .lower_ref_term(&ref_term)
                    .map(|it| self.alloc_stmt(Stmt::RefTerm(it), syntax_ptr))
                    .unwrap_or_else(|| self.missing_stmt()),
                ast::AtomStmt::RelationStmt(relation_stmt) => {
                    let rhs = self.collect_expr_opt(relation_stmt.rhs());
                    let lhs = self.collect_expr_opt(relation_stmt.lhs());
                    match relation_stmt.op() {
                        Some(op) => {
                            self.alloc_stmt(Stmt::RelationStmt { lhs, op, rhs }, syntax_ptr)
                        }
                        None => self.missing_stmt(),
                    }
                }
                ast::AtomStmt::TryStmt(try_stmt) => {
                    let body = self.collect_stmt_opt(try_stmt.stmt());
                    let handlers = try_stmt
                        .try_handlers()
                        .map(|handler| match handler {
                            ast::TryHandler::CatchHandler(catch) => TryHandler::Catch {
                                exception: self.collect_expr_opt(catch.expr()),
                                body: self.collect_stmt_opt(catch.stmt()),
                            },
                            ast::TryHandler::FinallyHandler(finally) => {
                                TryHandler::Finally { body: self.collect_stmt_opt(finally.stmt()) }
                            }
                        })
                        .collect();
                    self.alloc_stmt(Stmt::TryStmt { body, handlers }, syntax_ptr)
                }
            },
        }
    }
    fn collect_stmt_opt(&mut self, stmt: Option<ast::Stmt>) -> StmtId {
        match stmt {
            Some(stmt) => self.collect_stmt(stmt),
            None => self.missing_stmt(),
        }
    }

    fn collect_expr(&mut self, expr: ast::Expr) -> ExprId {
        let syntax_ptr = AstPtr::new(&expr);
        match expr {
            ast::Expr::AccessTerm(access_term) => self
                .collect_access_term(access_term)
                .map_or_else(|e| e, |a| self.alloc_expr(Expr::AccessTerm(a), syntax_ptr)),
            ast::Expr::ApplyTerm(apply_term) => {
                let apply_term = self.collect_apply_term(apply_term);
                self.alloc_expr(Expr::ApplyTerm(apply_term), syntax_ptr)
            }
            ast::Expr::BinExpr(bin_expr) => {
                let rhs = self.collect_expr_opt(bin_expr.rhs());
                let lhs = self.collect_expr_opt(bin_expr.lhs());
                match bin_expr.op() {
                    Some(op) => self.alloc_expr(Expr::BinExpr { lhs, op, rhs }, syntax_ptr),
                    None => self.missing_expr(),
                }
            }
            ast::Expr::BinInclude(_) => self.alloc_expr(Expr::BinInclude, syntax_ptr),
            ast::Expr::BinLiteral(_) => self.alloc_expr(Expr::BinLiteral, syntax_ptr),
            ast::Expr::ConsExpr(cons_expr) => {
                let heads = cons_expr.head_exprs().map(|arg| self.collect_expr(arg)).collect();
                let tail = self.collect_expr_opt(cons_expr.expr());
                self.alloc_expr(Expr::Cons { heads, tail }, syntax_ptr)
            }
            ast::Expr::Ellipsis(_) => self.alloc_expr(Expr::Ellipsis, syntax_ptr),
            ast::Expr::FindAllExpr(find_all_expr) => {
                let generator = self.collect_stmt_opt(find_all_expr.stmt());
                let element = self.collect_expr_opt(find_all_expr.expr());
                self.alloc_expr(Expr::FindAll { element, generator }, syntax_ptr)
            }
            ast::Expr::IfTerm(if_term) => {
                let if_term = self.collect_if_term(if_term);
                self.alloc_expr(Expr::IfTerm(if_term), syntax_ptr)
            }
            ast::Expr::LambdaExpr(lambda_expr) => {
                let args = lambda_expr.args().map(|arg| self.collect_expr(arg)).collect();
                let body = lambda_expr.stmt().map(|stmt| self.collect_stmt(stmt));
                let return_expr = lambda_expr.expr().map(|expr| self.collect_expr(expr));
                self.alloc_expr(Expr::Lambda { args, return_expr, body }, syntax_ptr)
            }
            ast::Expr::ListExpr(list_expr) => {
                let elements = list_expr.elements().map(|arg| self.collect_expr(arg)).collect();
                self.alloc_expr(Expr::List { elements }, syntax_ptr)
            }
            ast::Expr::NumericLiteral(numeric_literal) => {
                if numeric_literal.int_number_token().is_some() {
                    self.alloc_expr(Expr::Int, syntax_ptr)
                } else if numeric_literal.float_number_token().is_some() {
                    self.alloc_expr(Expr::Float, syntax_ptr)
                } else {
                    self.missing_expr()
                }
            }
            ast::Expr::ObjectExpr(object_expr) => match object_expr.scope_ref() {
                Some(object_ty) => {
                    let interface = match ScopeRef::from_ast(&object_ty) {
                        Some(scope) => TypeRef::Name(RefTerm::Scope(scope)),
                        None => TypeRef::Error,
                    };
                    let interface = self.alloc_type_ref_desugared(interface);
                    self.alloc_expr(Expr::ObjectExpr { interface }, syntax_ptr)
                }
                None => self.missing_expr(),
            },
            ast::Expr::ParenTerm(paren_term) => {
                let src = InFile::new(self.file_id, syntax_ptr);
                let inner = if let Some(inner) = paren_term.expr() {
                    let inner = self.collect_expr(inner);
                    self.source_map.expr_map.insert(src, inner);
                    ExprOrStmt::Expr(inner)
                } else if let Some(inner) = paren_term.stmt() {
                    ExprOrStmt::Stmt(self.collect_stmt(inner))
                } else {
                    let inner = self.missing_expr();
                    self.source_map.expr_map.insert(src, inner);
                    ExprOrStmt::Expr(inner)
                };
                self.alloc_expr(Expr::ParenTerm(inner), syntax_ptr)
            }
            ast::Expr::PrefixExpr(prefix_expr) => {
                let expr = self.collect_expr_opt(prefix_expr.expr());

                match prefix_expr.op() {
                    Some(op) => self.alloc_expr(Expr::PrefixExpr { op, expr }, syntax_ptr),
                    None => self.missing_expr(),
                }
            }
            ast::Expr::RefTerm(ref_term) => self
                .lower_ref_term(&ref_term)
                .map(|it| self.alloc_expr(Expr::RefTerm(it), syntax_ptr))
                .unwrap_or_else(|| self.missing_expr()),
            ast::Expr::ScopeType(scope_type) => {
                let type_ref = self.lower_type(ast::TypeKind::ScopeType(scope_type));
                self.alloc_expr(Expr::TypeRef(type_ref), syntax_ptr)
            }
            ast::Expr::StringInclude(_) => self.alloc_expr(Expr::StringInclude, syntax_ptr),
            ast::Expr::StringSeq(_) => self.alloc_expr(Expr::Str, syntax_ptr),
            ast::Expr::VarExpr(var_expr) => var_expr
                .var_token()
                .map(|t| {
                    let text = t.text();
                    let expr = match text {
                        "_" => Expr::Wildcard,
                        _ => Expr::Var(Symbol::intern(text)),
                    };
                    self.alloc_expr(expr, syntax_ptr)
                })
                .unwrap_or_else(|| self.missing_expr()),
        }
    }
    fn collect_expr_opt(&mut self, expr: Option<ast::Expr>) -> ExprId {
        match expr {
            Some(expr) => self.collect_expr(expr),
            None => self.missing_expr(),
        }
    }

    fn collect_if_term(&mut self, if_term: ast::IfTerm) -> IfTerm {
        let cond = self.collect_stmt_opt(if_term.cond_stmt());
        let then = if let Some(body_expr) = if_term.expr() {
            Some(ExprOrStmt::Expr(self.collect_expr(body_expr)))
        } else {
            if_term.stmt().map(|stmt| ExprOrStmt::Stmt(self.collect_stmt(stmt)))
        };
        let else_ifs = if_term
            .else_if_terms()
            .map(|else_if| {
                let cond = self.collect_stmt_opt(else_if.cond_stmt());
                let then = if let Some(body_expr) = else_if.expr() {
                    Some(ExprOrStmt::Expr(self.collect_expr(body_expr)))
                } else {
                    else_if.stmt().map(|stmt| ExprOrStmt::Stmt(self.collect_stmt(stmt)))
                };
                ElseIf { cond, then }
            })
            .collect();

        let else_ = if_term.else_term().map(|else_term| {
            if let Some(body_expr) = else_term.expr() {
                ExprOrStmt::Expr(self.collect_expr(body_expr))
            } else {
                ExprOrStmt::Stmt(self.collect_stmt(else_term.stmt().unwrap()))
            }
        });
        IfTerm { cond, then, else_ifs, else_ }
    }

    fn collect_access_term(&mut self, access_term: ast::AccessTerm) -> Result<AccessTerm, ExprId> {
        let expr = self.collect_expr_opt(access_term.expr());
        match access_term.ref_term().and_then(|ref_term| {
            self.lower_ref_term(&ref_term)
                .and_then(|it| it.member_or_name())
                .and_then(|it| it.not_generic())
        }) {
            Some(access) => Ok(AccessTerm { expr, access }),
            None => Err(expr),
        }
    }

    fn collect_apply_term(&mut self, apply_term: ast::ApplyTerm) -> ApplyTerm {
        let receiver = self.collect_expr_opt(apply_term.expr());
        let functor_original = apply_term.apply_arg().map(|arg| match arg {
            ast::ApplyArg::KeywordArg(keyword_arg) => ApplyArg::KeywordArg(KeywordArg {
                keyword: intern_keyword(&keyword_arg),
                value: self.collect_expr_opt(keyword_arg.expr()),
            }),
            ast::ApplyArg::Expr(expr) => ApplyArg::Expr(self.collect_expr(expr)),
        });
        let args = apply_term
            .args()
            .map(|arg| match arg {
                ast::FunctorArg::KeywordArg(keyword_arg) => FunctorArg::KeywordArg(KeywordArg {
                    keyword: intern_keyword(&keyword_arg),
                    value: self.collect_expr_opt(keyword_arg.expr()),
                }),
                ast::FunctorArg::AtomStmt(atom_stmt) => {
                    FunctorArg::AtomStmt(self.collect_stmt(ast::Stmt::AtomStmt(atom_stmt)))
                }
                ast::FunctorArg::Expr(expr) => FunctorArg::Expr(self.collect_expr(expr)),
            })
            .collect();

        ApplyTerm { receiver, args, functor_original }
    }
}

fn intern_keyword(keyword_arg: &ast::KeywordArg) -> Symbol {
    keyword_arg.var_token().map(|t| Symbol::intern(t.text())).unwrap_or_else(Symbol::empty)
}

impl ExprCollector<'_> {
    fn missing_stmt(&mut self) -> StmtId {
        self.alloc_stmt_desugared(Stmt::Missing)
    }
    fn alloc_stmt_desugared(&mut self, stmt: Stmt) -> StmtId {
        self.store.stmts.alloc(stmt)
    }
    fn alloc_stmt(&mut self, stmt: Stmt, ptr: StmtPtr) -> StmtId {
        let src = InFile::new(self.file_id, ptr);
        let id = self.store.stmts.alloc(stmt);
        self.source_map.stmt_map_back.insert(id, src);
        self.source_map.stmt_map.insert(src, id);
        id
    }

    fn missing_expr(&mut self) -> ExprId {
        self.alloc_expr_desugared(Expr::Missing)
    }
    fn alloc_expr_desugared(&mut self, expr: Expr) -> ExprId {
        self.store.exprs.alloc(expr)
    }
    fn alloc_expr(&mut self, expr: Expr, ptr: ExprPtr) -> ExprId {
        let src = InFile::new(self.file_id, ptr);
        let id = self.store.exprs.alloc(expr);
        self.source_map.expr_map_back.insert(id, src);
        self.source_map.expr_map.insert(src, id);
        id
    }
}
