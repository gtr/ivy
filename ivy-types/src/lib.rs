#![allow(clippy::result_large_err)]

pub mod env;
pub mod error;
pub mod exhaustiveness;
pub mod infer;
pub mod registry;
pub mod subst;
pub mod types;
pub mod unify;
pub use env::{TypeEnv, TypeVarGen};
pub use error::{TypeError, TypeErrorKind, TypeResult};
pub use infer::TypeChecker;
use ivy_parse::ModuleLoader;
use ivy_syntax::decl::{Constraint, Decl, FnBody, FnDecl, ImportKind, TraitItem, TypeBody};
use ivy_syntax::pattern::Pattern;
use ivy_syntax::types::TypeExpr;
use ivy_syntax::{collect_public_names, Ident, Program, Span, Spanned};
pub use registry::{ImplInfo, TraitInfo, TypeRegistry, VariantInfo};
use std::collections::{HashMap, HashSet};
use std::mem;
pub use subst::Subst;
pub use types::{Scheme, TraitConstraint, Type, TypeVar};
pub use unify::unify;

/// Type check an entire program (without import support, use check_program_with_env for imports).
pub fn check_program(program: &Program) -> TypeResult<()> {
    let mut env = TypeEnv::with_builtins();
    let mut checker = TypeChecker::new();
    let mut loader = ModuleLoader::new(vec![]);

    for decl in &program.declarations {
        check_decl(&mut checker, decl, &mut env, &mut loader)?;
    }

    Ok(())
}

/// Type check a program with a given type environment and search paths for imports.
pub fn check_program_with_env(
    program: &Program,
    checker: &mut TypeChecker,
    env: &mut TypeEnv,
    loader: &mut ModuleLoader,
) -> TypeResult<()> {
    for decl in &program.declarations {
        check_decl(checker, decl, env, loader)?;
    }
    Ok(())
}

/// If the error is a Mismatch without an `expected_span`, attach one.
pub(crate) fn add_expected_span(err: TypeError, expected_span: ivy_syntax::Span) -> TypeError {
    match err {
        TypeError::Mismatch {
            expected,
            found,
            span,
            expected_span: None,
        } => TypeError::mismatch_at(expected, found, span, expected_span),
        other => other,
    }
}

/// Type check a single declaration
fn check_decl(
    checker: &mut TypeChecker,
    decl: &Spanned<Decl>,
    env: &mut TypeEnv,
    loader: &mut ModuleLoader,
) -> TypeResult<()> {
    match &decl.node {
        Decl::TypeSig { name, ty, .. } => {
            let mut scope = HashMap::new();
            let sig_ty = checker.type_expr_to_type_scoped(&ty.node, env, Some(&mut scope));
            let final_ty = checker.finalize(&sig_ty);
            // Apply the substitution to the env so generalization sees solved
            // vars correctly (otherwise constraints would reference stale vars).
            *env = env.apply(&checker.subst);
            let scheme = env.generalize(&final_ty);
            env.insert(name.name.clone(), scheme);
            Ok(())
        }
        Decl::Let { pattern, value, ty, .. } => {
            let existing_scheme = if let Pattern::Var(ident) = &pattern.node {
                env.get(&ident.name).cloned()
            } else {
                None
            };

            let value_ty = checker.infer(value, env)?;
            if let Some(ann) = ty {
                let mut scope = HashMap::new();
                let ann_ty = checker.type_expr_to_type_scoped(&ann.node, env, Some(&mut scope));
                // unify(expected, found): annotation is the expected type; value is what we got.
                // Span points at the value (the wrong thing); enrich with the annotation span.
                unify::unify_with_subst(&ann_ty, &value_ty, &mut checker.subst, value.span)
                    .map_err(|e| add_expected_span(e, ann.span))?;
            }

            if let Some(existing) = existing_scheme {
                let existing_ty = checker.instantiate(&existing, decl.span);
                unify::unify_with_subst(&value_ty, &existing_ty, &mut checker.subst, decl.span)?;
            }

            if let Pattern::Var(ident) = &pattern.node {
                let final_ty = checker.finalize(&value_ty);
                *env = env.apply(&checker.subst);
                let env_vars = env.free_vars();
                let ty_vars: Vec<TypeVar> = final_ty.free_vars().difference(&env_vars).copied().collect();
                let (attached, deferred) = discharge_at_boundary(checker, &ty_vars, &env_vars)?;
                checker.constraints = deferred;
                let scheme = if ty_vars.is_empty() && attached.is_empty() {
                    Scheme::mono(final_ty)
                } else {
                    Scheme::poly_with_constraints(ty_vars, attached, final_ty)
                };
                env.insert(ident.name.clone(), scheme);
            } else {
                let env_vars = env.free_vars();
                let (attached, deferred) = discharge_at_boundary(checker, &[], &env_vars)?;
                if !attached.is_empty() {
                    return Err(TypeError::ambiguous_constraint(attached[0].clone(), decl.span));
                }
                checker.constraints = deferred;
            }
            Ok(())
        }
        Decl::Fn(fn_decl) => check_fn_decl(checker, fn_decl, env),
        Decl::Type { name, params, body, .. } => {
            register_type_constructors(name, params, body, env, checker);
            Ok(())
        }
        Decl::Import { path, kind } => check_import(checker, env, loader, path, kind),
        Decl::Trait {
            name,
            params,
            items,
            span,
            ..
        } => check_trait_decl(checker, env, name, params, items, *span),
        Decl::Impl {
            trait_name,
            for_type,
            where_clause,
            methods,
            span,
        } => check_impl_decl(checker, env, trait_name, for_type, where_clause, methods, *span),
        // TODO(gtr): modules...
        _ => Ok(()),
    }
}

fn check_trait_decl(
    checker: &mut TypeChecker,
    env: &mut TypeEnv,
    name: &Ident,
    params: &[Ident],
    items: &[TraitItem],
    span: Span,
) -> TypeResult<()> {
    if params.len() != 1 {
        return Err(TypeError::multi_param_trait_unsupported(&name.name, params.len(), span));
    }
    let param_name = params[0].name.clone();
    let param_var = checker.fresh_var();
    let param_ty = Type::Var(param_var);

    let trait_constraint = TraitConstraint {
        trait_name: name.name.clone(),
        type_args: vec![param_ty.clone()],
    };

    let mut method_schemes: HashMap<String, Scheme> = HashMap::new();
    let mut default_impls: HashMap<String, FnDecl> = HashMap::new();

    for item in items {
        match item {
            TraitItem::Signature { name: m_name, ty, .. } => {
                let mut scope: HashMap<String, Type> = HashMap::new();
                scope.insert(param_name.clone(), param_ty.clone());
                let method_ty = checker.type_expr_to_type_scoped(&ty.node, env, Some(&mut scope));
                let scheme = Scheme::poly_with_constraints(vec![param_var], vec![trait_constraint.clone()], method_ty);
                method_schemes.insert(m_name.name.clone(), scheme);
            }
            TraitItem::DefaultImpl(fn_decl) => {
                let mut scope: HashMap<String, Type> = HashMap::new();
                scope.insert(param_name.clone(), param_ty.clone());
                let mut method_ty = match &fn_decl.return_ty {
                    Some(rt) => checker.type_expr_to_type_scoped(&rt.node, env, Some(&mut scope)),
                    None => checker.fresh_type(),
                };
                for param in fn_decl.params.iter().rev() {
                    let p_ty = match &param.ty {
                        Some(ann) => checker.type_expr_to_type_scoped(&ann.node, env, Some(&mut scope)),
                        None => checker.fresh_type(),
                    };
                    method_ty = Type::fun(p_ty, method_ty);
                }
                let scheme = Scheme::poly_with_constraints(vec![param_var], vec![trait_constraint.clone()], method_ty);
                method_schemes.insert(fn_decl.name.name.clone(), scheme);
                default_impls.insert(fn_decl.name.name.clone(), fn_decl.clone());
            }
        }
    }

    for (m_name, scheme) in &method_schemes {
        env.insert(m_name.clone(), scheme.clone());
    }

    let info = TraitInfo {
        name: name.name.clone(),
        param: param_var,
        methods: method_schemes,
        default_impls,
        span,
    };
    checker.registry.register_trait(info);
    Ok(())
}

fn check_impl_decl(
    checker: &mut TypeChecker,
    env: &mut TypeEnv,
    trait_name: &Ident,
    for_type: &Spanned<TypeExpr>,
    where_clause: &[Constraint],
    methods: &[Spanned<FnDecl>],
    span: Span,
) -> TypeResult<()> {
    let trait_info = match checker.registry.get_trait(&trait_name.name) {
        Some(info) => info.clone(),
        None => return Err(TypeError::unknown_trait(&trait_name.name, trait_name.span)),
    };

    let mut head_scope: HashMap<String, Type> = HashMap::new();
    let head = checker.type_expr_to_type_scoped(&for_type.node, env, Some(&mut head_scope));

    if matches!(head, Type::Var(_)) {
        return Err(TypeError::blanket_impl_unsupported(&trait_name.name, for_type.span));
    }

    let mut where_constraints = Vec::with_capacity(where_clause.len());
    for c in where_clause {
        let arg_ty = checker.type_expr_to_type_scoped(&c.type_arg.node, env, Some(&mut head_scope));
        if !checker.registry.is_trait(&c.trait_name.name) {
            return Err(TypeError::unknown_trait(&c.trait_name.name, c.trait_name.span));
        }
        where_constraints.push(TraitConstraint {
            trait_name: c.trait_name.name.clone(),
            type_args: vec![arg_ty],
        });
    }

    let existing_heads: Vec<(Type, Span)> = checker
        .registry
        .get_impls(&trait_name.name)
        .iter()
        .map(|i| (i.head.clone(), i.span))
        .collect();
    for (existing_head, existing_span) in &existing_heads {
        if heads_overlap(checker, &head, existing_head) {
            return Err(TypeError::overlapping_impls(
                &trait_name.name,
                head.normalize(),
                existing_head.normalize(),
                span,
                *existing_span,
            ));
        }
    }

    let mut provided: HashSet<String> = HashSet::new();
    for method in methods {
        let m_name = &method.node.name.name;
        let trait_method_scheme = trait_info
            .methods
            .get(m_name)
            .ok_or_else(|| TypeError::unknown_method(&trait_name.name, m_name, method.node.name.span))?;

        // TODO(gtr): the snapshot/restore below is skipped on early `?` returns but we should
        // switch to a scope_guard pattern when error recovery is introduced
        let prev_subst = checker.subst.clone();
        let prev_constraints = mem::take(&mut checker.constraints);
        let prior_assumed_len = checker.assumed_constraints.len();

        let mut rename: HashMap<TypeVar, Type> = HashMap::new();
        for v in head.free_vars() {
            rename.insert(v, checker.fresh_type());
        }
        let method_head = substitute_type_in(&head, &rename);
        let method_where: Vec<TraitConstraint> = where_constraints
            .iter()
            .map(|c| TraitConstraint {
                trait_name: c.trait_name.clone(),
                type_args: c.type_args.iter().map(|t| substitute_type_in(t, &rename)).collect(),
            })
            .collect();
        for c in &method_where {
            checker.assumed_constraints.push(c.clone());
        }
        checker.assumed_constraints.push(TraitConstraint {
            trait_name: trait_name.name.clone(),
            type_args: vec![method_head.clone()],
        });

        let mut specialize: HashMap<TypeVar, Type> = HashMap::new();
        specialize.insert(trait_info.param, method_head.clone());
        let expected_ty = substitute_type_in(&trait_method_scheme.ty, &specialize);

        let actual_ty = check_impl_method(checker, env, &method.node)?;
        unify::unify_with_subst(&expected_ty, &actual_ty, &mut checker.subst, method.node.name.span).map_err(|_| {
            TypeError::method_signature_mismatch(
                &trait_name.name,
                m_name,
                checker.finalize(&expected_ty),
                checker.finalize(&actual_ty),
                method.node.name.span,
            )
        })?;

        let collected = mem::take(&mut checker.constraints);
        for (c, c_span) in collected {
            let resolved = TraitConstraint {
                trait_name: c.trait_name,
                type_args: c.type_args.iter().map(|t| checker.subst.apply(t)).collect(),
            };
            try_discharge(checker, &resolved, c_span)?;
        }

        checker.subst = prev_subst;
        checker.constraints = prev_constraints;
        checker.assumed_constraints.truncate(prior_assumed_len);

        provided.insert(m_name.clone());
    }

    let mut trait_method_names: Vec<&String> = trait_info.methods.keys().collect();
    trait_method_names.sort();
    for m_name in trait_method_names {
        if provided.contains(m_name) {
            continue;
        }
        if !trait_info.default_impls.contains_key(m_name) {
            return Err(TypeError::missing_method(&trait_name.name, m_name, span));
        }
    }

    let head_vars = method_head_vars(&head);
    let impl_info = ImplInfo {
        trait_name: trait_name.name.clone(),
        head,
        head_vars,
        where_constraints,
        span,
    };
    checker.registry.register_impl(impl_info);
    Ok(())
}

fn method_head_vars(head: &Type) -> Vec<TypeVar> {
    let mut vars: Vec<TypeVar> = head.free_vars().into_iter().collect();
    vars.sort_by_key(|v| v.0);
    vars
}

/// Type-check a single impl method body
///
/// Returns its inferred function type
fn check_impl_method(checker: &mut TypeChecker, env: &TypeEnv, fn_decl: &FnDecl) -> TypeResult<Type> {
    let mut param_types = Vec::new();
    let mut bindings = Vec::new();
    let mut type_var_scope: HashMap<String, Type> = HashMap::new();

    for param in &fn_decl.params {
        let ty = match &param.ty {
            Some(ann) => checker.type_expr_to_type_scoped(&ann.node, env, Some(&mut type_var_scope)),
            None => checker.fresh_type(),
        };
        let pattern_bindings = checker.infer_pattern(&param.pattern, &ty, env)?;
        bindings.extend(pattern_bindings);
        param_types.push(ty);
    }

    let body_env = env.extend(bindings);
    let body_ty = match &fn_decl.body {
        FnBody::Expr(expr) => checker.infer(expr, &body_env)?,
        FnBody::Guards(guards) => {
            let mut result_ty = None;
            for guard in guards {
                let cond_ty = checker.infer(&guard.guard, &body_env)?;
                unify::unify_with_subst(&cond_ty, &Type::Bool, &mut checker.subst, guard.guard.span)?;
                let g_body_ty = checker.infer(&guard.body, &body_env)?;
                match &result_ty {
                    Some(t) => {
                        unify::unify_with_subst(t, &g_body_ty, &mut checker.subst, guard.span)?;
                    }
                    None => result_ty = Some(g_body_ty),
                }
            }
            result_ty.unwrap_or(Type::Unit)
        }
    };

    if let Some(ann) = &fn_decl.return_ty {
        let ann_ty = checker.type_expr_to_type_scoped(&ann.node, env, Some(&mut type_var_scope));
        unify::unify_with_subst(&ann_ty, &body_ty, &mut checker.subst, fn_decl.span)?;
    }

    let mut fn_ty = body_ty;
    for p in param_types.into_iter().rev() {
        fn_ty = Type::fun(p, fn_ty);
    }
    Ok(fn_ty)
}

// TODO(gtr): a `seen: HashSet<(String, Type)>` cycle detector would catch circular impls more precisely
const MAX_IMPL_RECURSION: usize = 64;

fn find_impl(checker: &mut TypeChecker, constraint: &TraitConstraint) -> bool {
    find_impl_at(checker, constraint, 0)
}

/// Search for an impl satisfying `constraint`
///
/// Returns true if we found it
fn find_impl_at(checker: &mut TypeChecker, constraint: &TraitConstraint, depth: usize) -> bool {
    if depth > MAX_IMPL_RECURSION {
        return false;
    }
    let trait_name = constraint.trait_name.clone();
    let Some(want) = constraint.type_args.first().cloned() else {
        return false;
    };
    let impls: Vec<ImplInfo> = checker.registry.get_impls(&trait_name).to_vec();

    for info in impls {
        let mut fresh: HashMap<TypeVar, Type> = HashMap::new();
        for v in &info.head_vars {
            fresh.insert(*v, checker.fresh_type());
        }
        let head_ty = substitute_type_in(&info.head, &fresh);

        let saved = checker.subst.clone();
        if unify::unify_with_subst(&head_ty, &want, &mut checker.subst, Span::default()).is_err() {
            checker.subst = saved;
            continue;
        }

        let mut all_ok = true;
        for c in &info.where_constraints {
            let resolved = TraitConstraint {
                trait_name: c.trait_name.clone(),
                type_args: c
                    .type_args
                    .iter()
                    .map(|t| checker.subst.apply(&substitute_type_in(t, &fresh)))
                    .collect(),
            };
            if checker.assumed_constraints.iter().any(|a| a.covers(&resolved)) {
                continue;
            }
            if !find_impl_at(checker, &resolved, depth + 1) {
                all_ok = false;
                break;
            }
        }
        if all_ok {
            return true;
        }
        checker.subst = saved;
    }
    false
}

fn substitute_type_in(ty: &Type, mapping: &HashMap<TypeVar, Type>) -> Type {
    Subst::from_mappings(mapping.clone()).apply(ty)
}

/// Two impl heads "overlap" if some assignment unifies them. Both sides are alpha-renamed to fresh vars first so
/// var-id collisions between separately
fn heads_overlap(checker: &mut TypeChecker, h1: &Type, h2: &Type) -> bool {
    freshen_with(checker, h1).overlaps(&freshen_with(checker, h2))
}

fn freshen_with(checker: &mut TypeChecker, ty: &Type) -> Type {
    let mut mapping: HashMap<TypeVar, Type> = HashMap::new();
    for v in ty.free_vars() {
        mapping.insert(v, checker.fresh_type());
    }
    substitute_type_in(ty, &mapping)
}

/// Partition accumulated constraints at a decl boundary into:
/// (a) constraints to attach to the scheme being generalized (mentioning vars in `generalized_vars`)
/// (b) constraints to defer back to the outer scope (mentioning outer-scope vars)
type DischargeOutcome = (Vec<TraitConstraint>, Vec<(TraitConstraint, Span)>);

fn discharge_at_boundary(
    checker: &mut TypeChecker,
    generalized_vars: &[TypeVar],
    env_vars: &HashSet<TypeVar>,
) -> TypeResult<DischargeOutcome> {
    let mut attached: Vec<TraitConstraint> = Vec::new();
    let mut deferred: Vec<(TraitConstraint, Span)> = Vec::new();
    let pending = mem::take(&mut checker.constraints);
    let mut iter = pending.into_iter();

    while let Some((c, span)) = iter.next() {
        let resolved = TraitConstraint {
            trait_name: c.trait_name,
            type_args: c.type_args.iter().map(|t| checker.subst.apply(t)).collect(),
        };
        let free: HashSet<TypeVar> = resolved.type_args.iter().flat_map(Type::free_vars).collect();

        let result = if free.is_empty() {
            try_discharge(checker, &resolved, span).map(|_| ())
        } else if free.iter().any(|v| generalized_vars.contains(v)) {
            if !attached.iter().any(|c| c == &resolved) {
                attached.push(resolved);
            }
            Ok(())
        } else if free.iter().any(|v| env_vars.contains(v)) {
            deferred.push((resolved, span));
            Ok(())
        } else {
            Err(TypeError::ambiguous_constraint(resolved, span))
        };

        if let Err(e) = result {
            checker.constraints = iter.collect();
            return Err(e);
        }
    }

    Ok((attached, deferred))
}

/// Try to discharge a ground (or assumed) constraint
///
/// Returns Ok if an assumed constraint matches or an impl is found, Err with `NoImplFound` otherwise
fn try_discharge(checker: &mut TypeChecker, c: &TraitConstraint, span: Span) -> TypeResult<()> {
    if checker.assumed_constraints.iter().any(|a| a.covers(c)) {
        return Ok(());
    }
    if find_impl(checker, c) {
        return Ok(());
    }
    let ty = c.type_args.first().cloned().unwrap_or(Type::Unit);
    Err(TypeError::no_impl_found(&c.trait_name, ty.normalize(), span))
}

/// Type check an import declaration.
fn check_import(
    checker: &mut TypeChecker,
    env: &mut TypeEnv,
    loader: &mut ModuleLoader,
    path: &[Ident],
    kind: &ImportKind,
) -> TypeResult<()> {
    if path.is_empty() {
        return Ok(());
    }

    let span = path[0].span;
    let path_strings: Vec<String> = path.iter().map(|id| id.name.clone()).collect();
    let module_name = path_strings.join(".");

    // Check if module already type-checked
    if env.get_module(&module_name).is_some() {
        return handle_import_kind(env, &module_name, kind);
    }

    // Check for circular import
    if checker.loaded_modules.contains(&module_name) {
        let cycle: Vec<String> = checker.loaded_modules.iter().cloned().collect();
        return Err(TypeError::circular_import(&module_name, cycle, span));
    }

    // Load and parse via shared loader
    let parsed = loader.load(&path_strings).map_err(|e| match &e {
        ivy_parse::ModuleLoadError::NotFound { .. } => TypeError::module_not_found(&module_name, span),
        ivy_parse::ModuleLoadError::IoError { message, .. } => TypeError::module_io_error(&module_name, message, span),
        ivy_parse::ModuleLoadError::ParseError { message, .. } => {
            TypeError::module_parse_error(&module_name, message, span)
        }
    })?;

    // Clone what we need before mutating checker/env
    let module_program = parsed.program.clone();
    let source = parsed.source.clone();
    let file_path = parsed.path.clone();

    // Mark as loading
    checker.loaded_modules.insert(module_name.clone());

    let exports = match type_check_module(&module_program, checker, loader) {
        Ok(e) => e,
        Err(e) => {
            checker.loaded_modules.remove(&module_name);
            let path_str = file_path.to_string_lossy();
            return Err(TypeError::module_type_error(&module_name, &path_str, &source, e));
        }
    };

    checker.loaded_modules.remove(&module_name);
    env.insert_module(module_name.clone(), exports);

    handle_import_kind(env, &module_name, kind)
}

/// Handle the different import kinds after module is loaded.
fn handle_import_kind(env: &mut TypeEnv, module_name: &str, kind: &ImportKind) -> TypeResult<()> {
    match kind {
        ImportKind::Qualified => Ok(()),
        ImportKind::Alias(alias) => {
            if let Some(exports) = env.get_module(module_name).cloned() {
                env.insert_module(alias.name.clone(), exports);
            }
            Ok(())
        }
        ImportKind::All => {
            if let Some(module_exports) = env.get_module(module_name).cloned() {
                for (name, scheme) in module_exports {
                    env.insert(name, scheme);
                }
            }
            Ok(())
        }
        ImportKind::Items(items) => {
            if let Some(module_exports) = env.get_module(module_name) {
                let exports_to_add: Vec<_> = items
                    .iter()
                    .filter_map(|name| module_exports.get(&name.name).map(|s| (name.name.clone(), s.clone())))
                    .collect();

                for (name, scheme) in exports_to_add {
                    env.insert(name, scheme);
                }
            }
            Ok(())
        }
    }
}

/// Type check a function declaration.
fn check_fn_decl(checker: &mut TypeChecker, fn_decl: &FnDecl, env: &mut TypeEnv) -> TypeResult<()> {
    let fn_name = &fn_decl.name.name;

    let existing_scheme = env.get(fn_name).cloned();
    let mut param_types = Vec::new();
    let mut bindings = Vec::new();
    let mut type_var_scope = HashMap::new();

    for param in &fn_decl.params {
        let ty = if let Some(ann) = &param.ty {
            checker.type_expr_to_type_scoped(&ann.node, env, Some(&mut type_var_scope))
        } else {
            checker.fresh_type()
        };

        let pattern_bindings = checker.infer_pattern(&param.pattern, &ty, env)?;
        bindings.extend(pattern_bindings);
        param_types.push(ty);
    }

    let return_ty_placeholder = checker.fresh_type();
    let mut preliminary_fn_ty = return_ty_placeholder.clone();
    for param_ty in param_types.iter().rev() {
        preliminary_fn_ty = Type::fun(param_ty.clone(), preliminary_fn_ty);
    }

    bindings.push((fn_name.clone(), Scheme::mono(preliminary_fn_ty.clone())));
    let body_env = env.extend(bindings);

    // Infer body type
    let body_ty = match &fn_decl.body {
        FnBody::Expr(expr) => checker.infer(expr, &body_env)?,
        FnBody::Guards(guards) => {
            let mut result_ty = None;
            for guard in guards {
                let cond_ty = checker.infer(&guard.guard, &body_env)?;
                unify::unify_with_subst(&cond_ty, &Type::Bool, &mut checker.subst, guard.guard.span)?;

                let body_ty = checker.infer(&guard.body, &body_env)?;
                match &result_ty {
                    Some(ty) => {
                        unify::unify_with_subst(ty, &body_ty, &mut checker.subst, guard.span)?;
                    }
                    None => {
                        result_ty = Some(body_ty);
                    }
                }
            }
            result_ty.unwrap_or(Type::Unit)
        }
    };

    unify::unify_with_subst(&return_ty_placeholder, &body_ty, &mut checker.subst, fn_decl.name.span)?;

    if let Some(ann) = &fn_decl.return_ty {
        let ann_ty = checker.type_expr_to_type_scoped(&ann.node, env, Some(&mut type_var_scope));
        let body_span = match &fn_decl.body {
            FnBody::Expr(expr) => expr.span,
            FnBody::Guards(guards) => guards.first().map(|g| g.body.span).unwrap_or(fn_decl.span),
        };
        // unify(expected, found): annotation is expected, body type is found.
        unify::unify_with_subst(&ann_ty, &body_ty, &mut checker.subst, body_span)
            .map_err(|e| add_expected_span(e, ann.span))?;
    }

    let mut fn_ty = body_ty;
    for param_ty in param_types.into_iter().rev() {
        fn_ty = Type::fun(param_ty, fn_ty);
    }
    if let Some(existing) = existing_scheme {
        let existing_ty = checker.instantiate(&existing, fn_decl.name.span);
        unify::unify_with_subst(&fn_ty, &existing_ty, &mut checker.subst, fn_decl.name.span)?;
    }

    let final_ty = checker.finalize(&fn_ty);
    *env = env.apply(&checker.subst);
    let env_vars = env.free_vars();
    let ty_vars: Vec<TypeVar> = final_ty.free_vars().difference(&env_vars).copied().collect();
    let (attached, deferred) = discharge_at_boundary(checker, &ty_vars, &env_vars)?;
    checker.constraints = deferred;
    let scheme = if ty_vars.is_empty() && attached.is_empty() {
        Scheme::mono(final_ty)
    } else {
        Scheme::poly_with_constraints(ty_vars, attached, final_ty)
    };
    env.insert(fn_name.clone(), scheme);

    Ok(())
}

/// Type check a module's program and collect the types of public exports.
///
/// Returns a map of export name -> type scheme for all public declarations.
pub fn type_check_module(
    program: &Program,
    checker: &mut TypeChecker,
    loader: &mut ModuleLoader,
) -> TypeResult<HashMap<String, Scheme>> {
    let public_names = collect_public_names(&program.declarations);
    let mut module_env = TypeEnv::with_builtins();
    for decl in &program.declarations {
        check_decl(checker, decl, &mut module_env, loader)?;
    }
    let mut exports = HashMap::new();
    for name in public_names {
        if let Some(scheme) = module_env.get(&name) {
            exports.insert(name, scheme.clone());
        }
    }

    Ok(exports)
}

/// Register type constructors from a type definition.
fn register_type_constructors(
    name: &Ident,
    params: &[Ident],
    body: &TypeBody,
    env: &mut TypeEnv,
    checker: &mut TypeChecker,
) {
    let type_params: Vec<TypeVar> = params.iter().map(|_| checker.fresh_var()).collect();
    let result_ty = if type_params.is_empty() {
        Type::named(&name.name)
    } else {
        Type::named_with(&name.name, type_params.iter().map(|v| Type::Var(*v)).collect())
    };

    let mut param_scope: HashMap<String, Type> = params
        .iter()
        .zip(type_params.iter())
        .map(|(p, &v)| (p.name.clone(), Type::Var(v)))
        .collect();

    match body {
        TypeBody::Sum(variants) => {
            let variant_infos: Vec<VariantInfo> = variants
                .iter()
                .map(|v| VariantInfo {
                    name: v.name.name.clone(),
                    arity: v.fields.len(),
                })
                .collect();
            checker.registry.register_from_variants(&name.name, &variant_infos);
            for variant in variants {
                let mut ctor_ty = result_ty.clone();
                for field in variant.fields.iter().rev() {
                    let field_ty = checker.type_expr_to_type_scoped(&field.node, env, Some(&mut param_scope));
                    ctor_ty = Type::fun(field_ty, ctor_ty);
                }
                let scheme = if type_params.is_empty() {
                    Scheme::mono(ctor_ty)
                } else {
                    Scheme::poly(type_params.clone(), ctor_ty)
                };

                env.insert(variant.name.name.clone(), scheme);
            }
        }
        TypeBody::Record(fields) => {
            let mut field_types: Vec<(String, Type)> = Vec::with_capacity(fields.len());
            for f in fields {
                let field_ty = checker.type_expr_to_type_scoped(&f.ty.node, env, Some(&mut param_scope));
                field_types.push((f.name.name.clone(), field_ty));
            }

            checker
                .registry
                .register_record(&name.name, type_params.clone(), &field_types);
        }
        TypeBody::Alias(ty) => {
            let body_ty = checker.type_expr_to_type_scoped(&ty.node, env, Some(&mut param_scope));
            checker.registry.register_alias(&name.name, type_params, body_ty);
        }
    }
}
