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
use ivy_syntax::decl::{Decl, FnBody, FnDecl, ImportKind, TypeBody};
use ivy_syntax::pattern::Pattern;
use ivy_syntax::{collect_public_names, Ident, Program, Spanned};
pub use registry::{TypeRegistry, VariantInfo};
use std::collections::HashMap;
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
                let scheme = env.generalize(&final_ty);
                env.insert(ident.name.clone(), scheme);
            }
            Ok(())
        }
        Decl::Fn(fn_decl) => check_fn_decl(checker, fn_decl, env),
        Decl::Type { name, params, body, .. } => {
            register_type_constructors(name, params, body, env, checker);
            Ok(())
        }
        Decl::Import { path, kind } => check_import(checker, env, loader, path, kind),
        // TODO(gtr): modules, traits, impl, etc.
        _ => Ok(()),
    }
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
    let scheme = env.generalize(&final_ty);
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
