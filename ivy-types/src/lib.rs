//! Ivy Types
//!
//! Type system for the Ivy programming language.

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
pub use registry::{TypeRegistry, VariantInfo};
pub use subst::Subst;
pub use types::{Scheme, Type, TypeVar};
pub use unify::unify;

use ivy_syntax::decl::{Decl, FnBody, FnDecl};
use ivy_syntax::pattern::Pattern;
use ivy_syntax::{Program, Spanned};
use std::collections::HashMap;

/// Type check an entire program.
///
/// Returns Ok(()) if the program type checks and all pattern matches are exhaustive.
pub fn check_program(program: &Program) -> TypeResult<()> {
    let mut env = TypeEnv::with_builtins();
    let mut checker = TypeChecker::new();

    for decl in &program.declarations {
        check_decl(&mut checker, decl, &mut env)?;
    }

    Ok(())
}

/// Type check a program with a given type environment.
pub fn check_program_with_env(program: &Program, checker: &mut TypeChecker, env: &mut TypeEnv) -> TypeResult<()> {
    for decl in &program.declarations {
        check_decl(checker, decl, env)?;
    }
    Ok(())
}

/// Type check a single declaration.
fn check_decl(checker: &mut TypeChecker, decl: &Spanned<Decl>, env: &mut TypeEnv) -> TypeResult<()> {
    match &decl.node {
        Decl::TypeSig { name, ty, .. } => {
            let sig_ty = checker.type_expr_to_type(&ty.node, env);
            let final_ty = checker.finalize(&sig_ty);
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
                let ann_ty = checker.type_expr_to_type(&ann.node, env);
                unify::unify_with_subst(&value_ty, &ann_ty, &mut checker.subst, ann.span)?;
            }

            if let Some(existing) = existing_scheme {
                let existing_ty = checker.instantiate(&existing);
                unify::unify_with_subst(&value_ty, &existing_ty, &mut checker.subst, decl.span)?;
            }

            if let Pattern::Var(ident) = &pattern.node {
                let final_ty = checker.finalize(&value_ty);
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
        // TODO(gtr): Imports, modules, traits, impl, etc.
        _ => Ok(()),
    }
}

/// Type check a function declaration.
fn check_fn_decl(checker: &mut TypeChecker, fn_decl: &FnDecl, env: &mut TypeEnv) -> TypeResult<()> {
    let fn_name = &fn_decl.name.name;

    let existing_scheme = env.get(fn_name).cloned();
    let mut param_types = Vec::new();
    let mut bindings = Vec::new();
    let mut type_var_scope = std::collections::HashMap::new();

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
        unify::unify_with_subst(&body_ty, &ann_ty, &mut checker.subst, ann.span)?;
    }

    let mut fn_ty = body_ty;
    for param_ty in param_types.into_iter().rev() {
        fn_ty = Type::fun(param_ty, fn_ty);
    }
    if let Some(existing) = existing_scheme {
        let existing_ty = checker.instantiate(&existing);
        unify::unify_with_subst(&fn_ty, &existing_ty, &mut checker.subst, fn_decl.name.span)?;
    }

    let final_ty = checker.finalize(&fn_ty);
    let scheme = env.generalize(&final_ty);
    env.insert(fn_name.clone(), scheme);

    Ok(())
}

/// Register type constructors from a type definition.
fn register_type_constructors(
    name: &ivy_syntax::ast::Ident,
    params: &[ivy_syntax::ast::Ident],
    body: &ivy_syntax::decl::TypeBody,
    env: &mut TypeEnv,
    checker: &mut TypeChecker,
) {
    use ivy_syntax::decl::TypeBody;

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
            // Register variants in the type registry for exhaustiveness checking
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
            // Register record type info in the type registry for field validation
            let mut field_types: Vec<(String, Type)> = Vec::with_capacity(fields.len());
            for f in fields {
                let field_ty = checker.type_expr_to_type_scoped(&f.ty.node, env, Some(&mut param_scope));
                field_types.push((f.name.name.clone(), field_ty));
            }

            checker.registry.register_record(&name.name, &field_types);
        }
    }
}
