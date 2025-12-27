//! Unification algorithm for type inference.
//!
//! Unification finds a substitution that makes two types equal.
//! This is a core part of Hindley-Milner type inference.

use ivy_syntax::Span;

use crate::error::{TypeError, TypeResult};
use crate::subst::Subst;
use crate::types::{Type, TypeVar};

/// Unify two types, returning a substitution that makes them equal.
pub fn unify(t1: &Type, t2: &Type, span: Span) -> TypeResult<Subst> {
    let mut subst = Subst::new();
    unify_with_subst(t1, t2, &mut subst, span)?;
    Ok(subst)
}

/// Unify two types, extending an existing substitution.
pub fn unify_with_subst(t1: &Type, t2: &Type, subst: &mut Subst, span: Span) -> TypeResult<()> {
    let t1 = subst.apply(t1);
    let t2 = subst.apply(t2);

    match (&t1, &t2) {
        // Same primitive types
        (Type::Int, Type::Int)
        | (Type::Float, Type::Float)
        | (Type::Bool, Type::Bool)
        | (Type::String, Type::String)
        | (Type::Char, Type::Char)
        | (Type::Unit, Type::Unit) => Ok(()),

        // Same type variable
        (Type::Var(v1), Type::Var(v2)) if v1 == v2 => Ok(()),

        // Type variable on left: bind it
        (Type::Var(v), ty) => bind_var(*v, ty, subst, span),

        // Type variable on right: bind it
        (ty, Type::Var(v)) => bind_var(*v, ty, subst, span),

        // Function types: unify parameter and return types
        (Type::Fun(a1, r1), Type::Fun(a2, r2)) => {
            unify_with_subst(a1, a2, subst, span)?;
            unify_with_subst(r1, r2, subst, span)
        }

        // Tuple types: unify element-wise
        (Type::Tuple(elems1), Type::Tuple(elems2)) => {
            if elems1.len() != elems2.len() {
                return Err(TypeError::mismatch(t1.clone(), t2.clone(), span));
            }
            for (e1, e2) in elems1.iter().zip(elems2.iter()) {
                unify_with_subst(e1, e2, subst, span)?;
            }
            Ok(())
        }

        // List types: unify element types
        (Type::List(e1), Type::List(e2)) => unify_with_subst(e1, e2, subst, span),

        // Named types: must have same name and unify type arguments
        (Type::Named(n1, args1), Type::Named(n2, args2)) => {
            if n1 != n2 {
                return Err(TypeError::mismatch(t1.clone(), t2.clone(), span));
            }
            if args1.len() != args2.len() {
                return Err(TypeError::mismatch(t1.clone(), t2.clone(), span));
            }
            for (a1, a2) in args1.iter().zip(args2.iter()) {
                unify_with_subst(a1, a2, subst, span)?;
            }
            Ok(())
        }

        // Record types: must have same name and unify field types
        (Type::Record(n1, fields1), Type::Record(n2, fields2)) => {
            if n1 != n2 {
                return Err(TypeError::mismatch(t1.clone(), t2.clone(), span));
            }
            if fields1.len() != fields2.len() {
                return Err(TypeError::mismatch(t1.clone(), t2.clone(), span));
            }
            // Fields should be in same order for structural comparison
            for ((name1, ty1), (name2, ty2)) in fields1.iter().zip(fields2.iter()) {
                if name1 != name2 {
                    return Err(TypeError::mismatch(t1.clone(), t2.clone(), span));
                }
                unify_with_subst(ty1, ty2, subst, span)?;
            }
            Ok(())
        }

        // Mismatch
        _ => Err(TypeError::mismatch(t1, t2, span)),
    }
}

/// Bind a type variable to a type, with occurs check.
fn bind_var(var: TypeVar, ty: &Type, subst: &mut Subst, span: Span) -> TypeResult<()> {
    // Occurs check: prevent infinite types like a = [a]
    if ty.contains_var(var) {
        return Err(TypeError::infinite_type(var, ty.clone(), span));
    }

    subst.bind(var, ty.clone());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span() -> Span {
        Span::new(0, 0)
    }

    #[test]
    fn test_unify_same_primitive() {
        assert!(unify(&Type::Int, &Type::Int, span()).is_ok());
        assert!(unify(&Type::Bool, &Type::Bool, span()).is_ok());
        assert!(unify(&Type::String, &Type::String, span()).is_ok());
    }

    #[test]
    fn test_unify_different_primitives() {
        assert!(unify(&Type::Int, &Type::Bool, span()).is_err());
    }

    #[test]
    fn test_unify_var_with_type() {
        let a = TypeVar(0);
        let result = unify(&Type::Var(a), &Type::Int, span());
        assert!(result.is_ok());

        let subst = result.unwrap();
        assert_eq!(subst.apply(&Type::Var(a)), Type::Int);
    }

    #[test]
    fn test_unify_type_with_var() {
        let a = TypeVar(0);
        let result = unify(&Type::Int, &Type::Var(a), span());
        assert!(result.is_ok());

        let subst = result.unwrap();
        assert_eq!(subst.apply(&Type::Var(a)), Type::Int);
    }

    #[test]
    fn test_unify_two_vars() {
        let a = TypeVar(0);
        let b = TypeVar(1);
        let result = unify(&Type::Var(a), &Type::Var(b), span());
        assert!(result.is_ok());

        let subst = result.unwrap();
        // Either a maps to b, or b maps to a
        let a_ty = subst.apply(&Type::Var(a));
        let b_ty = subst.apply(&Type::Var(b));
        assert_eq!(a_ty, b_ty);
    }

    #[test]
    fn test_unify_functions() {
        let a = TypeVar(0);
        let b = TypeVar(1);

        let t1 = Type::fun(Type::Var(a), Type::Int);
        let t2 = Type::fun(Type::Bool, Type::Var(b));

        let result = unify(&t1, &t2, span());
        assert!(result.is_ok());

        let subst = result.unwrap();
        assert_eq!(subst.apply(&Type::Var(a)), Type::Bool);
        assert_eq!(subst.apply(&Type::Var(b)), Type::Int);
    }

    #[test]
    fn test_unify_lists() {
        let a = TypeVar(0);
        let result = unify(&Type::list(Type::Var(a)), &Type::list(Type::Int), span());
        assert!(result.is_ok());

        let subst = result.unwrap();
        assert_eq!(subst.apply(&Type::Var(a)), Type::Int);
    }

    #[test]
    fn test_unify_tuples() {
        let a = TypeVar(0);
        let t1 = Type::Tuple(vec![Type::Int, Type::Var(a)]);
        let t2 = Type::Tuple(vec![Type::Int, Type::Bool]);

        let result = unify(&t1, &t2, span());
        assert!(result.is_ok());

        let subst = result.unwrap();
        assert_eq!(subst.apply(&Type::Var(a)), Type::Bool);
    }

    #[test]
    fn test_unify_tuple_length_mismatch() {
        let t1 = Type::Tuple(vec![Type::Int, Type::Bool]);
        let t2 = Type::Tuple(vec![Type::Int]);
        assert!(unify(&t1, &t2, span()).is_err());
    }

    #[test]
    fn test_unify_named() {
        let a = TypeVar(0);
        let t1 = Type::named_with("Option", vec![Type::Var(a)]);
        let t2 = Type::named_with("Option", vec![Type::Int]);

        let result = unify(&t1, &t2, span());
        assert!(result.is_ok());

        let subst = result.unwrap();
        assert_eq!(subst.apply(&Type::Var(a)), Type::Int);
    }

    #[test]
    fn test_unify_different_named() {
        let t1 = Type::named_with("Option", vec![Type::Int]);
        let t2 = Type::named_with("Result", vec![Type::Int]);
        assert!(unify(&t1, &t2, span()).is_err());
    }

    #[test]
    fn test_occurs_check() {
        let a = TypeVar(0);
        // a = [a] is an infinite type
        let result = unify(&Type::Var(a), &Type::list(Type::Var(a)), span());
        assert!(result.is_err());
    }

    #[test]
    fn test_unify_complex() {
        // (a -> b) -> [a] -> [b] unified with (Int -> Bool) -> [Int] -> c
        let a = TypeVar(0);
        let b = TypeVar(1);
        let c = TypeVar(2);

        let t1 = Type::fun(
            Type::fun(Type::Var(a), Type::Var(b)),
            Type::fun(Type::list(Type::Var(a)), Type::list(Type::Var(b))),
        );
        let t2 = Type::fun(
            Type::fun(Type::Int, Type::Bool),
            Type::fun(Type::list(Type::Int), Type::Var(c)),
        );

        let result = unify(&t1, &t2, span());
        assert!(result.is_ok());

        let subst = result.unwrap();
        assert_eq!(subst.apply(&Type::Var(a)), Type::Int);
        assert_eq!(subst.apply(&Type::Var(b)), Type::Bool);
        assert_eq!(subst.apply(&Type::Var(c)), Type::list(Type::Bool));
    }
}
