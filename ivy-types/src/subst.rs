//! Substitution for type variables.

use std::collections::HashMap;

use crate::types::{Scheme, Type, TypeVar};

/// A substitution mapping type variables to types.
#[derive(Debug, Clone, Default)]
pub struct Subst {
    mappings: HashMap<TypeVar, Type>,
}

impl Subst {
    /// Create an empty substitution.
    pub fn new() -> Subst {
        Subst {
            mappings: HashMap::new(),
        }
    }

    /// Bind a type variable to a type.
    pub fn bind(&mut self, var: TypeVar, ty: Type) {
        self.mappings.insert(var, ty);
    }

    /// Look up a type variable.
    pub fn get(&self, var: TypeVar) -> Option<&Type> {
        self.mappings.get(&var)
    }

    /// Check if the substitution is empty.
    pub fn is_empty(&self) -> bool {
        self.mappings.is_empty()
    }

    /// Apply this substitution to a type.
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Int | Type::Float | Type::Bool | Type::String | Type::Char | Type::Unit => ty.clone(),
            Type::Var(v) => {
                if let Some(bound) = self.mappings.get(v) {
                    // Recursively apply to handle chains: a -> b -> Int
                    self.apply(bound)
                } else {
                    ty.clone()
                }
            }
            Type::Fun(a, b) => Type::Fun(Box::new(self.apply(a)), Box::new(self.apply(b))),
            Type::Tuple(elems) => Type::Tuple(elems.iter().map(|e| self.apply(e)).collect()),
            Type::List(elem) => Type::List(Box::new(self.apply(elem))),
            Type::Named(name, args) => Type::Named(name.clone(), args.iter().map(|a| self.apply(a)).collect()),
            Type::Record(name, fields) => Type::Record(
                name.clone(),
                fields.iter().map(|(n, ty)| (n.clone(), self.apply(ty))).collect(),
            ),
        }
    }

    /// Apply this substitution to a type scheme.
    pub fn apply_scheme(&self, scheme: &Scheme) -> Scheme {
        let mut filtered = self.clone();
        for v in &scheme.vars {
            filtered.mappings.remove(v);
        }
        Scheme {
            vars: scheme.vars.clone(),
            ty: filtered.apply(&scheme.ty),
        }
    }

    /// Compose two substitutions
    pub fn compose(&self, other: &Subst) -> Subst {
        let mut result = Subst::new();

        for (var, ty) in &other.mappings {
            result.bind(*var, self.apply(ty));
        }

        for (var, ty) in &self.mappings {
            if !result.mappings.contains_key(var) {
                result.bind(*var, ty.clone());
            }
        }

        result
    }

    pub fn extend(&mut self, other: &Subst) {
        for (var, ty) in &other.mappings {
            let applied = self.apply(ty);
            self.bind(*var, applied);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_subst() {
        let subst = Subst::new();
        assert!(subst.is_empty());
        assert_eq!(subst.apply(&Type::Int), Type::Int);
    }

    #[test]
    fn test_bind_and_apply() {
        let mut subst = Subst::new();
        let a = TypeVar(0);

        subst.bind(a, Type::Int);
        assert_eq!(subst.apply(&Type::Var(a)), Type::Int);
    }

    #[test]
    fn test_apply_function() {
        let mut subst = Subst::new();
        let a = TypeVar(0);
        let b = TypeVar(1);

        subst.bind(a, Type::Int);
        subst.bind(b, Type::Bool);

        let ty = Type::fun(Type::Var(a), Type::Var(b));
        let result = subst.apply(&ty);
        assert_eq!(result, Type::fun(Type::Int, Type::Bool));
    }

    #[test]
    fn test_apply_chain() {
        let mut subst = Subst::new();
        let a = TypeVar(0);
        let b = TypeVar(1);

        // a -> b, b -> Int
        subst.bind(a, Type::Var(b));
        subst.bind(b, Type::Int);

        // Applying to 'a' should give Int, not just 'b'
        let result = subst.apply(&Type::Var(a));
        assert_eq!(result, Type::Int);
    }

    #[test]
    fn test_compose() {
        let mut s1 = Subst::new();
        let mut s2 = Subst::new();
        let a = TypeVar(0);
        let b = TypeVar(1);

        s1.bind(b, Type::Int);
        s2.bind(a, Type::Var(b));

        let composed = s1.compose(&s2);
        assert_eq!(composed.apply(&Type::Var(a)), Type::Int);
    }

    #[test]
    fn test_apply_scheme() {
        let mut subst = Subst::new();
        let a = TypeVar(0);
        let b = TypeVar(1);

        subst.bind(a, Type::Int);
        subst.bind(b, Type::Bool);

        let scheme = Scheme::poly(vec![a], Type::fun(Type::Var(a), Type::Var(b)));
        let result = subst.apply_scheme(&scheme);

        // a should not be substituted (cuz it's bound); b should become Bool
        assert_eq!(result.ty, Type::fun(Type::Var(a), Type::Bool));
    }
}
