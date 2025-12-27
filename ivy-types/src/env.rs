//! Type environment (type context).

use std::collections::{HashMap, HashSet};

use crate::subst::Subst;
use crate::types::{Scheme, Type, TypeVar};

/// A type environment mapping names to type schemes.
#[derive(Debug, Clone)]
pub struct TypeEnv {
    bindings: HashMap<String, Scheme>,
}

impl TypeEnv {
    /// Create an empty type environment.
    pub fn new() -> TypeEnv {
        TypeEnv {
            bindings: HashMap::new(),
        }
    }

    /// Create a type environment with built-in types and functions.
    pub fn with_builtins() -> TypeEnv {
        let mut env = TypeEnv::new();

        // Built-in functions
        // print: a -> ()
        let a = TypeVar(1000);
        env.insert(
            "print".to_string(),
            Scheme::poly(vec![a], Type::fun(Type::Var(a), Type::Unit)),
        );

        // println: a -> ()
        env.insert(
            "println".to_string(),
            Scheme::poly(vec![a], Type::fun(Type::Var(a), Type::Unit)),
        );

        // show: a -> String
        env.insert(
            "show".to_string(),
            Scheme::poly(vec![a], Type::fun(Type::Var(a), Type::String)),
        );

        // Note: length, head, tail, isEmpty are defined in prelude with proper Option types

        // Option type constructors
        // Some: a -> Option<a>
        env.insert(
            "Some".to_string(),
            Scheme::poly(
                vec![a],
                Type::fun(Type::Var(a), Type::named_with("Option", vec![Type::Var(a)])),
            ),
        );

        // None: Option<a>
        env.insert(
            "None".to_string(),
            Scheme::poly(vec![a], Type::named_with("Option", vec![Type::Var(a)])),
        );

        // Result type constructors
        let b = TypeVar(1001);
        // Ok: a -> Result<a, e>
        env.insert(
            "Ok".to_string(),
            Scheme::poly(
                vec![a, b],
                Type::fun(
                    Type::Var(a),
                    Type::named_with("Result", vec![Type::Var(a), Type::Var(b)]),
                ),
            ),
        );

        // Err: e -> Result<a, e>
        env.insert(
            "Err".to_string(),
            Scheme::poly(
                vec![a, b],
                Type::fun(
                    Type::Var(b),
                    Type::named_with("Result", vec![Type::Var(a), Type::Var(b)]),
                ),
            ),
        );

        // Math functions
        // abs: Int -> Int
        env.insert("abs".to_string(), Scheme::mono(Type::fun(Type::Int, Type::Int)));

        // min: Int -> Int -> Int
        env.insert(
            "min".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::fun(Type::Int, Type::Int))),
        );

        // max: Int -> Int -> Int
        env.insert(
            "max".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::fun(Type::Int, Type::Int))),
        );

        // pow: Int -> Int -> Int
        env.insert(
            "pow".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::fun(Type::Int, Type::Int))),
        );

        // sqrt: Int -> Float (or Float -> Float, but we convert Int to Float)
        env.insert("sqrt".to_string(), Scheme::mono(Type::fun(Type::Int, Type::Float)));

        // floor: Float -> Int
        env.insert("floor".to_string(), Scheme::mono(Type::fun(Type::Float, Type::Int)));

        // ceil: Float -> Int
        env.insert("ceil".to_string(), Scheme::mono(Type::fun(Type::Float, Type::Int)));

        // round: Float -> Int
        env.insert("round".to_string(), Scheme::mono(Type::fun(Type::Float, Type::Int)));

        // random: Int -> Int -> Int
        env.insert(
            "random".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::fun(Type::Int, Type::Int))),
        );

        // String functions
        // strLength: String -> Int
        env.insert(
            "strLength".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::Int)),
        );

        // strTrim: String -> String
        env.insert(
            "strTrim".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::String)),
        );

        // strContains: String -> String -> Bool
        env.insert(
            "strContains".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Bool))),
        );

        // strSubstring: String -> Int -> Int -> String
        env.insert(
            "strSubstring".to_string(),
            Scheme::mono(Type::fun(
                Type::String,
                Type::fun(Type::Int, Type::fun(Type::Int, Type::String)),
            )),
        );

        // strSplit: String -> String -> [String]
        env.insert(
            "strSplit".to_string(),
            Scheme::mono(Type::fun(
                Type::String,
                Type::fun(Type::String, Type::List(Box::new(Type::String))),
            )),
        );

        // strToUpper: String -> String
        env.insert(
            "strToUpper".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::String)),
        );

        // strToLower: String -> String
        env.insert(
            "strToLower".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::String)),
        );

        // strStartsWith: String -> String -> Bool
        env.insert(
            "strStartsWith".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Bool))),
        );

        // strEndsWith: String -> String -> Bool
        env.insert(
            "strEndsWith".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Bool))),
        );

        // strReplace: String -> String -> String -> String
        env.insert(
            "strReplace".to_string(),
            Scheme::mono(Type::fun(
                Type::String,
                Type::fun(Type::String, Type::fun(Type::String, Type::String)),
            )),
        );

        // File I/O functions
        // readFile: String -> String
        env.insert(
            "readFile".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::String)),
        );

        // writeFile: String -> String -> ()
        env.insert(
            "writeFile".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Unit))),
        );

        // appendFile: String -> String -> ()
        env.insert(
            "appendFile".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Unit))),
        );

        // fileExists: String -> Bool
        env.insert(
            "fileExists".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::Bool)),
        );

        env
    }

    /// Look up a variable's type scheme.
    pub fn get(&self, name: &str) -> Option<&Scheme> {
        self.bindings.get(name)
    }

    /// Insert a new binding.
    pub fn insert(&mut self, name: String, scheme: Scheme) {
        self.bindings.insert(name, scheme);
    }

    /// Remove a binding.
    pub fn remove(&mut self, name: &str) {
        self.bindings.remove(name);
    }

    /// Create an extended environment with additional bindings.
    pub fn extend(&self, bindings: Vec<(String, Scheme)>) -> TypeEnv {
        let mut new_env = self.clone();
        for (name, scheme) in bindings {
            new_env.insert(name, scheme);
        }
        new_env
    }

    /// Apply a substitution to all type schemes in the environment.
    pub fn apply(&self, subst: &Subst) -> TypeEnv {
        TypeEnv {
            bindings: self
                .bindings
                .iter()
                .map(|(name, scheme)| (name.clone(), subst.apply_scheme(scheme)))
                .collect(),
        }
    }

    /// Collect all free type variables in the environment.
    pub fn free_vars(&self) -> HashSet<TypeVar> {
        self.bindings.values().flat_map(|scheme| scheme.free_vars()).collect()
    }

    /// Generalize a type into a type scheme.
    ///
    /// This quantifies all type variables that are free in the type
    /// but not free in the environment.
    pub fn generalize(&self, ty: &Type) -> Scheme {
        let env_vars = self.free_vars();
        let ty_vars = ty.free_vars();

        // Variables to generalize: in type but not in environment
        let vars: Vec<TypeVar> = ty_vars.difference(&env_vars).copied().collect();

        if vars.is_empty() {
            Scheme::mono(ty.clone())
        } else {
            Scheme::poly(vars, ty.clone())
        }
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        TypeEnv::new()
    }
}

/// Type variable generator for fresh variables.
#[derive(Debug, Clone)]
pub struct TypeVarGen {
    next_id: u32,
}

impl TypeVarGen {
    pub fn new() -> TypeVarGen {
        TypeVarGen { next_id: 0 }
    }

    /// Generate a fresh type variable.
    pub fn fresh(&mut self) -> TypeVar {
        let var = TypeVar(self.next_id);
        self.next_id += 1;
        var
    }

    /// Generate a fresh type variable as a Type.
    pub fn fresh_type(&mut self) -> Type {
        Type::Var(self.fresh())
    }

    /// Instantiate a type scheme with fresh type variables.
    pub fn instantiate(&mut self, scheme: &Scheme) -> Type {
        if scheme.vars.is_empty() {
            return scheme.ty.clone();
        }

        let mut subst = Subst::new();
        for var in &scheme.vars {
            subst.bind(*var, self.fresh_type());
        }
        subst.apply(&scheme.ty)
    }
}

impl Default for TypeVarGen {
    fn default() -> Self {
        TypeVarGen::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env_lookup() {
        let mut env = TypeEnv::new();
        env.insert("x".to_string(), Scheme::mono(Type::Int));

        assert!(env.get("x").is_some());
        assert!(env.get("y").is_none());
    }

    #[test]
    fn test_env_extend() {
        let mut env = TypeEnv::new();
        env.insert("x".to_string(), Scheme::mono(Type::Int));

        let env2 = env.extend(vec![("y".to_string(), Scheme::mono(Type::Bool))]);

        assert!(env.get("y").is_none());
        assert!(env2.get("x").is_some());
        assert!(env2.get("y").is_some());
    }

    #[test]
    fn test_generalize() {
        let env = TypeEnv::new();
        let a = TypeVar(0);

        let ty = Type::fun(Type::Var(a), Type::Var(a));
        let scheme = env.generalize(&ty);

        assert_eq!(scheme.vars.len(), 1);
        assert!(scheme.vars.contains(&a));
    }

    #[test]
    fn test_generalize_with_env_var() {
        let a = TypeVar(0);
        let b = TypeVar(1);

        let mut env = TypeEnv::new();
        env.insert("x".to_string(), Scheme::mono(Type::Var(a)));

        let ty = Type::fun(Type::Var(a), Type::Var(b));
        let scheme = env.generalize(&ty);

        assert_eq!(scheme.vars.len(), 1);
        assert!(scheme.vars.contains(&b));
        assert!(!scheme.vars.contains(&a));
    }

    #[test]
    fn test_fresh_var() {
        let mut gen = TypeVarGen::new();

        let a = gen.fresh();
        let b = gen.fresh();
        let c = gen.fresh();

        assert_eq!(a, TypeVar(0));
        assert_eq!(b, TypeVar(1));
        assert_eq!(c, TypeVar(2));
    }

    #[test]
    fn test_instantiate() {
        let mut gen = TypeVarGen::new();
        let a = TypeVar(100);

        let scheme = Scheme::poly(vec![a], Type::fun(Type::Var(a), Type::Var(a)));

        let t1 = gen.instantiate(&scheme);
        let t2 = gen.instantiate(&scheme);

        assert_ne!(t1, t2);

        match (&t1, &t2) {
            (Type::Fun(a1, r1), Type::Fun(a2, r2)) => {
                assert_eq!(a1, r1); // a -> a
                assert_eq!(a2, r2); // b -> b
            }
            _ => panic!("Expected function types"),
        }
    }

    #[test]
    fn test_builtins() {
        let env = TypeEnv::with_builtins();

        assert!(env.get("print").is_some());
        assert!(env.get("println").is_some());
        assert!(env.get("show").is_some());
    }
}
