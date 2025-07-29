use std::collections::{HashMap, HashSet};

use crate::subst::Subst;
use crate::types::{Scheme, Type, TypeVar};

#[derive(Debug, Clone)]
pub struct TypeEnv {
    bindings: HashMap<String, Scheme>,
}

impl TypeEnv {
    pub fn new() -> TypeEnv {
        TypeEnv {
            bindings: HashMap::new(),
        }
    }

    pub fn with_builtins() -> TypeEnv {
        let mut env = TypeEnv::new();

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

        env.insert("abs".to_string(), Scheme::mono(Type::fun(Type::Int, Type::Int)));

        env.insert(
            "min".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::fun(Type::Int, Type::Int))),
        );

        env.insert(
            "max".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::fun(Type::Int, Type::Int))),
        );

        env.insert(
            "pow".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::fun(Type::Int, Type::Int))),
        );

        env.insert("sqrt".to_string(), Scheme::mono(Type::fun(Type::Int, Type::Float)));

        env.insert("floor".to_string(), Scheme::mono(Type::fun(Type::Float, Type::Int)));

        env.insert("ceil".to_string(), Scheme::mono(Type::fun(Type::Float, Type::Int)));

        env.insert("round".to_string(), Scheme::mono(Type::fun(Type::Float, Type::Int)));

        env.insert(
            "random".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::fun(Type::Int, Type::Int))),
        );

        env.insert(
            "strLength".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::Int)),
        );

        env.insert(
            "strTrim".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::String)),
        );

        env.insert(
            "strContains".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Bool))),
        );

        env.insert(
            "strSubstring".to_string(),
            Scheme::mono(Type::fun(
                Type::String,
                Type::fun(Type::Int, Type::fun(Type::Int, Type::String)),
            )),
        );

        env.insert(
            "strSplit".to_string(),
            Scheme::mono(Type::fun(
                Type::String,
                Type::fun(Type::String, Type::List(Box::new(Type::String))),
            )),
        );

        env.insert(
            "strToUpper".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::String)),
        );

        env.insert(
            "strToLower".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::String)),
        );

        env.insert(
            "strStartsWith".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Bool))),
        );

        env.insert(
            "strEndsWith".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Bool))),
        );

        env.insert(
            "strReplace".to_string(),
            Scheme::mono(Type::fun(
                Type::String,
                Type::fun(Type::String, Type::fun(Type::String, Type::String)),
            )),
        );

        env.insert(
            "readFile".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::String)),
        );

        env.insert(
            "writeFile".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Unit))),
        );

        env.insert(
            "appendFile".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Unit))),
        );

        env.insert(
            "fileExists".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::Bool)),
        );

        env
    }

    pub fn get(&self, name: &str) -> Option<&Scheme> {
        self.bindings.get(name)
    }

    pub fn insert(&mut self, name: String, scheme: Scheme) {
        self.bindings.insert(name, scheme);
    }

    pub fn remove(&mut self, name: &str) {
        self.bindings.remove(name);
    }

    pub fn extend(&self, bindings: Vec<(String, Scheme)>) -> TypeEnv {
        let mut new_env = self.clone();
        for (name, scheme) in bindings {
            new_env.insert(name, scheme);
        }
        new_env
    }

    pub fn apply(&self, subst: &Subst) -> TypeEnv {
        TypeEnv {
            bindings: self
                .bindings
                .iter()
                .map(|(name, scheme)| (name.clone(), subst.apply_scheme(scheme)))
                .collect(),
        }
    }

    pub fn free_vars(&self) -> HashSet<TypeVar> {
        self.bindings.values().flat_map(|scheme| scheme.free_vars()).collect()
    }

    pub fn generalize(&self, ty: &Type) -> Scheme {
        let env_vars = self.free_vars();
        let ty_vars = ty.free_vars();

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

#[derive(Debug, Clone)]
pub struct TypeVarGen {
    next_id: u32,
}

impl TypeVarGen {
    pub fn new() -> TypeVarGen {
        TypeVarGen { next_id: 0 }
    }

    pub fn fresh(&mut self) -> TypeVar {
        let var = TypeVar(self.next_id);
        self.next_id += 1;
        var
    }

    pub fn fresh_type(&mut self) -> Type {
        Type::Var(self.fresh())
    }

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
