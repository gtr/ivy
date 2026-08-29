use crate::subst::Subst;
use crate::types::{Scheme, Type, TypeVar};
use std::collections::{HashMap, HashSet};

/// Builtin schemes use canonical bound vars in this range. Anything below this
/// is reserved for the user's `TypeVarGen` to produce fresh vars without
/// colliding with un-instantiated builtin schemes still in scope.
pub const BUILTIN_VAR_OFFSET: u32 = 1_000_000;

/// A type environment mapping names to type schemes
#[derive(Debug, Clone)]
pub struct TypeEnv {
    /// Loaded bindings: name -> type scheme
    bindings: HashMap<String, Scheme>,
    /// Loaded modules: module name -> (export name -> type scheme)
    modules: HashMap<String, HashMap<String, Scheme>>,
}

impl TypeEnv {
    /// Create an empty type environment
    pub fn new() -> TypeEnv {
        TypeEnv {
            bindings: HashMap::new(),
            modules: HashMap::new(),
        }
    }

    /// Create a type environment with built-in types and functions
    /// TODO(gtr): fix repeated built-ins in different places
    pub fn with_builtins() -> TypeEnv {
        let mut env = TypeEnv::new();
        let a = TypeVar(BUILTIN_VAR_OFFSET);
        let b = TypeVar(BUILTIN_VAR_OFFSET + 1);

        // ========================================================================
        // True builtins (always available, user-facing)
        // ========================================================================

        // print: a -> ()
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

        // ========================================================================
        // Type constructors
        // ========================================================================

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

        // ========================================================================
        // Conversion intrinsics (wrapped by lib/Convert.ivy)
        // ========================================================================

        // __floatFromInt: Int -> Float
        env.insert(
            "__floatFromInt".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::Float)),
        );

        // __floatToInt: Float -> Int
        env.insert(
            "__floatToInt".to_string(),
            Scheme::mono(Type::fun(Type::Float, Type::Int)),
        );

        // __floatToString: Float -> String
        env.insert(
            "__floatToString".to_string(),
            Scheme::mono(Type::fun(Type::Float, Type::String)),
        );

        // __intToString: Int -> String
        env.insert(
            "__intToString".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::String)),
        );

        // __charToString: Char -> String
        env.insert(
            "__charToString".to_string(),
            Scheme::mono(Type::fun(Type::Char, Type::String)),
        );

        // __intEq: Int -> Int -> Bool
        env.insert(
            "__intEq".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::fun(Type::Int, Type::Bool))),
        );

        // __floatEq: Float -> Float -> Bool
        env.insert(
            "__floatEq".to_string(),
            Scheme::mono(Type::fun(Type::Float, Type::fun(Type::Float, Type::Bool))),
        );

        // __strEq: String -> String -> Bool
        env.insert(
            "__strEq".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Bool))),
        );

        // __intCompare / __floatCompare / __strCompare: a -> a -> Ordering
        env.insert(
            "__intCompare".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::fun(Type::Int, Type::named("Ordering")))),
        );
        env.insert(
            "__floatCompare".to_string(),
            Scheme::mono(Type::fun(Type::Float, Type::fun(Type::Float, Type::named("Ordering")))),
        );
        env.insert(
            "__strCompare".to_string(),
            Scheme::mono(Type::fun(
                Type::String,
                Type::fun(Type::String, Type::named("Ordering")),
            )),
        );

        // __stringToInt: String -> Int (may fail at runtime)
        env.insert(
            "__stringToInt".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::Int)),
        );

        // __stringToFloat: String -> Float (may fail at runtime)
        env.insert(
            "__stringToFloat".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::Float)),
        );

        // __tryStringToInt: String -> Option Int (safe parse)
        env.insert(
            "__tryStringToInt".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::named_with("Option", vec![Type::Int]))),
        );

        // __tryStringToFloat: String -> Option Float (safe parse)
        env.insert(
            "__tryStringToFloat".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::named_with("Option", vec![Type::Float]))),
        );

        // ========================================================================
        // Math intrinsics (wrapped by lib/Math.ivy)
        // ========================================================================

        // __abs: Int -> Int (runtime handles Float too)
        env.insert("__abs".to_string(), Scheme::mono(Type::fun(Type::Int, Type::Int)));

        // __min: Int -> Int -> Int
        env.insert(
            "__min".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::fun(Type::Int, Type::Int))),
        );

        // __max: Int -> Int -> Int
        env.insert(
            "__max".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::fun(Type::Int, Type::Int))),
        );

        // __pow: Float -> Float -> Float
        env.insert(
            "__pow".to_string(),
            Scheme::mono(Type::fun(Type::Float, Type::fun(Type::Float, Type::Float))),
        );

        // __sqrt: Float -> Float
        env.insert("__sqrt".to_string(), Scheme::mono(Type::fun(Type::Float, Type::Float)));

        // __floor: Float -> Int
        env.insert("__floor".to_string(), Scheme::mono(Type::fun(Type::Float, Type::Int)));

        // __ceil: Float -> Int
        env.insert("__ceil".to_string(), Scheme::mono(Type::fun(Type::Float, Type::Int)));

        // __round: Float -> Int
        env.insert("__round".to_string(), Scheme::mono(Type::fun(Type::Float, Type::Int)));

        // __random: Int -> Int -> Int
        env.insert(
            "__random".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::fun(Type::Int, Type::Int))),
        );

        // ========================================================================
        // String intrinsics (wrapped by lib/String.ivy)
        // ========================================================================

        // __strLength: String -> Int
        env.insert(
            "__strLength".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::Int)),
        );

        // __strTrim: String -> String
        env.insert(
            "__strTrim".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::String)),
        );

        // __strContains: String -> String -> Bool
        env.insert(
            "__strContains".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Bool))),
        );

        // __strSubstring: String -> Int -> Int -> String
        env.insert(
            "__strSubstring".to_string(),
            Scheme::mono(Type::fun(
                Type::String,
                Type::fun(Type::Int, Type::fun(Type::Int, Type::String)),
            )),
        );

        // __strSplit: String -> String -> [String]
        env.insert(
            "__strSplit".to_string(),
            Scheme::mono(Type::fun(
                Type::String,
                Type::fun(Type::String, Type::List(Box::new(Type::String))),
            )),
        );

        // __strToUpper: String -> String
        env.insert(
            "__strToUpper".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::String)),
        );

        // __strToLower: String -> String
        env.insert(
            "__strToLower".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::String)),
        );

        // __strStartsWith: String -> String -> Bool
        env.insert(
            "__strStartsWith".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Bool))),
        );

        // __strEndsWith: String -> String -> Bool
        env.insert(
            "__strEndsWith".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Bool))),
        );

        // __strReplace: String -> String -> String -> String
        env.insert(
            "__strReplace".to_string(),
            Scheme::mono(Type::fun(
                Type::String,
                Type::fun(Type::String, Type::fun(Type::String, Type::String)),
            )),
        );

        env.insert(
            "__strChars".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::List(Box::new(Type::Char)))),
        );
        env.insert(
            "__charIsDigit".to_string(),
            Scheme::mono(Type::fun(Type::Char, Type::Bool)),
        );
        env.insert(
            "__charIsAlpha".to_string(),
            Scheme::mono(Type::fun(Type::Char, Type::Bool)),
        );
        env.insert(
            "__charIsWhitespace".to_string(),
            Scheme::mono(Type::fun(Type::Char, Type::Bool)),
        );
        env.insert(
            "__charToInt".to_string(),
            Scheme::mono(Type::fun(Type::Char, Type::Int)),
        );
        env.insert(
            "__intToChar".to_string(),
            Scheme::mono(Type::fun(Type::Int, Type::Char)),
        );

        // ========================================================================
        // File I/O intrinsics (wrapped by lib/File.ivy)
        // ========================================================================

        // __readFile: String -> String
        env.insert(
            "__readFile".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::String)),
        );

        // __writeFile: String -> String -> ()
        env.insert(
            "__writeFile".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Unit))),
        );

        // __appendFile: String -> String -> ()
        env.insert(
            "__appendFile".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::fun(Type::String, Type::Unit))),
        );

        // __fileExists: String -> Bool
        env.insert(
            "__fileExists".to_string(),
            Scheme::mono(Type::fun(Type::String, Type::Bool)),
        );

        env
    }

    /// Look up a variable's type scheme
    pub fn get(&self, name: &str) -> Option<&Scheme> {
        self.bindings.get(name)
    }

    /// Insert a new binding
    pub fn insert(&mut self, name: String, scheme: Scheme) {
        self.bindings.insert(name, scheme);
    }

    /// Remove a binding
    pub fn remove(&mut self, name: &str) {
        self.bindings.remove(name);
    }

    /// Insert a module with its exported type schemes
    pub fn insert_module(&mut self, name: String, exports: HashMap<String, Scheme>) {
        self.modules.insert(name, exports);
    }

    /// Get all exports of a module
    pub fn get_module(&self, name: &str) -> Option<&HashMap<String, Scheme>> {
        self.modules.get(name)
    }

    /// Get a specific export from a module
    pub fn get_module_export(&self, module: &str, name: &str) -> Option<&Scheme> {
        self.modules.get(module).and_then(|exports| exports.get(name))
    }

    /// Check if a name refers to a loaded module
    pub fn is_module(&self, name: &str) -> bool {
        self.modules.contains_key(name)
    }

    /// Create an extended environment with additional bindings
    pub fn extend(&self, bindings: Vec<(String, Scheme)>) -> TypeEnv {
        let mut new_env = self.clone();
        for (name, scheme) in bindings {
            new_env.insert(name, scheme);
        }
        new_env
    }

    /// Apply a substitution to all type schemes in the environment
    pub fn apply(&self, subst: &Subst) -> TypeEnv {
        TypeEnv {
            bindings: self
                .bindings
                .iter()
                .map(|(name, scheme)| (name.clone(), subst.apply_scheme(scheme)))
                .collect(),
            modules: self.modules.clone(),
        }
    }

    /// Collect all free type variables in the environment.
    pub fn free_vars(&self) -> HashSet<TypeVar> {
        self.bindings
            .values()
            .flat_map(super::types::Scheme::free_vars)
            .collect()
    }

    /// Generalize a type into a type scheme.
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

/// Type variable generator for fresh variables
#[derive(Debug, Clone)]
pub struct TypeVarGen {
    next_id: u32,
}

impl TypeVarGen {
    pub fn new() -> TypeVarGen {
        TypeVarGen { next_id: 0 }
    }

    /// Generate a fresh type variable
    pub fn fresh(&mut self) -> TypeVar {
        let var = TypeVar(self.next_id);
        self.next_id += 1;
        var
    }

    /// Generate a fresh type variable as a Type
    pub fn fresh_type(&mut self) -> Type {
        Type::Var(self.fresh())
    }

    pub fn instantiate(&mut self, scheme: &Scheme) -> (Type, Vec<crate::TraitConstraint>) {
        scheme.instantiate(|| self.fresh_type())
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

        let (t1, _) = gen.instantiate(&scheme);
        let (t2, _) = gen.instantiate(&scheme);

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
