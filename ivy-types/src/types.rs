//! Type representation for Ivy.

use std::collections::HashSet;
use std::fmt;

/// Unique identifier for type variables during inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Display as lowercase letters: a, b, c, ... z, a1, b1, ...
        let n = self.0 as usize;
        let letter = (b'a' + (n % 26) as u8) as char;
        if n < 26 {
            write!(f, "{}", letter)
        } else {
            write!(f, "{}{}", letter, n / 26)
        }
    }
}

/// Core type representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// Primitive integer type
    Int,

    /// Primitive floating-point type
    Float,

    /// Primitive boolean type
    Bool,

    /// Primitive string type
    String,

    /// Primitive character type
    Char,

    /// Unit type ()
    Unit,

    /// Type variable (unknown, to be solved during inference)
    Var(TypeVar),

    /// Function type: a -> b
    Fun(Box<Type>, Box<Type>),

    /// Tuple type: (a, b, c)
    Tuple(Vec<Type>),

    /// List type: [a]
    List(Box<Type>),

    /// Named type with type parameters: Option<Int>, Result<T, E>
    Named(String, Vec<Type>),

    /// Record type: { name: String, age: Int }
    Record(String, Vec<(String, Type)>),
}

impl Type {
    /// Create a function type.
    pub fn fun(from: Type, to: Type) -> Type {
        Type::Fun(Box::new(from), Box::new(to))
    }

    /// Create a list type.
    pub fn list(elem: Type) -> Type {
        Type::List(Box::new(elem))
    }

    /// Create a named type with no parameters.
    pub fn named(name: &str) -> Type {
        Type::Named(name.to_string(), vec![])
    }

    /// Create a named type with parameters.
    pub fn named_with(name: &str, params: Vec<Type>) -> Type {
        Type::Named(name.to_string(), params)
    }

    /// Collect all free type variables in this type.
    pub fn free_vars(&self) -> HashSet<TypeVar> {
        let mut vars = HashSet::new();
        self.collect_free_vars(&mut vars);
        vars
    }

    fn collect_free_vars(&self, vars: &mut HashSet<TypeVar>) {
        match self {
            Type::Int | Type::Float | Type::Bool | Type::String | Type::Char | Type::Unit => {}
            Type::Var(v) => {
                vars.insert(*v);
            }
            Type::Fun(a, b) => {
                a.collect_free_vars(vars);
                b.collect_free_vars(vars);
            }
            Type::Tuple(elems) => {
                for elem in elems {
                    elem.collect_free_vars(vars);
                }
            }
            Type::List(elem) => {
                elem.collect_free_vars(vars);
            }
            Type::Named(_, args) => {
                for arg in args {
                    arg.collect_free_vars(vars);
                }
            }
            Type::Record(_, fields) => {
                for (_, ty) in fields {
                    ty.collect_free_vars(vars);
                }
            }
        }
    }

    /// Check if this type contains the given type variable.
    pub fn contains_var(&self, v: TypeVar) -> bool {
        match self {
            Type::Int | Type::Float | Type::Bool | Type::String | Type::Char | Type::Unit => false,
            Type::Var(v2) => *v2 == v,
            Type::Fun(a, b) => a.contains_var(v) || b.contains_var(v),
            Type::Tuple(elems) => elems.iter().any(|e| e.contains_var(v)),
            Type::List(elem) => elem.contains_var(v),
            Type::Named(_, args) => args.iter().any(|a| a.contains_var(v)),
            Type::Record(_, fields) => fields.iter().any(|(_, ty)| ty.contains_var(v)),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Bool => write!(f, "Bool"),
            Type::String => write!(f, "String"),
            Type::Char => write!(f, "Char"),
            Type::Unit => write!(f, "()"),
            Type::Var(v) => write!(f, "{}", v),
            Type::Fun(a, b) => {
                // Parenthesize function types on the left side
                match a.as_ref() {
                    Type::Fun(_, _) => write!(f, "({}) -> {}", a, b),
                    _ => write!(f, "{} -> {}", a, b),
                }
            }
            Type::Tuple(elems) => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, ")")
            }
            Type::List(elem) => write!(f, "[{}]", elem),
            Type::Named(name, args) => {
                write!(f, "{}", name)?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Type::Record(name, fields) => {
                write!(f, "{} {{ ", name)?;
                for (i, (field_name, ty)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", field_name, ty)?;
                }
                write!(f, " }}")
            }
        }
    }
}

/// A type scheme (polymorphic type): forall a b. a -> b -> a
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scheme {
    /// Bound (quantified) type variables
    pub vars: Vec<TypeVar>,
    /// The underlying type
    pub ty: Type,
}

impl Scheme {
    /// Create a monomorphic scheme
    pub fn mono(ty: Type) -> Scheme {
        Scheme { vars: vec![], ty }
    }

    /// Create a polymorphic scheme
    pub fn poly(vars: Vec<TypeVar>, ty: Type) -> Scheme {
        Scheme { vars, ty }
    }

    /// Collect free type variables (not bound by this scheme)
    pub fn free_vars(&self) -> HashSet<TypeVar> {
        let mut vars = self.ty.free_vars();
        for v in &self.vars {
            vars.remove(v);
        }
        vars
    }
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "forall ")?;
            for (i, v) in self.vars.iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", v)?;
            }
            write!(f, ". {}", self.ty)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_var_display() {
        assert_eq!(TypeVar(0).to_string(), "a");
        assert_eq!(TypeVar(1).to_string(), "b");
        assert_eq!(TypeVar(25).to_string(), "z");
        assert_eq!(TypeVar(26).to_string(), "a1");
        assert_eq!(TypeVar(27).to_string(), "b1");
    }

    #[test]
    fn test_type_display() {
        assert_eq!(Type::Int.to_string(), "Int");
        assert_eq!(Type::fun(Type::Int, Type::Bool).to_string(), "Int -> Bool");
        assert_eq!(
            Type::fun(Type::fun(Type::Int, Type::Int), Type::Bool).to_string(),
            "(Int -> Int) -> Bool"
        );
        assert_eq!(Type::list(Type::Int).to_string(), "[Int]");
        assert_eq!(Type::Tuple(vec![Type::Int, Type::String]).to_string(), "(Int, String)");
        assert_eq!(Type::named_with("Option", vec![Type::Int]).to_string(), "Option<Int>");
    }

    #[test]
    fn test_free_vars() {
        let a = TypeVar(0);
        let b = TypeVar(1);

        let ty = Type::fun(Type::Var(a), Type::Var(b));
        let vars = ty.free_vars();
        assert!(vars.contains(&a));
        assert!(vars.contains(&b));
        assert_eq!(vars.len(), 2);
    }

    #[test]
    fn test_scheme_free_vars() {
        let a = TypeVar(0);
        let b = TypeVar(1);

        let scheme = Scheme::poly(vec![a], Type::fun(Type::Var(a), Type::Var(b)));
        let vars = scheme.free_vars();
        assert!(!vars.contains(&a));
        assert!(vars.contains(&b));
        assert_eq!(vars.len(), 1);
    }

    #[test]
    fn test_contains_var() {
        let a = TypeVar(0);
        let b = TypeVar(1);

        let ty = Type::fun(Type::Var(a), Type::Int);
        assert!(ty.contains_var(a));
        assert!(!ty.contains_var(b));
    }
}
