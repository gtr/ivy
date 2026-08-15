use crate::subst::Subst;
use std::collections::{HashMap, HashSet};
use std::fmt;

/// Unique identifier for type variables during inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

    /// Two types "overlap" if there's an assignment of their free vars that makes them equal
    pub fn overlaps(&self, other: &Type) -> bool {
        let mut subst = HashMap::<TypeVar, Type>::new();
        Self::overlaps_walk(self, other, &mut subst)
    }

    fn overlaps_walk(a: &Type, b: &Type, subst: &mut HashMap<TypeVar, Type>) -> bool {
        let resolve = |t: &Type, subst: &HashMap<TypeVar, Type>| -> Type {
            let mut current = t.clone();
            while let Type::Var(v) = &current {
                match subst.get(v) {
                    Some(next) if next != &current => current = next.clone(),
                    _ => break,
                }
            }
            current
        };
        // TODO(gtr): pretty sure this is bad perf
        let a = resolve(a, subst);
        let b = resolve(b, subst);
        match (&a, &b) {
            (Type::Var(v1), Type::Var(v2)) if v1 == v2 => true,
            (Type::Var(v), other) | (other, Type::Var(v)) => {
                subst.insert(*v, other.clone());
                true
            }
            (Type::Int, Type::Int)
            | (Type::Float, Type::Float)
            | (Type::Bool, Type::Bool)
            | (Type::String, Type::String)
            | (Type::Char, Type::Char)
            | (Type::Unit, Type::Unit) => true,
            (Type::Fun(p1, r1), Type::Fun(p2, r2)) => {
                Self::overlaps_walk(p1, p2, subst) && Self::overlaps_walk(r1, r2, subst)
            }
            (Type::Tuple(xs), Type::Tuple(ys)) => {
                xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| Self::overlaps_walk(x, y, subst))
            }
            (Type::List(x), Type::List(y)) => Self::overlaps_walk(x, y, subst),
            (Type::Named(n1, a1), Type::Named(n2, a2)) => {
                n1 == n2
                    && a1.len() == a2.len()
                    && a1.iter().zip(a2.iter()).all(|(x, y)| Self::overlaps_walk(x, y, subst))
            }
            (Type::Record(n1, f1), Type::Record(n2, f2)) => {
                n1 == n2
                    && f1.len() == f2.len()
                    && f1
                        .iter()
                        .zip(f2.iter())
                        .all(|((na, ta), (nb, tb))| na == nb && Self::overlaps_walk(ta, tb, subst))
            }
            _ => false,
        }
    }

    pub fn normalize(&self) -> Type {
        let mut mapping: HashMap<TypeVar, TypeVar> = HashMap::new();
        let mut next_id: u32 = 0;
        self.normalize_with_mapping(&mut mapping, &mut next_id)
    }

    fn normalize_with_mapping(&self, mapping: &mut HashMap<TypeVar, TypeVar>, next_id: &mut u32) -> Type {
        match self {
            Type::Int => Type::Int,
            Type::Float => Type::Float,
            Type::Bool => Type::Bool,
            Type::String => Type::String,
            Type::Char => Type::Char,
            Type::Unit => Type::Unit,
            Type::Var(v) => {
                let normalized = *mapping.entry(*v).or_insert_with(|| {
                    let id = *next_id;
                    *next_id += 1;
                    TypeVar(id)
                });
                Type::Var(normalized)
            }
            Type::Fun(a, b) => {
                let a_norm = a.normalize_with_mapping(mapping, next_id);
                let b_norm = b.normalize_with_mapping(mapping, next_id);
                Type::fun(a_norm, b_norm)
            }
            Type::Tuple(elems) => {
                let elems_norm: Vec<Type> = elems
                    .iter()
                    .map(|e| e.normalize_with_mapping(mapping, next_id))
                    .collect();
                Type::Tuple(elems_norm)
            }
            Type::List(elem) => Type::list(elem.normalize_with_mapping(mapping, next_id)),
            Type::Named(name, args) => {
                let args_norm: Vec<Type> = args
                    .iter()
                    .map(|a| a.normalize_with_mapping(mapping, next_id))
                    .collect();
                Type::Named(name.clone(), args_norm)
            }
            Type::Record(name, fields) => {
                let fields_norm: Vec<(String, Type)> = fields
                    .iter()
                    .map(|(n, ty)| (n.clone(), ty.normalize_with_mapping(mapping, next_id)))
                    .collect();
                Type::Record(name.clone(), fields_norm)
            }
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
            Type::Fun(a, b) => match a.as_ref() {
                Type::Fun(_, _) => write!(f, "({}) -> {}", a, b),
                _ => write!(f, "{} -> {}", a, b),
            },
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

/// A trait constraint: `Show a` means "type `a` must implement `Show`"
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitConstraint {
    pub trait_name: String,
    pub type_args: Vec<Type>,
}

impl TraitConstraint {
    pub fn covers(&self, other: &TraitConstraint) -> bool {
        if self.trait_name != other.trait_name || self.type_args.len() != other.type_args.len() {
            return false;
        }
        let mut bindings: HashMap<TypeVar, Type> = HashMap::new();
        for (lhs, rhs) in self.type_args.iter().zip(other.type_args.iter()) {
            if !match_one(lhs, rhs, &mut bindings) {
                return false;
            }
        }
        true
    }
}

fn match_one(pattern: &Type, target: &Type, bindings: &mut HashMap<TypeVar, Type>) -> bool {
    match (pattern, target) {
        (Type::Int, Type::Int)
        | (Type::Float, Type::Float)
        | (Type::Bool, Type::Bool)
        | (Type::String, Type::String)
        | (Type::Char, Type::Char)
        | (Type::Unit, Type::Unit) => true,
        (Type::Var(v), _) => match bindings.get(v) {
            Some(prev) => prev == target,
            None => {
                bindings.insert(*v, target.clone());
                true
            }
        },
        (Type::Fun(p1, r1), Type::Fun(p2, r2)) => match_one(p1, p2, bindings) && match_one(r1, r2, bindings),
        (Type::Tuple(a), Type::Tuple(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| match_one(x, y, bindings))
        }
        (Type::List(a), Type::List(b)) => match_one(a, b, bindings),
        (Type::Named(n1, a1), Type::Named(n2, a2)) => {
            n1 == n2 && a1.len() == a2.len() && a1.iter().zip(a2.iter()).all(|(x, y)| match_one(x, y, bindings))
        }
        (Type::Record(n1, f1), Type::Record(n2, f2)) => {
            n1 == n2
                && f1.len() == f2.len()
                && f1
                    .iter()
                    .zip(f2.iter())
                    .all(|((na, ta), (nb, tb))| na == nb && match_one(ta, tb, bindings))
        }
        _ => false,
    }
}

impl fmt::Display for TraitConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}<", self.trait_name)?;
        for (i, ty) in self.type_args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", ty)?;
        }
        write!(f, ">")
    }
}

/// A type scheme (polymorphic type): forall a b. C => a -> b -> a
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scheme {
    /// Bound (quantified) type variables
    pub vars: Vec<TypeVar>,
    /// Trait constraints carried by this scheme.
    pub constraints: Vec<TraitConstraint>,
    /// The underlying type
    pub ty: Type,
}

impl Scheme {
    /// Create a monomorphic scheme
    pub fn mono(ty: Type) -> Scheme {
        Scheme {
            vars: vec![],
            constraints: vec![],
            ty,
        }
    }

    /// Create a polymorphic scheme without constraints
    pub fn poly(vars: Vec<TypeVar>, ty: Type) -> Scheme {
        Scheme {
            vars,
            constraints: vec![],
            ty,
        }
    }

    /// Create a polymorphic scheme with constraints
    pub fn poly_with_constraints(vars: Vec<TypeVar>, constraints: Vec<TraitConstraint>, ty: Type) -> Scheme {
        Scheme { vars, constraints, ty }
    }

    /// Collect free type variables (not bound by this scheme)
    pub fn free_vars(&self) -> HashSet<TypeVar> {
        let mut vars = self.ty.free_vars();
        for v in &self.vars {
            vars.remove(v);
        }
        vars
    }

    /// Replace this scheme's bound vars with fresh ones produced by `next_var`
    ///
    /// Returns the instantiated type and the freshened constraints
    pub fn instantiate<F: FnMut() -> Type>(&self, mut next_var: F) -> (Type, Vec<TraitConstraint>) {
        if self.vars.is_empty() {
            return (self.ty.clone(), self.constraints.clone());
        }

        let mapping: HashMap<TypeVar, Type> = self.vars.iter().map(|v| (*v, next_var())).collect();
        let subst = Subst::from_mappings(mapping);
        let ty = subst.apply(&self.ty);
        let constraints = self
            .constraints
            .iter()
            .map(|c| TraitConstraint {
                trait_name: c.trait_name.clone(),
                type_args: c.type_args.iter().map(|t| subst.apply(t)).collect(),
            })
            .collect();

        (ty, constraints)
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

    #[test]
    fn test_normalize() {
        let v100 = TypeVar(100);
        let v200 = TypeVar(200);
        let ty = Type::fun(
            Type::Var(v100),
            Type::named_with("Result", vec![Type::Var(v100), Type::Var(v200)]),
        );
        let normalized = ty.normalize();
        let expected = Type::fun(
            Type::Var(TypeVar(0)),
            Type::named_with("Result", vec![Type::Var(TypeVar(0)), Type::Var(TypeVar(1))]),
        );
        assert_eq!(normalized, expected);
        assert_eq!(normalized.to_string(), "a -> Result<a, b>");
    }

    #[test]
    fn test_normalize_preserves_structure() {
        let v50 = TypeVar(50);
        let v60 = TypeVar(60);
        let ty = Type::fun(
            Type::Tuple(vec![Type::Var(v50), Type::Var(v60)]),
            Type::Tuple(vec![Type::Var(v60), Type::Var(v50)]),
        );
        let normalized = ty.normalize();
        assert_eq!(normalized.to_string(), "(a, b) -> (b, a)");
    }
}
