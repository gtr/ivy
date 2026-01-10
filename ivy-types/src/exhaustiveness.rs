//! Exhaustiveness checking for pattern matching.
//!
//! This implements a simplified version of the pattern exhaustiveness
//! algorithm described in the  "Warnings for Pattern Matching" paper by Maranget (2007).
//! http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

use ivy_syntax::pattern::Pattern;
use ivy_syntax::Span;

use crate::error::{TypeError, TypeErrorKind, TypeResult};
use crate::registry::TypeRegistry;
use crate::types::Type;

/// Check that a match expression is exhaustive.
///
/// Returns Ok(()) if all cases are covered, or an error listing missing patterns.
pub fn check_exhaustiveness(
    scrutinee_ty: &Type,
    patterns: &[&Pattern],
    registry: &TypeRegistry,
    span: Span,
) -> TypeResult<()> {
    let missing = find_missing_patterns(scrutinee_ty, patterns, registry);

    if !missing.is_empty() {
        return Err(TypeError::new(TypeErrorKind::NonExhaustive { missing }, span));
    }

    Ok(())
}

/// Find patterns that are not covered by the given pattern list.
///
/// returns a list of string descriptions of missing patterns.
pub fn find_missing_patterns(ty: &Type, patterns: &[&Pattern], registry: &TypeRegistry) -> Vec<String> {
    if patterns.iter().any(|p| is_catch_all(p)) {
        return vec![];
    }

    match ty {
        // sum types: check each constructor
        Type::Named(type_name, _) => {
            if let Some(constructors) = registry.get_constructors(type_name) {
                find_missing_constructors(constructors, patterns, registry)
            } else {
                vec![]
            }
        }

        // Bool: check true and false
        Type::Bool => find_missing_bool(patterns),

        // Unit: only () is possible
        Type::Unit => find_missing_unit(patterns),

        // Tuples: need to check all positions recursively
        Type::Tuple(elem_types) => find_missing_tuple(elem_types, patterns, registry),

        // Lists: check [] and (_ :: _) patterns
        Type::List(_elem_ty) => find_missing_list(patterns),

        // Primitives (Int, Float, String, Char): infinite domain
        // only exhaustive if there's a catch-all (which is already checked above)
        Type::Int | Type::Float | Type::String | Type::Char => {
            vec!["_".to_string()]
        }

        // TODO(gtr) Type variables: can't know constructors, assume OK for now...
        Type::Var(_) => vec![],

        // TODO(gtr): Function types: can't pattern match on functions. assume OK for now...
        Type::Fun(_, _) => vec![],

        // Records: single constructor, just check it exists
        Type::Record(name, _) => {
            if patterns.iter().any(|p| matches_record(p, name)) {
                vec![]
            } else {
                vec![format!("{} {{ ... }}", name)]
            }
        }
    }
}

/// Check if a pattern is a catch-all (matches anything).
fn is_catch_all(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Wildcard => true,
        Pattern::Var(_) => true,
        Pattern::Or { left, right } => is_catch_all(&left.node) || is_catch_all(&right.node),
        _ => false,
    }
}

/// Find missing constructors from a sum type.
fn find_missing_constructors(constructors: &[String], patterns: &[&Pattern], registry: &TypeRegistry) -> Vec<String> {
    let mut missing = Vec::new();

    for ctor_name in constructors {
        if !is_constructor_covered(ctor_name, patterns, registry) {
            if let Some(info) = registry.get_constructor_info(ctor_name) {
                if info.arity == 0 {
                    missing.push(ctor_name.clone());
                } else {
                    let args: Vec<&str> = (0..info.arity).map(|_| "_").collect();
                    missing.push(format!("{}({})", ctor_name, args.join(", ")));
                }
            } else {
                missing.push(ctor_name.clone());
            }
        }
    }

    missing
}

/// Check if a constructor is covered by any pattern.
fn is_constructor_covered(ctor_name: &str, patterns: &[&Pattern], registry: &TypeRegistry) -> bool {
    for pattern in patterns {
        if covers_constructor(pattern, ctor_name, registry) {
            return true;
        }
    }
    false
}

/// Check if a single pattern covers a constructor.
fn covers_constructor(pattern: &Pattern, ctor_name: &str, registry: &TypeRegistry) -> bool {
    match pattern {
        Pattern::Wildcard | Pattern::Var(_) => true,
        Pattern::Constructor { name, .. } => name.name == ctor_name,
        Pattern::Or { left, right } => {
            covers_constructor(&left.node, ctor_name, registry) || covers_constructor(&right.node, ctor_name, registry)
        }
        _ => false,
    }
}

fn find_missing_bool(patterns: &[&Pattern]) -> Vec<String> {
    let mut has_true = false;
    let mut has_false = false;

    for pattern in patterns {
        match pattern {
            Pattern::Lit(ivy_syntax::lit::Literal::Bool(true)) => has_true = true,
            Pattern::Lit(ivy_syntax::lit::Literal::Bool(false)) => has_false = true,
            Pattern::Or { left, right } => {
                let left_missing = find_missing_bool(&[&left.node]);
                let right_missing = find_missing_bool(&[&right.node]);
                if left_missing.iter().all(|m| m != "true") || right_missing.iter().all(|m| m != "true") {
                    has_true = true;
                }
                if left_missing.iter().all(|m| m != "false") || right_missing.iter().all(|m| m != "false") {
                    has_false = true;
                }
            }
            _ => {}
        }
    }

    let mut missing = Vec::new();
    if !has_true {
        missing.push("true".to_string());
    }
    if !has_false {
        missing.push("false".to_string());
    }
    missing
}

fn find_missing_unit(patterns: &[&Pattern]) -> Vec<String> {
    for pattern in patterns {
        match pattern {
            Pattern::Lit(ivy_syntax::lit::Literal::Unit) => return vec![],
            Pattern::Tuple { elements } if elements.is_empty() => return vec![],
            Pattern::Or { left, right } => {
                if find_missing_unit(&[&left.node]).is_empty() || find_missing_unit(&[&right.node]).is_empty() {
                    return vec![];
                }
            }
            _ => {}
        }
    }
    vec!["()".to_string()]
}

fn find_missing_tuple(elem_types: &[Type], patterns: &[&Pattern], registry: &TypeRegistry) -> Vec<String> {
    let tuple_patterns: Vec<&Vec<ivy_syntax::Spanned<Pattern>>> = patterns
        .iter()
        .filter_map(|p| match p {
            Pattern::Tuple { elements } if elements.len() == elem_types.len() => Some(elements),
            _ => None,
        })
        .collect();

    if tuple_patterns.is_empty() {
        let wildcards: Vec<&str> = (0..elem_types.len()).map(|_| "_").collect();
        return vec![format!("({})", wildcards.join(", "))];
    }

    for (i, elem_ty) in elem_types.iter().enumerate() {
        let position_patterns: Vec<&Pattern> = tuple_patterns.iter().map(|tp| &tp[i].node).collect();

        let missing = find_missing_patterns(elem_ty, &position_patterns, registry);
        if !missing.is_empty() {
            let mut parts: Vec<String> = (0..elem_types.len()).map(|_| "_".to_string()).collect();
            parts[i] = missing[0].clone();
            return vec![format!("({})", parts.join(", "))];
        }
    }

    vec![]
}

fn find_missing_list(patterns: &[&Pattern]) -> Vec<String> {
    let mut has_empty = false;
    let mut has_cons = false;

    for pattern in patterns {
        match pattern {
            Pattern::List { elements } if elements.is_empty() => has_empty = true,
            Pattern::List { elements } if !elements.is_empty() => {
                if elements.len() == 1 {
                    if matches!(&elements[0].node, Pattern::Cons { .. }) {
                        has_cons = true;
                    }
                }
            }
            Pattern::Cons { .. } => has_cons = true,
            Pattern::Or { left, right } => {
                let left_missing = find_missing_list(&[&left.node]);
                let right_missing = find_missing_list(&[&right.node]);
                if left_missing.iter().all(|m| m != "[]") || right_missing.iter().all(|m| m != "[]") {
                    has_empty = true;
                }
                if left_missing.iter().all(|m| m != "(_ :: _)") || right_missing.iter().all(|m| m != "(_ :: _)") {
                    has_cons = true;
                }
            }
            _ => {}
        }
    }

    let mut missing = Vec::new();
    if !has_empty {
        missing.push("[]".to_string());
    }
    if !has_cons {
        missing.push("(_ :: _)".to_string());
    }
    missing
}

fn matches_record(pattern: &Pattern, record_name: &str) -> bool {
    match pattern {
        Pattern::Record { name, .. } => name.name == record_name,
        Pattern::Or { left, right } => {
            matches_record(&left.node, record_name) || matches_record(&right.node, record_name)
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ivy_syntax::ast::Ident;
    use ivy_syntax::Span;

    fn span() -> Span {
        Span::new(0, 0)
    }

    fn ident(name: &str) -> Ident {
        Ident::new(name.to_string(), span())
    }

    #[test]
    fn test_wildcard_is_exhaustive() {
        let registry = TypeRegistry::with_builtins();
        let ty = Type::named_with("Option", vec![Type::Int]);
        let patterns: Vec<&Pattern> = vec![&Pattern::Wildcard];

        let missing = find_missing_patterns(&ty, &patterns, &registry);
        assert!(missing.is_empty());
    }

    #[test]
    fn test_missing_none() {
        let registry = TypeRegistry::with_builtins();
        let ty = Type::named_with("Option", vec![Type::Int]);

        let some_pattern = Pattern::Constructor {
            name: ident("Some"),
            args: vec![ivy_syntax::Spanned {
                node: Pattern::Wildcard,
                span: span(),
            }],
        };
        let patterns: Vec<&Pattern> = vec![&some_pattern];

        let missing = find_missing_patterns(&ty, &patterns, &registry);
        assert_eq!(missing, vec!["None"]);
    }

    #[test]
    fn test_option_exhaustive() {
        let registry = TypeRegistry::with_builtins();
        let ty = Type::named_with("Option", vec![Type::Int]);

        let some_pattern = Pattern::Constructor {
            name: ident("Some"),
            args: vec![ivy_syntax::Spanned {
                node: Pattern::Wildcard,
                span: span(),
            }],
        };
        let none_pattern = Pattern::Constructor {
            name: ident("None"),
            args: vec![],
        };
        let patterns: Vec<&Pattern> = vec![&some_pattern, &none_pattern];

        let missing = find_missing_patterns(&ty, &patterns, &registry);
        assert!(missing.is_empty());
    }

    #[test]
    fn test_bool_missing_false() {
        let registry = TypeRegistry::with_builtins();
        let ty = Type::Bool;

        let true_pattern = Pattern::Lit(ivy_syntax::lit::Literal::Bool(true));
        let patterns: Vec<&Pattern> = vec![&true_pattern];

        let missing = find_missing_patterns(&ty, &patterns, &registry);
        assert_eq!(missing, vec!["false"]);
    }

    #[test]
    fn test_bool_exhaustive() {
        let registry = TypeRegistry::with_builtins();
        let ty = Type::Bool;

        let true_pattern = Pattern::Lit(ivy_syntax::lit::Literal::Bool(true));
        let false_pattern = Pattern::Lit(ivy_syntax::lit::Literal::Bool(false));
        let patterns: Vec<&Pattern> = vec![&true_pattern, &false_pattern];

        let missing = find_missing_patterns(&ty, &patterns, &registry);
        assert!(missing.is_empty());
    }

    #[test]
    fn test_list_missing_empty() {
        let registry = TypeRegistry::with_builtins();
        let ty = Type::list(Type::Int);

        let cons_pattern = Pattern::Cons {
            head: Box::new(ivy_syntax::Spanned {
                node: Pattern::Wildcard,
                span: span(),
            }),
            tail: Box::new(ivy_syntax::Spanned {
                node: Pattern::Wildcard,
                span: span(),
            }),
        };
        let patterns: Vec<&Pattern> = vec![&cons_pattern];

        let missing = find_missing_patterns(&ty, &patterns, &registry);
        assert_eq!(missing, vec!["[]"]);
    }
}
