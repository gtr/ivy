use ivy_syntax::lit::Literal;
use ivy_syntax::pattern::Pattern;
use ivy_syntax::{Span, Spanned};

use super::matrix::{missing_patterns, row_count, rows, MAX_ROWS};
use crate::error::{TypeError, TypeResult};
use crate::registry::TypeRegistry;
use crate::types::Type;

const MAX_SHOWN: usize = 3;

pub(crate) fn check_exhaustiveness(
    scrutinee_ty: &Type,
    patterns: &[&Pattern],
    registry: &TypeRegistry,
    span: Span,
) -> TypeResult<()> {
    let missing = find_missing_patterns(scrutinee_ty, patterns, registry);

    if !missing.is_empty() {
        return Err(TypeError::NonExhaustive { missing, span });
    }

    Ok(())
}

fn find_missing_patterns(ty: &Type, patterns: &[&Pattern], registry: &TypeRegistry) -> Vec<String> {
    // records aren't modeled by the matrix engine (a record pattern reads as a wildcard there).
    // handle the single-constructor type directly
    if let Type::Record(name, _) = ty {
        return if patterns.iter().any(|p| matches_record(p, name)) {
            vec![]
        } else {
            vec![format!("{} {{ ... }}", name)]
        };
    }
    if matches!(ty, Type::Var(_) | Type::Fun(_, _)) {
        return vec![];
    }
    // too many or-pattern combinations to check in reasonable time, so accept the match
    if patterns.iter().map(|p| row_count(&[p])).fold(0, usize::saturating_add) > MAX_ROWS {
        return vec![];
    }
    let matrix: Vec<Vec<Pattern>> = patterns.iter().copied().flat_map(|p| rows(&[p])).collect();

    let witnesses = missing_patterns(&matrix, 1, MAX_SHOWN + 1, registry);
    let mut missing: Vec<String> = witnesses.iter().take(MAX_SHOWN).map(|w| describe(&w[0])).collect();
    if witnesses.len() > MAX_SHOWN {
        missing.push("...".to_string());
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

fn describe(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Wildcard | Pattern::Var(_) => "_".to_string(),
        Pattern::Lit(Literal::Bool(b)) => b.to_string(),
        Pattern::Lit(Literal::Unit) => "()".to_string(),
        Pattern::Lit(Literal::Int(n)) => n.to_string(),
        Pattern::Lit(Literal::Char(c)) => format!("'{}'", c),
        Pattern::Lit(Literal::String(s)) => format!("\"{}\"", s),
        Pattern::Lit(Literal::Float(f)) => f.to_string(),
        Pattern::Constructor { name, args } if args.is_empty() => name.name.clone(),
        Pattern::Constructor { name, args } => format!("{}({})", name.name, describe_all(args)),
        Pattern::Tuple { elements } => format!("({})", describe_all(elements)),
        Pattern::List { elements } if elements.is_empty() => "[]".to_string(),
        Pattern::List { elements } => format!("[{}]", describe_all(elements)),
        Pattern::Cons { head, tail } => format!("[{} | {}]", describe(&head.node), describe(&tail.node)),
        Pattern::Record { name, .. } => format!("{} {{ ... }}", name.name),
        Pattern::Or { left, .. } => describe(&left.node),
    }
}

fn describe_all(elements: &[Spanned<Pattern>]) -> String {
    elements
        .iter()
        .map(|e| describe(&e.node))
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(test)]
mod tests {
    use super::*;
    use ivy_syntax::ast::Ident;

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

        let true_pattern = Pattern::Lit(Literal::Bool(true));
        let patterns: Vec<&Pattern> = vec![&true_pattern];

        let missing = find_missing_patterns(&ty, &patterns, &registry);
        assert_eq!(missing, vec!["false"]);
    }

    #[test]
    fn test_bool_exhaustive() {
        let registry = TypeRegistry::with_builtins();
        let ty = Type::Bool;

        let true_pattern = Pattern::Lit(Literal::Bool(true));
        let false_pattern = Pattern::Lit(Literal::Bool(false));
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

    fn missing_for_arms(ty: &Type, arms: &str) -> Vec<String> {
        let program = ivy_parse::parse(&format!("let r = match x with {arms} end;")).expect("parse failed");
        let ivy_syntax::Decl::Let { value, .. } = &program.declarations[0].node else {
            panic!("expected a let");
        };
        let ivy_syntax::Expr::Match { arms, .. } = &value.node else {
            panic!("expected a match");
        };
        let patterns: Vec<&Pattern> = arms.iter().map(|arm| &arm.pattern.node).collect();
        find_missing_patterns(ty, &patterns, &TypeRegistry::with_builtins())
    }

    #[test]
    fn test_reports_every_missing_case() {
        let ty = Type::Tuple(vec![Type::Bool, Type::Bool]);
        let missing = missing_for_arms(&ty, "| (true, true) -> 1 | (false, false) -> 0");
        assert_eq!(missing, vec!["(true, false)", "(false, true)"]);
    }

    #[test]
    fn test_long_missing_list_is_truncated() {
        let small = ["true|false"; 3].join(", ");
        let ty = Type::Tuple(vec![Type::Bool; 4]);
        let missing = missing_for_arms(&ty, &format!("| ({small}, true) -> 0"));
        assert_eq!(
            missing,
            vec![
                "(true, true, true, false)",
                "(true, true, false, false)",
                "(true, false, true, false)",
                "..."
            ]
        );
    }

    #[test]
    fn test_nested_constructor_case_is_missing() {
        let ty = Type::named_with("Option", vec![Type::Int]);
        assert_eq!(missing_for_arms(&ty, "| Some(1) -> 1 | None -> 0"), vec!["Some(_)"]);
    }

    #[test]
    fn test_or_pattern_blowup_is_skipped() {
        let wide = ["true|false"; 22].join(", ");
        let ty = Type::Tuple(vec![Type::Bool; 23]);
        assert!(missing_for_arms(&ty, &format!("| ({wide}, true) -> 0")).is_empty());
    }
}
