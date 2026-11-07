//! Exhaustiveness checking for pattern matching.
//!
//! This implements a simplified version of the pattern exhaustiveness
//! algorithm described in the  "Warnings for Pattern Matching" paper by Maranget (2007).
//! http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

use ivy_syntax::decl::{Decl, FnDecl};
use ivy_syntax::lit::Literal;
use ivy_syntax::pattern::Pattern;
use ivy_syntax::{Param, Span, Spanned};
use std::collections::HashMap;

use crate::error::{TypeError, TypeResult, TypeWarning};
use crate::registry::TypeRegistry;
use crate::types::Type;

const MAX_ROWS: usize = 4096;

fn covers(outer: &Pattern, inner: &Pattern) -> bool {
    match (outer, inner) {
        (Pattern::Wildcard | Pattern::Var(_), _) => true,
        (Pattern::Or { left, right }, _) => covers(&left.node, inner) || covers(&right.node, inner),
        (_, Pattern::Or { left, right }) => covers(outer, &left.node) && covers(outer, &right.node),
        (Pattern::Lit(a), Pattern::Lit(b)) => a == b,
        (Pattern::Constructor { name: n1, args: a1 }, Pattern::Constructor { name: n2, args: a2 }) => {
            n1.name == n2.name && a1.len() == a2.len() && a1.iter().zip(a2).all(|(x, y)| covers(&x.node, &y.node))
        }
        (Pattern::Tuple { elements: e1 }, Pattern::Tuple { elements: e2 }) => {
            e1.len() == e2.len() && e1.iter().zip(e2).all(|(x, y)| covers(&x.node, &y.node))
        }
        (Pattern::List { elements: e1 }, Pattern::List { elements: e2 }) => {
            e1.len() == e2.len() && e1.iter().zip(e2).all(|(x, y)| covers(&x.node, &y.node))
        }
        (Pattern::Cons { head: h1, tail: t1 }, Pattern::Cons { head: h2, tail: t2 }) => {
            covers(&h1.node, &h2.node) && covers(&t1.node, &t2.node)
        }
        _ => false,
    }
}

/// A pattern's head constructor, normalized across every shape Ivy matches
#[derive(Clone, PartialEq)]
enum Head {
    Variant(String),
    Bool(bool),
    Unit,
    Tuple(usize),
    Nil,
    Cons,
    Int(i64),
    Char(char),
    Str(String),
    Float(u64),
}

fn deconstruct(pattern: &Pattern) -> Option<(Head, Vec<Pattern>)> {
    match pattern {
        Pattern::Wildcard | Pattern::Var(_) => None,
        Pattern::Lit(Literal::Bool(b)) => Some((Head::Bool(*b), vec![])),
        Pattern::Lit(Literal::Unit) => Some((Head::Unit, vec![])),
        Pattern::Lit(Literal::Int(n)) => Some((Head::Int(*n), vec![])),
        Pattern::Lit(Literal::Char(c)) => Some((Head::Char(*c), vec![])),
        Pattern::Lit(Literal::String(s)) => Some((Head::Str(s.clone()), vec![])),
        Pattern::Lit(Literal::Float(f)) => Some((Head::Float(f.to_bits()), vec![])),
        Pattern::Constructor { name, args } => Some((
            Head::Variant(name.name.clone()),
            args.iter().map(|a| a.node.clone()).collect(),
        )),
        Pattern::Tuple { elements } => Some((
            Head::Tuple(elements.len()),
            elements.iter().map(|e| e.node.clone()).collect(),
        )),
        Pattern::List { elements } if elements.is_empty() => Some((Head::Nil, vec![])),
        Pattern::List { elements } => {
            let head = elements[0].node.clone();
            let tail = Pattern::List {
                elements: elements[1..].to_vec(),
            };
            Some((Head::Cons, vec![head, tail]))
        }
        Pattern::Cons { head, tail } => Some((Head::Cons, vec![head.node.clone(), tail.node.clone()])),
        Pattern::Or { .. } | Pattern::Record { .. } => None,
    }
}

fn expand(pattern: &Pattern) -> Vec<Pattern> {
    match pattern {
        Pattern::Or { left, right } => {
            let mut alts = expand(&left.node);
            alts.extend(expand(&right.node));
            alts
        }
        Pattern::Constructor { name, args } => product(args)
            .into_iter()
            .map(|combo| Pattern::Constructor {
                name: name.clone(),
                args: combo,
            })
            .collect(),
        Pattern::Tuple { elements } => product(elements)
            .into_iter()
            .map(|combo| Pattern::Tuple { elements: combo })
            .collect(),
        Pattern::List { elements } => product(elements)
            .into_iter()
            .map(|combo| Pattern::List { elements: combo })
            .collect(),
        Pattern::Cons { head, tail } => {
            let mut out = Vec::new();
            for h in expand(&head.node) {
                for t in expand(&tail.node) {
                    out.push(Pattern::Cons {
                        head: Box::new(Spanned::new(h.clone(), head.span)),
                        tail: Box::new(Spanned::new(t, tail.span)),
                    });
                }
            }
            out
        }
        other => vec![other.clone()],
    }
}

fn expansion_size(pattern: &Pattern) -> usize {
    match pattern {
        Pattern::Or { left, right } => expansion_size(&left.node).saturating_add(expansion_size(&right.node)),
        Pattern::Constructor { args, .. } => args
            .iter()
            .map(|a| expansion_size(&a.node))
            .fold(1, usize::saturating_mul),
        Pattern::Tuple { elements } | Pattern::List { elements } => elements
            .iter()
            .map(|e| expansion_size(&e.node))
            .fold(1, usize::saturating_mul),
        Pattern::Cons { head, tail } => expansion_size(&head.node).saturating_mul(expansion_size(&tail.node)),
        _ => 1,
    }
}

fn product(elements: &[Spanned<Pattern>]) -> Vec<Vec<Spanned<Pattern>>> {
    let mut combos = vec![vec![]];
    for element in elements {
        let alts = expand(&element.node);
        let mut next = Vec::new();
        for combo in &combos {
            for alt in &alts {
                let mut extended = combo.clone();
                extended.push(Spanned::new(alt.clone(), element.span));
                next.push(extended);
            }
        }
        combos = next;
    }
    combos
}

fn contains_record(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Record { .. } => true,
        Pattern::Constructor { args, .. } => args.iter().any(|a| contains_record(&a.node)),
        Pattern::Tuple { elements } | Pattern::List { elements } => elements.iter().any(|e| contains_record(&e.node)),
        Pattern::Cons { head, tail } => contains_record(&head.node) || contains_record(&tail.node),
        Pattern::Or { left, right } => contains_record(&left.node) || contains_record(&right.node),
        _ => false,
    }
}

fn specialize(matrix: &[Vec<Pattern>], head: &Head, arity: usize) -> Vec<Vec<Pattern>> {
    let mut out = Vec::new();
    for row in matrix {
        match deconstruct(&row[0]) {
            None => {
                let mut new_row = vec![Pattern::Wildcard; arity];
                new_row.extend_from_slice(&row[1..]);
                out.push(new_row);
            }
            Some((h, args)) if h == *head => {
                let mut new_row = args;
                new_row.extend_from_slice(&row[1..]);
                out.push(new_row);
            }
            Some(_) => {}
        }
    }
    out
}

fn default_matrix(matrix: &[Vec<Pattern>]) -> Vec<Vec<Pattern>> {
    matrix
        .iter()
        .filter_map(|row| match deconstruct(&row[0]) {
            None => Some(row[1..].to_vec()),
            Some(_) => None,
        })
        .collect()
}

fn column_heads(matrix: &[Vec<Pattern>]) -> Vec<Head> {
    let mut heads: Vec<Head> = Vec::new();
    for row in matrix {
        if let Some((head, _)) = deconstruct(&row[0]) {
            if !heads.contains(&head) {
                heads.push(head);
            }
        }
    }
    heads
}

fn complete_signature(heads: &[Head], registry: &TypeRegistry) -> Option<Vec<(Head, usize)>> {
    let first = heads.first()?;
    match first {
        Head::Bool(_) => {
            let full = [Head::Bool(true), Head::Bool(false)];
            full.iter()
                .all(|h| heads.contains(h))
                .then(|| vec![(Head::Bool(true), 0), (Head::Bool(false), 0)])
        }
        Head::Unit => Some(vec![(Head::Unit, 0)]),
        Head::Tuple(n) => Some(vec![(Head::Tuple(*n), *n)]),
        Head::Nil | Head::Cons => {
            (heads.contains(&Head::Nil) && heads.contains(&Head::Cons)).then(|| vec![(Head::Nil, 0), (Head::Cons, 2)])
        }
        Head::Variant(name) => {
            let type_name = &registry.get_constructor_info(name)?.type_name;
            let ctors = registry.get_constructors(type_name)?;
            let all_present = ctors.iter().all(|c| heads.contains(&Head::Variant(c.clone())));
            all_present.then(|| {
                ctors
                    .iter()
                    .map(|c| {
                        let arity = registry.get_constructor_info(c).map_or(0, |i| i.arity);
                        (Head::Variant(c.clone()), arity)
                    })
                    .collect()
            })
        }
        Head::Int(_) | Head::Char(_) | Head::Str(_) | Head::Float(_) => None,
    }
}

fn useful(matrix: &[Vec<Pattern>], row: &[Pattern], registry: &TypeRegistry) -> bool {
    if row.is_empty() {
        return matrix.is_empty();
    }
    match deconstruct(&row[0]) {
        Some((head, args)) => {
            let arity = args.len();
            let specialized = specialize(matrix, &head, arity);
            let mut rest = args;
            rest.extend_from_slice(&row[1..]);
            useful(&specialized, &rest, registry)
        }
        None => match complete_signature(&column_heads(matrix), registry) {
            Some(full) => full.iter().any(|(head, arity)| {
                let specialized = specialize(matrix, head, *arity);
                let mut rest = vec![Pattern::Wildcard; *arity];
                rest.extend_from_slice(&row[1..]);
                useful(&specialized, &rest, registry)
            }),
            None => useful(&default_matrix(matrix), &row[1..], registry),
        },
    }
}

fn clause_rows(params: &[Param]) -> Vec<Vec<Pattern>> {
    let mut rows = vec![vec![]];
    for param in params {
        let alts = expand(&param.pattern.node);
        let mut next = Vec::new();
        for row in &rows {
            for alt in &alts {
                let mut extended = row.clone();
                extended.push(alt.clone());
                next.push(extended);
            }
        }
        rows = next;
    }
    rows
}

/// Warn about function clauses that can never match bc the clauses above them already cover every value
pub fn unreachable_clause_warnings(decls: &[Spanned<Decl>], registry: &TypeRegistry) -> Vec<TypeWarning> {
    let mut groups: HashMap<&str, Vec<&FnDecl>> = HashMap::new();
    let mut order: Vec<&str> = Vec::new();
    for decl in decls {
        if let Decl::Fn(fn_decl) = &decl.node {
            let name = fn_decl.name.name.as_str();
            if !groups.contains_key(name) {
                order.push(name);
            }
            groups.entry(name).or_default().push(fn_decl);
        }
    }

    let mut warnings = Vec::new();
    for name in order {
        let clauses = &groups[name];
        let width = clauses[0].params.len();
        if clauses.len() < 2 || clauses.iter().any(|c| c.params.len() != width) {
            continue;
        }
        if clauses
            .iter()
            .any(|c| c.params.iter().any(|p| contains_record(&p.pattern.node)))
        {
            continue;
        }
        let total_rows = clauses
            .iter()
            .map(|c| {
                c.params
                    .iter()
                    .map(|p| expansion_size(&p.pattern.node))
                    .fold(1, usize::saturating_mul)
            })
            .fold(0, usize::saturating_add);
        if total_rows > MAX_ROWS {
            continue;
        }

        let mut matrix: Vec<Vec<Pattern>> = Vec::new();
        for (index, clause) in clauses.iter().enumerate() {
            let rows = clause_rows(&clause.params);
            let reachable = rows.iter().any(|row| useful(&matrix, row, registry));
            if !reachable {
                // Point at a single earlier clause when one alone subsumes this one (the common catchall case)
                let covered_by = clauses[..index]
                    .iter()
                    .rev()
                    .find(|earlier| {
                        earlier.params.len() == width
                            && earlier
                                .params
                                .iter()
                                .zip(&clause.params)
                                .all(|(e, l)| covers(&e.pattern.node, &l.pattern.node))
                    })
                    .map(|earlier| earlier.span);
                warnings.push(TypeWarning::UnreachableClause {
                    name: name.to_string(),
                    span: clause.span,
                    covered_by,
                });
            }
            matrix.extend(rows);
        }
    }
    warnings
}

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
        return Err(TypeError::NonExhaustive { missing, span });
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

        // Lists: check [] and [_ | _] patterns
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

fn is_catch_all(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Wildcard => true,
        Pattern::Var(_) => true,
        Pattern::Or { left, right } => is_catch_all(&left.node) || is_catch_all(&right.node),
        _ => false,
    }
}

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

fn is_constructor_covered(ctor_name: &str, patterns: &[&Pattern], registry: &TypeRegistry) -> bool {
    for pattern in patterns {
        if covers_constructor(pattern, ctor_name, registry) {
            return true;
        }
    }
    false
}

fn covers_constructor(pattern: &Pattern, ctor_name: &str, _registry: &TypeRegistry) -> bool {
    match pattern {
        Pattern::Wildcard | Pattern::Var(_) => true,
        Pattern::Constructor { name, .. } => name.name == ctor_name,
        Pattern::Or { left, right } => {
            covers_constructor(&left.node, ctor_name, _registry)
                || covers_constructor(&right.node, ctor_name, _registry)
        }
        _ => false,
    }
}

fn find_missing_bool(patterns: &[&Pattern]) -> Vec<String> {
    let mut has_true = false;
    let mut has_false = false;

    for pattern in patterns {
        match pattern {
            Pattern::Lit(Literal::Bool(true)) => has_true = true,
            Pattern::Lit(Literal::Bool(false)) => has_false = true,
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
            Pattern::Lit(Literal::Unit) => return vec![],
            Pattern::Tuple { elements } if elements.is_empty() => return vec![],
            Pattern::Or { left, right }
                if find_missing_unit(&[&left.node]).is_empty() || find_missing_unit(&[&right.node]).is_empty() =>
            {
                return vec![];
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
            Pattern::List { elements } if elements.len() == 1 && matches!(&elements[0].node, Pattern::Cons { .. }) => {
                has_cons = true;
            }
            Pattern::Cons { .. } => has_cons = true,
            Pattern::Or { left, right } => {
                let left_missing = find_missing_list(&[&left.node]);
                let right_missing = find_missing_list(&[&right.node]);
                if left_missing.iter().all(|m| m != "[]") || right_missing.iter().all(|m| m != "[]") {
                    has_empty = true;
                }
                if left_missing.iter().all(|m| m != "[_ | _]") || right_missing.iter().all(|m| m != "[_ | _]") {
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
        missing.push("[_ | _]".to_string());
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
    use ivy_syntax::lit::Literal;
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

    fn warnings(code: &str) -> Vec<TypeWarning> {
        let program = ivy_parse::parse(code).expect("parse failed");
        unreachable_clause_warnings(&program.declarations, &TypeRegistry::with_builtins())
    }

    #[test]
    fn test_warning_points_at_covering_clause() {
        let code = "fn f('x') => 1;\nfn f(_: Char) => 0;\nfn f('y') => 2;";
        let found = warnings(code);
        assert_eq!(found.len(), 1);
        let TypeWarning::UnreachableClause { span, covered_by, .. } = &found[0];
        assert_eq!(&code[span.start..span.end], "fn f('y') => 2;");
        let covered_by = covered_by.expect("a single clause covers it");
        assert_eq!(&code[covered_by.start..covered_by.end], "fn f(_: Char) => 0;");
    }

    #[test]
    fn test_wildcard_before_literal_is_unreachable() {
        assert_eq!(warnings("fn f(_: Char) => 0;\nfn f('x') => 1;").len(), 1);
    }

    #[test]
    fn test_literal_before_wildcard_is_reachable() {
        assert!(warnings("fn f('x') => 1;\nfn f(_: Char) => 0;").is_empty());
    }

    #[test]
    fn test_catch_all_before_specific_second_arg() {
        assert_eq!(
            warnings("fn step(s: Int, _: Char) => s;\nfn step(s: Int, ']') => s;").len(),
            1
        );
    }

    #[test]
    fn test_interleaved_definitions_grouped_by_name() {
        let code = "fn f(_: Char) => 0;\nfn g(n: Int) => n;\nfn f('x') => 1;";
        assert_eq!(warnings(code).len(), 1);
    }

    #[test]
    fn test_bool_true_false_shadow_wildcard() {
        let code = "fn f(true) => 1;\nfn f(false) => 2;\nfn f(_) => 3;";
        assert_eq!(warnings(code).len(), 1);
    }

    #[test]
    fn test_option_ctors_shadow_wildcard() {
        let code = "fn f(None) => 0;\nfn f(Some(_)) => 1;\nfn f(_) => 2;";
        assert_eq!(warnings(code).len(), 1);
    }

    #[test]
    fn test_list_nil_cons_shadow_wildcard() {
        let code = "fn f([]) => 0;\nfn f([_ | _]) => 1;\nfn f(_) => 2;";
        assert_eq!(warnings(code).len(), 1);
    }

    #[test]
    fn test_incomplete_bool_wildcard_reachable() {
        let code = "fn f(true) => 1;\nfn f(_) => 2;";
        assert!(warnings(code).is_empty());
    }

    #[test]
    fn test_infinite_literals_wildcard_reachable() {
        let code = "fn f(0) => 0;\nfn f(1) => 1;\nfn f(_) => 2;";
        assert!(warnings(code).is_empty());
    }

    #[test]
    fn test_variable_before_literal_is_unreachable() {
        let code = "fn f(x) => x;\nfn f(0) => 0;";
        assert_eq!(warnings(code).len(), 1);
    }

    #[test]
    fn test_record_patterns_skip_analysis() {
        let code = "type P = { x: Int };\nfn f(P { x: 0 }) => 0;\nfn f(P { x: _ }) => 1;";
        assert!(warnings(code).is_empty());
    }

    #[test]
    fn test_or_pattern_blowup_is_skipped() {
        let small = vec!["true|false"; 3].join(", ");
        assert_eq!(warnings(&format!("fn f(({small})) => 0;\nfn f(t) => 1;")).len(), 1);

        let positions = vec!["true|false"; 22].join(", ");
        let code = format!("fn f(({positions})) => 0;\nfn f(t) => 1;");
        assert!(warnings(&code).is_empty());
    }

    #[test]
    fn test_small_or_product_still_analyzed() {
        let code = "fn f(1|2, _) => 0;\nfn f(3, _) => 1;\nfn f(2, true) => 2;";
        let found = warnings(code);
        assert_eq!(found.len(), 1);
        let TypeWarning::UnreachableClause { span, .. } = &found[0];
        assert_eq!(&code[span.start..span.end], "fn f(2, true) => 2;");
    }
}
