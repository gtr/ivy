//! The pattern matrix engine behind Maranget's usefulness algorithm.
//!
//! Based on Maranget, "Warnings for Pattern Matching" (2007)
//! http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

use ivy_syntax::ast::Ident;
use ivy_syntax::lit::Literal;
use ivy_syntax::pattern::Pattern;
use ivy_syntax::{Span, Spanned};

use crate::registry::TypeRegistry;

pub(super) const MAX_ROWS: usize = 4096;

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

fn dummy(pat: Pattern) -> Spanned<Pattern> {
    Spanned::new(pat, Span::new(0, 0))
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
    let columns: Vec<&Pattern> = elements.iter().map(|e| &e.node).collect();
    rows(&columns)
        .into_iter()
        .map(|row| {
            row.into_iter()
                .zip(elements)
                .map(|(p, e)| Spanned::new(p, e.span))
                .collect()
        })
        .collect()
}

pub(super) fn rows(columns: &[&Pattern]) -> Vec<Vec<Pattern>> {
    let mut rows = vec![vec![]];
    for column in columns {
        let alts = expand(column);
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

pub(super) fn row_count(columns: &[&Pattern]) -> usize {
    columns.iter().map(|p| expansion_size(p)).fold(1, usize::saturating_mul)
}

pub(super) fn contains_record(pattern: &Pattern) -> bool {
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

pub(super) fn useful(matrix: &[Vec<Pattern>], row: &[Pattern], registry: &TypeRegistry) -> bool {
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

fn head_to_pattern(head: &Head, args: Vec<Pattern>) -> Pattern {
    match head {
        Head::Variant(name) => Pattern::Constructor {
            name: Ident::new(name.clone(), Span::new(0, 0)),
            args: args.into_iter().map(dummy).collect(),
        },
        Head::Bool(b) => Pattern::Lit(Literal::Bool(*b)),
        Head::Unit => Pattern::Lit(Literal::Unit),
        Head::Tuple(_) => Pattern::Tuple {
            elements: args.into_iter().map(dummy).collect(),
        },
        Head::Nil => Pattern::List { elements: vec![] },
        Head::Cons => {
            let mut it = args.into_iter();
            let head_p = it.next().unwrap_or(Pattern::Wildcard);
            let tail_p = it.next().unwrap_or(Pattern::Wildcard);
            Pattern::Cons {
                head: Box::new(dummy(head_p)),
                tail: Box::new(dummy(tail_p)),
            }
        }
        Head::Int(n) => Pattern::Lit(Literal::Int(*n)),
        Head::Char(c) => Pattern::Lit(Literal::Char(*c)),
        Head::Str(s) => Pattern::Lit(Literal::String(s.clone())),
        Head::Float(bits) => Pattern::Lit(Literal::Float(f64::from_bits(*bits))),
    }
}

fn missing_heads(heads: &[Head], registry: &TypeRegistry) -> Vec<Pattern> {
    match heads.first() {
        Some(Head::Bool(_)) => vec![Pattern::Lit(Literal::Bool(!heads.contains(&Head::Bool(true))))],
        Some(Head::Nil | Head::Cons) => {
            if !heads.contains(&Head::Nil) {
                vec![Pattern::List { elements: vec![] }]
            } else {
                vec![Pattern::Cons {
                    head: Box::new(dummy(Pattern::Wildcard)),
                    tail: Box::new(dummy(Pattern::Wildcard)),
                }]
            }
        }
        Some(Head::Variant(name)) => {
            let ctors = registry
                .get_constructor_info(name)
                .and_then(|i| registry.get_constructors(&i.type_name));
            match ctors {
                Some(ctors) => ctors
                    .iter()
                    .filter(|c| !heads.contains(&Head::Variant((*c).clone())))
                    .map(|ctor| {
                        let arity = registry.get_constructor_info(ctor).map_or(0, |i| i.arity);
                        Pattern::Constructor {
                            name: Ident::new(ctor.clone(), Span::new(0, 0)),
                            args: (0..arity).map(|_| dummy(Pattern::Wildcard)).collect(),
                        }
                    })
                    .collect(),
                None => vec![Pattern::Wildcard],
            }
        }
        _ => vec![Pattern::Wildcard],
    }
}

/// Maranget's `I`: up to `limit` patterns of length `width` matched by no row of `matrix`
///
/// Empty if the matrix is exhaustive.
pub(super) fn missing_patterns(
    matrix: &[Vec<Pattern>],
    width: usize,
    limit: usize,
    registry: &TypeRegistry,
) -> Vec<Vec<Pattern>> {
    if width == 0 {
        return if matrix.is_empty() { vec![vec![]] } else { vec![] };
    }
    let heads = column_heads(matrix);
    let mut found = Vec::new();
    match complete_signature(&heads, registry) {
        Some(full) => {
            for (head, arity) in &full {
                let specialized = specialize(matrix, head, *arity);
                for sub in missing_patterns(&specialized, arity + width - 1, limit - found.len(), registry) {
                    let (ctor_args, rest) = sub.split_at(*arity);
                    let mut witness = vec![head_to_pattern(head, ctor_args.to_vec())];
                    witness.extend_from_slice(rest);
                    found.push(witness);
                }
                if found.len() == limit {
                    break;
                }
            }
        }
        None => {
            let rests = missing_patterns(&default_matrix(matrix), width - 1, limit, registry);
            for head in missing_heads(&heads, registry) {
                for rest in &rests {
                    if found.len() == limit {
                        return found;
                    }
                    let mut witness = vec![head.clone()];
                    witness.extend_from_slice(rest);
                    found.push(witness);
                }
            }
        }
    }
    found
}
