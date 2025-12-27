//! Pattern matching for Ivy.
//! TODO(gtr): we haven't implemented some exhaustiveness checking yet

use ivy_syntax::{Literal, Pattern};

use crate::value::{ListValue, Value};

/// Result of pattern matching: bindings extracted, or None if no match.
pub type MatchResult = Option<Vec<(String, Value)>>;

/// Match a pattern against a value, extracting bindings.
pub fn match_pattern(pattern: &Pattern, value: &Value) -> MatchResult {
    match pattern {
        Pattern::Wildcard => Some(vec![]),

        Pattern::Lit(lit) => {
            if literal_matches(lit, value) {
                Some(vec![])
            } else {
                None
            }
        }

        Pattern::Var(ident) => Some(vec![(ident.name.clone(), value.clone())]),

        Pattern::Constructor { name, args } => match value {
            Value::Constructor { variant, fields, .. } => {
                if variant != &name.name {
                    return None;
                }
                if args.len() != fields.len() {
                    return None;
                }
                let mut bindings = vec![];
                for (pat, val) in args.iter().zip(fields.iter()) {
                    match match_pattern(&pat.node, val) {
                        Some(b) => bindings.extend(b),
                        None => return None,
                    }
                }
                Some(bindings)
            }
            _ => None,
        },

        Pattern::Tuple { elements } => match value {
            Value::Tuple(values) => {
                if elements.len() != values.len() {
                    return None;
                }
                let mut bindings = vec![];
                for (pat, val) in elements.iter().zip(values.iter()) {
                    match match_pattern(&pat.node, val) {
                        Some(b) => bindings.extend(b),
                        None => return None,
                    }
                }
                Some(bindings)
            }
            _ => None,
        },

        Pattern::List { elements } => {
            // Special case: [head :: tail] is syntactic sugar for a cons pattern
            if elements.len() == 1 {
                if let Pattern::Cons { head, tail } = &elements[0].node {
                    return match value {
                        Value::List(list) => match list.as_ref() {
                            ListValue::Nil => None,
                            ListValue::Cons(h, t) => {
                                let mut bindings = vec![];
                                match match_pattern(&head.node, h) {
                                    Some(b) => bindings.extend(b),
                                    None => return None,
                                }
                                let tail_val = Value::List(t.clone());
                                match match_pattern(&tail.node, &tail_val) {
                                    Some(b) => bindings.extend(b),
                                    None => return None,
                                }
                                Some(bindings)
                            }
                        },
                        _ => None,
                    };
                }
            }

            let values = list_to_vec(value)?;
            if elements.len() != values.len() {
                return None;
            }
            let mut bindings = vec![];
            for (pat, val) in elements.iter().zip(values.iter()) {
                match match_pattern(&pat.node, val) {
                    Some(b) => bindings.extend(b),
                    None => return None,
                }
            }
            Some(bindings)
        }

        Pattern::Cons { head, tail } => match value {
            Value::List(list) => match list.as_ref() {
                ListValue::Nil => None,
                ListValue::Cons(h, t) => {
                    let mut bindings = vec![];
                    match match_pattern(&head.node, h) {
                        Some(b) => bindings.extend(b),
                        None => return None,
                    }
                    let tail_val = Value::List(t.clone());
                    match match_pattern(&tail.node, &tail_val) {
                        Some(b) => bindings.extend(b),
                        None => return None,
                    }
                    Some(bindings)
                }
            },
            _ => None,
        },

        Pattern::Record { name, fields } => match value {
            Value::Record {
                type_name,
                fields: val_fields,
            } => {
                if type_name != &name.name {
                    return None;
                }
                let mut bindings = vec![];
                for field_pat in fields {
                    let field_val = val_fields.get(&field_pat.name.name)?;
                    match &field_pat.pattern {
                        Some(pat) => match match_pattern(&pat.node, field_val) {
                            Some(b) => bindings.extend(b),
                            None => return None,
                        },
                        None => {
                            bindings.push((field_pat.name.name.clone(), field_val.clone()));
                        }
                    }
                }
                Some(bindings)
            }
            _ => None,
        },

        Pattern::Or { left, right } => match_pattern(&left.node, value).or_else(|| match_pattern(&right.node, value)),
    }
}

/// Check if a literal matches a value.
fn literal_matches(lit: &Literal, value: &Value) -> bool {
    match (lit, value) {
        (Literal::Int(a), Value::Int(b)) => a == b,
        (Literal::Float(a), Value::Float(b)) => (a - b).abs() < f64::EPSILON,
        (Literal::String(a), Value::String(b)) => a == b,
        (Literal::Char(a), Value::Char(b)) => a == b,
        (Literal::Bool(a), Value::Bool(b)) => a == b,
        (Literal::Unit, Value::Unit) => true,
        _ => false,
    }
}

/// Convert a list value to a Vec for pattern matching.
/// TODO(gtr): better implementation?
fn list_to_vec(value: &Value) -> Option<Vec<Value>> {
    match value {
        Value::List(list) => Some(list.to_vec()),
        _ => None,
    }
}
