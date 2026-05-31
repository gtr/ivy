//! Operator evaluation and value comparison utilities.

use std::cmp::Ordering;
use std::rc::Rc;

use ivy_syntax::{BinOp, Expr, Literal, Span, Spanned, UnaryOp};

use crate::error::{EvalError, EvalResult};
use crate::eval::Interpreter;
use crate::pattern::match_pattern;
use crate::value::{ListValue, Value};

impl Interpreter {
    /// Evaluate binary operation.
    pub(crate) fn eval_binary(
        &mut self,
        left: &Spanned<Expr>,
        op: BinOp,
        right: &Spanned<Expr>,
        span: Span,
    ) -> EvalResult<Value> {
        // Short-circuit for and/or
        match op {
            BinOp::And => {
                let l = self.eval_expr(left)?;
                match l {
                    Value::Bool(false) => return Ok(Value::Bool(false)),
                    Value::Bool(true) => {
                        let r = self.eval_expr(right)?;
                        match r {
                            Value::Bool(b) => return Ok(Value::Bool(b)),
                            _ => {
                                return Err(EvalError::TypeError {
                                    expected: "Bool".to_string(),
                                    found: r.type_name(),
                                    span: right.span,
                                })
                            }
                        }
                    }
                    _ => {
                        return Err(EvalError::TypeError {
                            expected: "Bool".to_string(),
                            found: l.type_name(),
                            span: left.span,
                        })
                    }
                }
            }
            BinOp::Or => {
                let l = self.eval_expr(left)?;
                match l {
                    Value::Bool(true) => return Ok(Value::Bool(true)),
                    Value::Bool(false) => {
                        let r = self.eval_expr(right)?;
                        match r {
                            Value::Bool(b) => return Ok(Value::Bool(b)),
                            _ => {
                                return Err(EvalError::TypeError {
                                    expected: "Bool".to_string(),
                                    found: r.type_name(),
                                    span: right.span,
                                })
                            }
                        }
                    }
                    _ => {
                        return Err(EvalError::TypeError {
                            expected: "Bool".to_string(),
                            found: l.type_name(),
                            span: left.span,
                        })
                    }
                }
            }
            _ => {}
        }

        let l = self.eval_expr(left)?;
        let r = self.eval_expr(right)?;

        match op {
            BinOp::Add => match (&l, &r) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                _ => Err(EvalError::TypeError {
                    expected: "Int or Float".to_string(),
                    found: l.type_name(),
                    span,
                }),
            },
            BinOp::Sub => match (&l, &r) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                _ => Err(EvalError::TypeError {
                    expected: "Int or Float".to_string(),
                    found: l.type_name(),
                    span,
                }),
            },
            BinOp::Mul => match (&l, &r) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                _ => Err(EvalError::TypeError {
                    expected: "Int or Float".to_string(),
                    found: l.type_name(),
                    span,
                }),
            },
            BinOp::Div => match (&l, &r) {
                (Value::Int(a), Value::Int(b)) => {
                    if *b == 0 {
                        Err(EvalError::DivisionByZero { span })
                    } else {
                        Ok(Value::Int(a / b))
                    }
                }
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
                _ => Err(EvalError::TypeError {
                    expected: "Int or Float".to_string(),
                    found: l.type_name(),
                    span,
                }),
            },
            BinOp::Mod => match (&l, &r) {
                (Value::Int(a), Value::Int(b)) => {
                    if *b == 0 {
                        Err(EvalError::DivisionByZero { span })
                    } else {
                        Ok(Value::Int(a % b))
                    }
                }
                _ => Err(EvalError::TypeError {
                    expected: "Int".to_string(),
                    found: l.type_name(),
                    span,
                }),
            },
            BinOp::Eq => Ok(Value::Bool(values_equal(&l, &r))),
            BinOp::Ne => Ok(Value::Bool(!values_equal(&l, &r))),
            BinOp::Lt => compare_values(&l, &r, |ord| ord == Ordering::Less, span),
            BinOp::Le => compare_values(&l, &r, |ord| ord != Ordering::Greater, span),
            BinOp::Gt => compare_values(&l, &r, |ord| ord == Ordering::Greater, span),
            BinOp::Ge => compare_values(&l, &r, |ord| ord != Ordering::Less, span),
            BinOp::Cons => match r {
                Value::List(list) => Ok(Value::List(Rc::new(ListValue::Cons(l, list)))),
                _ => Err(EvalError::TypeError {
                    expected: "List".to_string(),
                    found: r.type_name(),
                    span,
                }),
            },
            BinOp::Concat => match (&l, &r) {
                (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
                (Value::List(a), Value::List(b)) => Ok(concat_lists(a, b)),
                _ => Err(EvalError::TypeError {
                    expected: "String or List".to_string(),
                    found: l.type_name(),
                    span,
                }),
            },
            BinOp::And | BinOp::Or => unreachable!(),
        }
    }

    /// Evaluate unary operation.
    pub(crate) fn eval_unary(&mut self, op: UnaryOp, operand: &Spanned<Expr>, span: Span) -> EvalResult<Value> {
        let val = self.eval_expr(operand)?;
        match op {
            UnaryOp::Neg => match val {
                Value::Int(n) => Ok(Value::Int(-n)),
                Value::Float(n) => Ok(Value::Float(-n)),
                _ => Err(EvalError::TypeError {
                    expected: "Int or Float".to_string(),
                    found: val.type_name(),
                    span,
                }),
            },
            UnaryOp::Not => match val {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                _ => Err(EvalError::TypeError {
                    expected: "Bool".to_string(),
                    found: val.type_name(),
                    span,
                }),
            },
        }
    }

    /// Evaluate assignment.
    pub(crate) fn eval_assign(&mut self, target: &Spanned<Expr>, value: &Spanned<Expr>) -> EvalResult<Value> {
        let val = self.eval_expr(value)?;
        match &target.node {
            Expr::Var(ident) => {
                self.env.assign(&ident.name, val.clone(), target.span)?;
                Ok(val)
            }
            _ => Err(EvalError::TypeError {
                expected: "variable".to_string(),
                found: "expression".to_string(),
                span: target.span,
            }),
        }
    }

    /// Bind a pattern to a value.
    pub(crate) fn bind_pattern(
        &mut self,
        pattern: &ivy_syntax::Pattern,
        value: &Value,
        is_mut: bool,
        span: Span,
    ) -> EvalResult<()> {
        match match_pattern(pattern, value) {
            Some(bindings) => {
                for (name, val) in bindings {
                    self.env.define(&name, val, is_mut);
                }
                Ok(())
            }
            None => Err(EvalError::MatchFailed { span }),
        }
    }

    /// Access a field on a value.
    pub(crate) fn access_field(&self, obj: &Value, field: &str, span: Span) -> EvalResult<Value> {
        match obj {
            Value::Record { type_name, fields } => fields.get(field).cloned().ok_or_else(|| EvalError::UnknownField {
                type_name: type_name.clone(),
                field: field.to_string(),
                span,
            }),
            Value::Tuple(elements) => {
                if let Ok(idx) = field.parse::<usize>() {
                    elements.get(idx).cloned().ok_or(EvalError::IndexOutOfBounds {
                        index: idx as i64,
                        length: elements.len(),
                        span,
                    })
                } else {
                    Err(EvalError::UnknownField {
                        type_name: "Tuple".to_string(),
                        field: field.to_string(),
                        span,
                    })
                }
            }
            Value::Module { name } => {
                if let Some(module_exports) = self.modules.get(name) {
                    module_exports
                        .get(field)
                        .cloned()
                        .ok_or_else(|| EvalError::PrivateItem {
                            name: field.to_string(),
                            module: name.clone(),
                            span,
                        })
                } else {
                    Err(EvalError::UndefinedModule {
                        name: name.clone(),
                        span,
                    })
                }
            }
            _ => Err(EvalError::TypeError {
                expected: "record, tuple, or module".to_string(),
                found: obj.type_name(),
                span,
            }),
        }
    }

    /// Access an index on a value.
    pub(crate) fn access_index(&self, obj: &Value, index: &Value, span: Span) -> EvalResult<Value> {
        match (obj, index) {
            (Value::List(list), Value::Int(idx)) => {
                let vec = list.to_vec();
                let i = if *idx < 0 {
                    (vec.len() as i64 + idx) as usize
                } else {
                    *idx as usize
                };
                vec.get(i).cloned().ok_or(EvalError::IndexOutOfBounds {
                    index: *idx,
                    length: vec.len(),
                    span,
                })
            }
            (Value::Tuple(elements), Value::Int(idx)) => {
                let i = *idx as usize;
                elements.get(i).cloned().ok_or(EvalError::IndexOutOfBounds {
                    index: *idx,
                    length: elements.len(),
                    span,
                })
            }
            (Value::String(s), Value::Int(idx)) => {
                let chars: Vec<char> = s.chars().collect();
                let i = if *idx < 0 {
                    (chars.len() as i64 + idx) as usize
                } else {
                    *idx as usize
                };
                chars
                    .get(i)
                    .map(|c| Value::Char(*c))
                    .ok_or(EvalError::IndexOutOfBounds {
                        index: *idx,
                        length: chars.len(),
                        span,
                    })
            }
            _ => Err(EvalError::TypeError {
                expected: "indexable type with Int index".to_string(),
                found: format!("{} with {}", obj.type_name(), index.type_name()),
                span,
            }),
        }
    }
}

/// Convert a literal to a value.
pub(crate) fn literal_to_value(lit: &Literal) -> Value {
    match lit {
        Literal::Int(n) => Value::Int(*n),
        Literal::Float(n) => Value::Float(*n),
        Literal::String(s) => Value::String(s.clone()),
        Literal::Char(c) => Value::Char(*c),
        Literal::Bool(b) => Value::Bool(*b),
        Literal::Unit => Value::Unit,
    }
}

/// Check if two values are equal.
fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Unit, Value::Unit) => true,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => (x - y).abs() < f64::EPSILON,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Tuple(xs), Value::Tuple(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| values_equal(x, y))
        }
        (Value::List(xs), Value::List(ys)) => lists_equal(xs, ys),
        (
            Value::Constructor {
                variant: v1,
                fields: f1,
                ..
            },
            Value::Constructor {
                variant: v2,
                fields: f2,
                ..
            },
        ) => v1 == v2 && f1.len() == f2.len() && f1.iter().zip(f2.iter()).all(|(x, y)| values_equal(x, y)),
        _ => false,
    }
}

/// Check if two lists are equal.
fn lists_equal(a: &ListValue, b: &ListValue) -> bool {
    match (a, b) {
        (ListValue::Nil, ListValue::Nil) => true,
        (ListValue::Cons(h1, t1), ListValue::Cons(h2, t2)) => values_equal(h1, h2) && lists_equal(t1, t2),
        _ => false,
    }
}

/// Compare two values.
fn compare_values<F>(a: &Value, b: &Value, cmp: F, span: Span) -> EvalResult<Value>
where
    F: Fn(Ordering) -> bool,
{
    let ord = match (a, b) {
        (Value::Int(x), Value::Int(y)) => x.cmp(y),
        (Value::Float(x), Value::Float(y)) => x.partial_cmp(y).unwrap_or(Ordering::Equal),
        (Value::String(x), Value::String(y)) => x.cmp(y),
        (Value::Char(x), Value::Char(y)) => x.cmp(y),
        _ => {
            return Err(EvalError::TypeError {
                expected: "comparable types".to_string(),
                found: format!("{} and {}", a.type_name(), b.type_name()),
                span,
            })
        }
    };
    Ok(Value::Bool(cmp(ord)))
}

/// Concatenate two lists.
pub(crate) fn concat_lists(a: &ListValue, b: &Rc<ListValue>) -> Value {
    match a {
        ListValue::Nil => Value::List(b.clone()),
        ListValue::Cons(head, tail) => {
            let Value::List(new_tail) = concat_lists(tail, b) else {
                unreachable!()
            };
            Value::List(Rc::new(ListValue::Cons(head.clone(), new_tail)))
        }
    }
}
