use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use ivy_syntax::{Expr, FnBody, Param, Spanned};

use crate::env::Env;
use crate::error::EvalResult;

/// Runtime values in Ivy.
#[derive(Clone)]
pub enum Value {
    /// Unit value: ()
    Unit,

    /// Boolean: true, false
    Bool(bool),

    /// Integer: 42, -17
    Int(i64),

    /// Float: 3.14
    Float(f64),

    /// String: "hello"
    String(String),

    /// Character: 'a'
    Char(char),

    /// Tuple: (a, b, c)
    Tuple(Vec<Value>),

    /// List: [1, 2, 3]
    List(Rc<ListValue>),

    /// Record instance: Person { name: "Alice", age: 30 }
    Record {
        type_name: String,
        fields: HashMap<String, Value>,
    },

    /// Sum type constructor: Some(42), None
    Constructor {
        type_name: String,
        variant: String,
        fields: Vec<Value>,
    },

    /// Closure (user-defined function or lambda)
    Closure(Rc<Closure>),

    /// Multi-clause function (multiple pattern-matching clauses)
    MultiClause(Rc<MultiClauseFn>),

    /// Builtin function
    Builtin(BuiltinFn),

    /// Module namespace (for qualified access like Math.add)
    Module { name: String },

    /// Partial application: a function with some arguments already applied
    PartialApp { func: Box<Value>, applied_args: Vec<Value> },
}

/// List structure for efficient cons/pattern matching.
#[derive(Debug, Clone)]
pub enum ListValue {
    Nil,
    Cons(Value, Rc<ListValue>),
}

/// A closure captures its defining environment.
pub struct Closure {
    /// Parameters with patterns
    pub params: Vec<Param>,
    /// Function body
    pub body: Spanned<Expr>,
    /// Captured environment
    pub env: Env,
    /// Optional name (for recursion)
    pub name: Option<String>,
}

/// Multi-clause function: multiple clauses tried in order.
#[derive(Clone)]
pub struct MultiClauseFn {
    pub name: String,
    pub clauses: Vec<FnClause>,
    pub env: Env,
}

/// A single clause of a multi-clause function.
#[derive(Clone)]
pub struct FnClause {
    pub params: Vec<Param>,
    pub body: FnBody,
}

/// Builtin function representation.
#[derive(Clone)]
pub struct BuiltinFn {
    pub name: &'static str,
    pub arity: usize,
    pub func: fn(&[Value]) -> EvalResult<Value>,
}

impl Value {
    pub fn type_name(&self) -> String {
        match self {
            Value::Unit => "()".to_string(),
            Value::Bool(_) => "Bool".to_string(),
            Value::Int(_) => "Int".to_string(),
            Value::Float(_) => "Float".to_string(),
            Value::String(_) => "String".to_string(),
            Value::Char(_) => "Char".to_string(),
            Value::Tuple(elems) => format!("({})", vec!["_"; elems.len()].join(", ")),
            Value::List(_) => "[_]".to_string(),
            Value::Record { type_name, .. } => type_name.clone(),
            Value::Constructor { type_name, .. } => type_name.clone(),
            Value::Closure(_) => "<function>".to_string(),
            Value::MultiClause(mc) => format!("<function {}>", mc.name),
            Value::Builtin(b) => format!("<builtin {}>", b.name),
            Value::Module { name } => format!("<module {}>", name),
            Value::PartialApp { .. } => "<partial>".to_string(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Char(c) => write!(f, "{}", c),
            Value::Tuple(elements) => {
                write!(f, "(")?;
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")
            }
            Value::List(list) => {
                write!(f, "[")?;
                list.fmt_contents(f)?;
                write!(f, "]")
            }
            Value::Record { type_name, fields } => {
                write!(f, "{} {{ ", type_name)?;
                let mut first = true;
                for (name, val) in fields {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{}: {}", name, val)?;
                }
                write!(f, " }}")
            }
            Value::Constructor { variant, fields, .. } => {
                write!(f, "{}", variant)?;
                if !fields.is_empty() {
                    write!(f, "(")?;
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", field)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Value::Closure(_) => write!(f, "<function>"),
            Value::MultiClause(mc) => write!(f, "<function {}>", mc.name),
            Value::Builtin(b) => write!(f, "<builtin {}>", b.name),
            Value::Module { name } => write!(f, "<module {}>", name),
            Value::PartialApp { applied_args, .. } => {
                write!(f, "<partial ({} args applied)>", applied_args.len())
            }
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Char(c) => write!(f, "'{}'", c),
            _ => write!(f, "{}", self),
        }
    }
}

impl ListValue {
    pub fn fmt_contents(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ListValue::Nil => Ok(()),
            ListValue::Cons(head, tail) => {
                write!(f, "{}", head)?;
                match tail.as_ref() {
                    ListValue::Nil => Ok(()),
                    ListValue::Cons(_, _) => {
                        write!(f, ", ")?;
                        tail.fmt_contents(f)
                    }
                }
            }
        }
    }

    pub fn to_vec(&self) -> Vec<Value> {
        let mut result = Vec::new();
        let mut current = self;
        loop {
            match current {
                ListValue::Nil => break,
                ListValue::Cons(head, tail) => {
                    result.push(head.clone());
                    current = tail.as_ref();
                }
            }
        }
        result
    }
}

pub fn vec_to_list(values: Vec<Value>) -> Value {
    let mut list = Rc::new(ListValue::Nil);
    for val in values.into_iter().rev() {
        list = Rc::new(ListValue::Cons(val, list));
    }
    Value::List(list)
}

impl fmt::Debug for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure>")
    }
}

impl fmt::Debug for MultiClauseFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<multi-clause fn {}>", self.name)
    }
}

impl fmt::Debug for BuiltinFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<builtin {}>", self.name)
    }
}
