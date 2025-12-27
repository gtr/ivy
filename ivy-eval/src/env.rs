//! Environment for variable bindings.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::error::{EvalError, EvalResult};
use crate::value::Value;
use ivy_syntax::Span;

/// A binding in the environment.
#[derive(Debug, Clone)]
struct Binding {
    value: Value,
    is_mut: bool,
}

/// A scope containing variable bindings.
#[derive(Debug, Clone, Default)]
struct Scope {
    bindings: HashMap<String, Binding>,
}

/// Environment with lexical scoping.
#[derive(Debug, Clone)]
pub struct Env {
    /// Stack of scopes (innermost last).
    scopes: Rc<RefCell<Vec<Scope>>>,
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl Env {
    /// Create a new empty environment with one scope.
    pub fn new() -> Self {
        Env {
            scopes: Rc::new(RefCell::new(vec![Scope::default()])),
        }
    }

    /// Push a new scope.
    pub fn push_scope(&self) {
        self.scopes.borrow_mut().push(Scope::default());
    }

    /// Pop the innermost scope.
    pub fn pop_scope(&self) {
        let mut scopes = self.scopes.borrow_mut();
        if scopes.len() > 1 {
            scopes.pop();
        }
    }

    /// Define a new binding in the current scope.
    pub fn define(&self, name: &str, value: Value, is_mut: bool) {
        let mut scopes = self.scopes.borrow_mut();
        let current = scopes.last_mut().expect("no scope");
        current.bindings.insert(name.to_string(), Binding { value, is_mut });
    }

    /// Look up a binding by name
    pub fn get(&self, name: &str) -> Option<Value> {
        let scopes = self.scopes.borrow();
        for scope in scopes.iter().rev() {
            if let Some(binding) = scope.bindings.get(name) {
                return Some(binding.value.clone());
            }
        }
        None
    }

    /// Assign to a mutable binding (error if not found or immutable)
    pub fn assign(&self, name: &str, value: Value, span: Span) -> EvalResult<()> {
        let mut scopes = self.scopes.borrow_mut();
        for scope in scopes.iter_mut().rev() {
            if let Some(binding) = scope.bindings.get_mut(name) {
                if !binding.is_mut {
                    return Err(EvalError::ImmutableAssignment {
                        name: name.to_string(),
                        span,
                    });
                }
                binding.value = value;
                return Ok(());
            }
        }
        Err(EvalError::UndefinedVariable {
            name: name.to_string(),
            span,
        })
    }

    /// Fork the environment for a closure.
    /// Creates a new Env that shares nothing with the current one
    /// (deep clone of _all_ scopes).
    pub fn fork(&self) -> Self {
        Env {
            scopes: Rc::new(RefCell::new(self.scopes.borrow().clone())),
        }
    }

    /// List all binding names across all scopes.
    pub fn list_bindings(&self) -> Vec<String> {
        let scopes = self.scopes.borrow();
        let mut names: Vec<String> = scopes.iter().flat_map(|scope| scope.bindings.keys().cloned()).collect();
        names.sort();
        names.dedup();
        names
    }
}
