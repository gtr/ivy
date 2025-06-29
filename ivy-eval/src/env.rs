use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::error::{EvalError, EvalResult};
use crate::value::Value;
use ivy_syntax::Span;

#[derive(Debug, Clone)]
struct Binding {
    value: Value,
    is_mut: bool,
}

#[derive(Debug, Clone, Default)]
struct Scope {
    bindings: HashMap<String, Binding>,
}

#[derive(Debug, Clone)]
pub struct Env {
    scopes: Rc<RefCell<Vec<Scope>>>,
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl Env {
    pub fn new() -> Self {
        Env {
            scopes: Rc::new(RefCell::new(vec![Scope::default()])),
        }
    }

    pub fn push_scope(&self) {
        self.scopes.borrow_mut().push(Scope::default());
    }

    pub fn pop_scope(&self) {
        let mut scopes = self.scopes.borrow_mut();
        if scopes.len() > 1 {
            scopes.pop();
        }
    }

    pub fn define(&self, name: &str, value: Value, is_mut: bool) {
        let mut scopes = self.scopes.borrow_mut();
        let current = scopes.last_mut().expect("no scope");
        current.bindings.insert(name.to_string(), Binding { value, is_mut });
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        let scopes = self.scopes.borrow();
        for scope in scopes.iter().rev() {
            if let Some(binding) = scope.bindings.get(name) {
                return Some(binding.value.clone());
            }
        }
        None
    }

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

    pub fn fork(&self) -> Self {
        Env {
            scopes: Rc::new(RefCell::new(self.scopes.borrow().clone())),
        }
    }

    pub fn list_bindings(&self) -> Vec<String> {
        let scopes = self.scopes.borrow();
        let mut names: Vec<String> = scopes.iter().flat_map(|scope| scope.bindings.keys().cloned()).collect();
        names.sort();
        names.dedup();
        names
    }
}
