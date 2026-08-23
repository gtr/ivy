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
    global: Rc<RefCell<Scope>>,
    locals: Rc<RefCell<Vec<Scope>>>,
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl Env {
    pub fn new() -> Self {
        Env {
            global: Rc::new(RefCell::new(Scope::default())),
            locals: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn push_scope(&self) {
        self.locals.borrow_mut().push(Scope::default());
    }

    pub fn pop_scope(&self) {
        self.locals.borrow_mut().pop();
    }

    pub fn define(&self, name: &str, value: Value, is_mut: bool) {
        let mut locals = self.locals.borrow_mut();
        let binding = Binding { value, is_mut };
        match locals.last_mut() {
            Some(scope) => {
                scope.bindings.insert(name.to_string(), binding);
            }
            None => {
                self.global.borrow_mut().bindings.insert(name.to_string(), binding);
            }
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        for scope in self.locals.borrow().iter().rev() {
            if let Some(binding) = scope.bindings.get(name) {
                return Some(binding.value.clone());
            }
        }
        self.global.borrow().bindings.get(name).map(|b| b.value.clone())
    }

    pub fn assign(&self, name: &str, value: Value, span: Span) -> EvalResult<()> {
        {
            let mut locals = self.locals.borrow_mut();
            for scope in locals.iter_mut().rev() {
                if let Some(binding) = scope.bindings.get_mut(name) {
                    return assign_binding(binding, value, name, span);
                }
            }
        }
        let mut global = self.global.borrow_mut();
        match global.bindings.get_mut(name) {
            Some(binding) => assign_binding(binding, value, name, span),
            None => Err(EvalError::UndefinedVariable {
                name: name.to_string(),
                span,
            }),
        }
    }

    /// Fork the environment for a call: share the global scope, copy only the local stack
    pub fn fork(&self) -> Self {
        Env {
            global: self.global.clone(),
            locals: Rc::new(RefCell::new(self.locals.borrow().clone())),
        }
    }

    /// List all binding names across the global and local scopes
    pub fn list_bindings(&self) -> Vec<String> {
        let mut names: Vec<String> = self.global.borrow().bindings.keys().cloned().collect();
        for scope in self.locals.borrow().iter() {
            names.extend(scope.bindings.keys().cloned());
        }
        names.sort();
        names.dedup();
        names
    }

    /// List all bindings (name, value) across the global and local scopes
    pub fn all_bindings(&self) -> Vec<(String, Value)> {
        let mut bindings: HashMap<String, Value> = HashMap::new();
        for (name, binding) in &self.global.borrow().bindings {
            bindings.insert(name.clone(), binding.value.clone());
        }
        for scope in self.locals.borrow().iter() {
            for (name, binding) in &scope.bindings {
                bindings.insert(name.clone(), binding.value.clone());
            }
        }
        bindings.into_iter().collect()
    }
}

fn assign_binding(binding: &mut Binding, value: Value, name: &str, span: Span) -> EvalResult<()> {
    if !binding.is_mut {
        return Err(EvalError::ImmutableAssignment {
            name: name.to_string(),
            span,
        });
    }
    binding.value = value;
    Ok(())
}
